# ============================================================ #
# Tool:         build_ngd_graph / simplify_network
# Description:  Constructs a directed igraph from OS NGD startnode/endnode
#               topology, then contracts intermediate reaches retaining only
#               confluences, gauges, headwaters, and the outlet.
# Flode Module: reach.network
# Author:       Jon Payne
# Created:      2026-04-01
# Modified:     2026-04-01 - JP: initial creation
# Tier:         3
# Inputs:       sf LINESTRING network (osid, startnode, endnode);
#               named character vector of gauge-to-node mappings
# Outputs:      directed igraph (full and simplified); node_meta data frame
# Dependencies: igraph (non-fastverse; no fastverse equivalent for graph ops)
# ============================================================ #

#' Build a directed graph from OS NGD Water Network
#'
#' Constructs an `igraph` directed graph using the `startnode`/`endnode`
#' columns from the OS NGD Water Link layer. This is more reliable than
#' reconstructing topology from endpoint coordinates (which requires
#' floating-point matching and fails on large networks with tiny gaps).
#'
#' @param network An sf LINESTRING object as returned by [load_ngd_network()].
#'   Must contain `osid`, `startnode`, and `endnode`.
#'
#' @return A directed `igraph` object. Each edge represents one watercourse
#'   link; edge attributes include `reach_id` (= `osid`) and `length_m`
#'   (if `geometry_length_m` is present in `network`).
#'
#' @examples
#' \dontrun{
#' network <- load_ngd_network("os_ngd_water.gpkg")
#' g <- build_ngd_graph(network)
#' }
#'
#' @export
build_ngd_graph <- function(network) {
  required <- c("osid", "startnode", "endnode")
  missing_cols <- setdiff(required, names(network))
  if (length(missing_cols) > 0) {
    stop("'network' is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  edge_df <- data.frame(
    from     = as.character(network$startnode),
    to       = as.character(network$endnode),
    reach_id = as.character(network$osid),
    stringsAsFactors = FALSE
  )

  if ("geometry_length_m" %in% names(network)) {
    edge_df$length_m <- as.numeric(network$geometry_length_m)
  }

  g <- igraph::graph_from_data_frame(edge_df, directed = TRUE)

  message("Full network: ", igraph::vcount(g), " nodes, ",
          igraph::ecount(g), " edges")
  g
}


#' Simplify a river network graph
#'
#' Contracts intermediate (degree-2) nodes, retaining only:
#' - **Confluences** — nodes with in-degree > 1 (more than one upstream reach)
#' - **Gauges** — nodes corresponding to snapped gauge positions
#' - **Headwaters** — nodes with in-degree 0
#' - **Outlet** — node(s) with out-degree 0
#'
#' Each edge in the simplified graph carries an `n_reaches` attribute
#' recording how many original reaches were collapsed into it.
#'
#' @param graph A directed `igraph` as returned by [build_ngd_graph()].
#' @param gauge_nodes Named character vector mapping gauge display labels to
#'   node IDs in `graph`. As returned by [gauge_node_map()]. Use
#'   `character(0)` or `c()` if there are no gauges.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{`graph`}{Simplified directed `igraph`}
#'     \item{`node_meta`}{Data frame with columns `name`, `type`, `label`
#'       for use in layout and plotting}
#'   }
#'
#' @examples
#' \dontrun{
#' g        <- build_ngd_graph(network)
#' g_map    <- gauge_node_map(gauges_snapped, network)
#' result   <- simplify_network(g, g_map)
#' g_simple <- result$graph
#' }
#'
#' @export
simplify_network <- function(graph, gauge_nodes = character(0)) {
  node_deg    <- igraph::degree(graph, mode = "all")
  confluences <- names(node_deg[node_deg > 2])
  headwaters  <- names(which(igraph::degree(graph, mode = "in")  == 0))
  outlets     <- names(which(igraph::degree(graph, mode = "out") == 0))

  # Warn if any gauge lands on a confluence node (rare with startnode placement)
  gauge_on_conf <- gauge_nodes[gauge_nodes %in% confluences]
  if (length(gauge_on_conf) > 0) {
    warning(
      "Gauge(s) share a node with a confluence: ",
      paste(names(gauge_on_conf), collapse = ", "),
      "\n  Both will appear but the gauge sits at the confluence level."
    )
  }

  keep <- unique(c(confluences, unname(gauge_nodes), headwaters, outlets))
  keep <- keep[!is.na(keep)]

  message(
    "Keeping ", length(keep), " nodes: ",
    length(confluences), " confluences, ",
    sum(!is.na(gauge_nodes)), " gauge(s), ",
    length(headwaters), " headwater(s), ",
    length(outlets), " outlet(s)"
  )

  # Contract: for each kept node, walk downstream until the next kept node
  simplified_edges <- list()

  for (node in keep) {
    out_nbs <- igraph::neighbors(graph, node, mode = "out")
    if (length(out_nbs) == 0) next

    for (i in seq_along(out_nbs)) {
      current   <- igraph::V(graph)$name[out_nbs[i]]
      n_reaches <- 1L

      while (!(current %in% keep)) {
        out_next <- igraph::neighbors(graph, current, mode = "out")
        if (length(out_next) == 0) {
          keep    <- c(keep, current)
          break
        }
        current   <- igraph::V(graph)$name[out_next[1]]
        n_reaches <- n_reaches + 1L
      }

      simplified_edges <- c(simplified_edges, list(data.frame(
        from      = node,
        to        = current,
        n_reaches = n_reaches,
        stringsAsFactors = FALSE
      )))
    }
  }

  simp_df  <- do.call(rbind, simplified_edges)
  g_simple <- igraph::graph_from_data_frame(simp_df, directed = TRUE)

  # Build a reverse lookup: node_id -> gauge label
  gauge_lookup <- if (length(gauge_nodes) > 0) {
    setNames(names(gauge_nodes), gauge_nodes)
  } else {
    character(0)
  }

  # Node type and label
  node_names <- igraph::V(g_simple)$name

  node_type <- vapply(node_names, \(nm) {
    if (nm %in% unname(gauge_nodes)) return("gauge")
    if (nm %in% confluences)         return("confluence")
    if (nm %in% headwaters)          return("headwater")
    if (nm %in% outlets)             return("outlet")
    "other"
  }, character(1))

  node_label <- vapply(node_names, \(nm) {
    if (nm %in% names(gauge_lookup)) return(gauge_lookup[[nm]])
    type <- node_type[match(nm, node_names)]
    switch(type,
      confluence = "Confluence",
      headwater  = "Headwater",
      outlet     = "Outlet",
      nm
    )
  }, character(1))

  node_meta <- data.frame(
    name  = node_names,
    type  = node_type,
    label = node_label,
    stringsAsFactors = FALSE
  )

  message("Simplified: ", igraph::vcount(g_simple), " nodes, ",
          igraph::ecount(g_simple), " edges")

  list(graph = g_simple, node_meta = node_meta)
}
