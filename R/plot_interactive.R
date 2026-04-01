# ============================================================ #
# Tool:         plot_interactive
# Description:  Builds a visNetwork HTML widget from the simplified river
#               network with zoom, pan, hover tooltips, and
#               click-to-highlight; rain gauges appended as disconnected nodes.
# Flode Module: reach.network
# Author:       Jon Payne
# Created:      2026-04-01
# Modified:     2026-04-01 - JP: initial creation
# Tier:         3
# Inputs:       simplified igraph; layout_df; optional model_meta and
#               rain_gauges data frames
# Outputs:      visNetwork HTML widget (save with visSave())
# Dependencies: visNetwork, igraph (non-fastverse; no fastverse equivalent
#               for interactive network widgets)
# ============================================================ #

#' Interactive network diagram (HTML widget)
#'
#' Builds a `visNetwork` HTML widget from the simplified river network.
#' The widget supports zooming, panning, hover tooltips, and
#' click-to-highlight nearest neighbours.
#'
#' Node colours and shapes follow the same palette as the static plots:
#' - **Gauge** (flow/stage): orange rectangle
#' - **Rain gauge**: blue diamond
#' - **Confluence**: grey rectangle
#' - **Headwater**: green rectangle
#' - **Outlet**: teal rectangle (double border)
#'
#' @param g_simple Simplified directed `igraph` (`$graph` from
#'   [simplify_network()]).
#' @param layout_df Layout data frame as returned by [hierarchical_layout()].
#' @param model_meta Optional data frame with columns `gauge_id` and
#'   `model_name`. Model names are appended to gauge tooltips and labels.
#'   Default `NULL`.
#' @param rain_gauges Optional data frame/sf of rain gauges (rows with
#'   `gauge_type == "rain"`). When supplied, rain gauges are added as
#'   disconnected nodes in the widget. Default `NULL`.
#' @param height Character. Widget height passed to `visNetwork()`.
#'   Default `"700px"`.
#'
#' @return A `visNetwork` HTML widget. Render it in RStudio Viewer, embed it
#'   in a Quarto/R Markdown document, or save it with
#'   `visNetwork::visSave(widget, "output.html")`.
#'
#' @examples
#' \dontrun{
#' result  <- simplify_network(g, gauge_node_map(gauges_snapped, network))
#' layout  <- hierarchical_layout(result$graph, result$node_meta)
#' widget  <- plot_interactive(result$graph, layout)
#' visNetwork::visSave(widget, "river_network.html")
#' }
#'
#' @export
plot_interactive <- function(g_simple, layout_df, model_meta = NULL,
                              rain_gauges = NULL, height = "700px") {
  # Build model label lookup
  model_summary <- .build_model_summary(model_meta)

  # ---- Nodes ---------------------------------------------------------------
  nodes <- data.frame(
    id    = seq_len(nrow(layout_df)),
    label = layout_df$label,
    title = .build_tooltip(layout_df, model_summary),  # HTML tooltip
    x     = layout_df$x * 1200,   # scale to pixel-ish coords
    y     = layout_df$y * -300,
    color.background = .type_fills[match_type_key(layout_df$type)],
    color.border     = .type_colours[match_type_key(layout_df$type)],
    shape     = "box",
    font.size = 11,
    font.bold = TRUE,
    stringsAsFactors = FALSE
  )

  # Double border for outlet
  outlet_idx <- which(layout_df$type == "outlet")
  if (length(outlet_idx) > 0) {
    nodes$borderWidth[outlet_idx]        <- 3
    nodes$borderWidthSelected[outlet_idx] <- 4
  }

  # Node ID lookup for edge building
  name_to_id <- setNames(nodes$id, layout_df$name)

  # ---- Edges ---------------------------------------------------------------
  edge_ends <- igraph::ends(g_simple, igraph::E(g_simple))
  edges <- data.frame(
    from   = name_to_id[edge_ends[, 1]],
    to     = name_to_id[edge_ends[, 2]],
    label  = as.character(igraph::E(g_simple)$n_reaches),
    title  = paste0(igraph::E(g_simple)$n_reaches, " reach(es)"),
    arrows = "to",
    color  = "steelblue",
    font.size = 9,
    stringsAsFactors = FALSE
  )

  # ---- Rain gauge nodes (appended, disconnected) ---------------------------
  if (!is.null(rain_gauges) && nrow(rain_gauges) > 0) {
    rain_labels <- vapply(seq_len(nrow(rain_gauges)), \(i) {
      gid <- rain_gauges$gauge_id[i]
      lbl <- rain_gauges$gauge_name[i]
      if (!is.null(model_summary[[gid]])) {
        lbl <- paste0(lbl, "\n", model_summary[[gid]])
      }
      lbl
    }, character(1))

    rain_tooltip <- vapply(seq_len(nrow(rain_gauges)), \(i) {
      gid <- rain_gauges$gauge_id[i]
      tt  <- paste0("<b>", rain_gauges$gauge_name[i], "</b><br>Type: rain")
      if (!is.null(model_summary[[gid]])) {
        tt <- paste0(tt, "<br>Models: ", model_summary[[gid]])
      }
      tt
    }, character(1))

    rain_nodes <- data.frame(
      id    = max(nodes$id) + seq_len(nrow(rain_gauges)),
      label = rain_labels,
      title = rain_tooltip,
      x     = max(nodes$x) + 250,
      y     = seq(0, (nrow(rain_gauges) - 1) * 80, by = 80),
      color.background = .type_fills[["gauge_rain"]],
      color.border     = .type_colours[["gauge_rain"]],
      shape     = "diamond",
      font.size = 11,
      font.bold = TRUE,
      stringsAsFactors = FALSE
    )

    nodes <- rbind(nodes, rain_nodes)
  }

  # ---- Build widget --------------------------------------------------------
  widget <- visNetwork::visNetwork(nodes, edges, height = height,
                                    width = "100%") |>
    visNetwork::visEdges(
      arrows   = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
      smooth   = list(enabled = TRUE, type = "cubicBezier",
                      roundness = 0.4)
    ) |>
    visNetwork::visNodes(
      borderWidth = 1.5,
      font = list(size = 11, bold = TRUE)
    ) |>
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1),
      nodesIdSelection = TRUE
    ) |>
    visNetwork::visLayout(randomSeed = 42) |>
    visNetwork::visPhysics(enabled = FALSE)   # use fixed coordinates from layout

  widget
}


# Internal helpers ----------------------------------------------------------

#' @keywords internal
.build_model_summary <- function(model_meta) {
  if (is.null(model_meta) || nrow(model_meta) == 0) {
    return(list())
  }
  as.list(tapply(
    model_meta$model_name,
    model_meta$gauge_id,
    \(x) paste(unique(x), collapse = " \u00b7 ")
  ))
}

#' @keywords internal
.build_tooltip <- function(layout_df, model_summary) {
  vapply(seq_len(nrow(layout_df)), \(i) {
    nm  <- layout_df$label[i]
    typ <- layout_df$type[i]
    tt  <- paste0("<b>", nm, "</b><br>Type: ", typ)

    # Try to extract gauge_id from label (first line)
    gid <- strsplit(nm, "\n")[[1]][1]
    if (!is.null(model_summary[[gid]])) {
      tt <- paste0(tt, "<br>Models: ", model_summary[[gid]])
    }
    tt
  }, character(1))
}
