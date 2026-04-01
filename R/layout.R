# ============================================================ #
# Tool:         hierarchical_layout / edge_geometry
# Description:  Places simplified network nodes top-to-bottom by topological
#               depth with subtree-width horizontal spacing; computes L-bend
#               edge segments for schematic plotting.
# Flode Module: reach.network
# Author:       Jon Payne
# Created:      2026-04-01
# Modified:     2026-04-01 - JP: initial creation
# Tier:         3
# Inputs:       simplified igraph; node_meta data frame (name, type, label)
# Outputs:      layout_df (name, x, y, type, label); edge_geom data frame
# Dependencies: igraph (non-fastverse; topo_sort and graph traversal)
# ============================================================ #

#' Compute a hierarchical layout for a simplified river network
#'
#' Places nodes top-to-bottom by topological depth (longest path from any
#' headwater) and distributes them horizontally in proportion to their
#' subtree widths. A post-layout spacing pass enforces a minimum gap between
#' nodes on the same row, and the result is rescaled to \[0.05, 0.95\].
#'
#' This is the same algorithm used in the large synthetic example, extracted
#' into a reusable function.
#'
#' @param g_simple Simplified directed `igraph` as returned by the `$graph`
#'   element of [simplify_network()].
#' @param node_meta Data frame with columns `name`, `type`, `label` as
#'   returned by the `$node_meta` element of [simplify_network()].
#' @param min_gap Numeric. Minimum centre-to-centre horizontal distance
#'   between nodes on the same depth level. Tune upward if boxes overlap on
#'   wide, bushy networks. Default `0.14`.
#'
#' @return A data frame with one row per node and columns:
#'   \describe{
#'     \item{`name`}{Node ID (matches `igraph` vertex names)}
#'     \item{`x`}{Horizontal position in \[0.05, 0.95\]}
#'     \item{`y`}{Vertical position (negative depth, so outlet is at 0 and
#'       headwaters are most negative)}
#'     \item{`type`}{Node type from `node_meta`}
#'     \item{`label`}{Display label from `node_meta`}
#'   }
#'
#' @export
hierarchical_layout <- function(g_simple, node_meta, min_gap = 0.14) {
  node_names  <- igraph::V(g_simple)$name
  topo_order  <- igraph::topo_sort(g_simple, mode = "out")

  # Depth: longest path from any headwater to each node
  depth <- setNames(rep(0L, length(node_names)), node_names)
  for (v in topo_order) {
    vname   <- igraph::V(g_simple)$name[v]
    out_nbs <- igraph::neighbors(g_simple, v, mode = "out")
    for (j in seq_along(out_nbs)) {
      nb_name        <- igraph::V(g_simple)$name[out_nbs[j]]
      depth[nb_name] <- max(depth[nb_name], depth[vname] + 1L)
    }
  }

  # Subtree width: number of leaves reachable from each node
  subtree_width <- setNames(rep(1L, length(node_names)), node_names)
  for (v in rev(topo_order)) {
    vname   <- igraph::V(g_simple)$name[v]
    in_nbs  <- igraph::neighbors(g_simple, v, mode = "in")
    if (length(in_nbs) > 0) {
      subtree_width[vname] <- sum(vapply(
        seq_along(in_nbs),
        \(j) subtree_width[igraph::V(g_simple)$name[in_nbs[j]]],
        integer(1)
      ))
    }
  }

  # Recursive x-position assignment from outlet upward
  x_pos <- setNames(rep(NA_real_, length(node_names)), node_names)

  assign_x <- function(node, x_left, x_right) {
    x_pos[node] <<- (x_left + x_right) / 2
    in_nbs <- igraph::neighbors(g_simple, node, mode = "in")
    if (length(in_nbs) == 0) return(invisible(NULL))

    in_names <- vapply(
      seq_along(in_nbs),
      \(j) igraph::V(g_simple)$name[in_nbs[j]],
      character(1)
    )
    widths  <- subtree_width[in_names]
    total_w <- sum(widths)

    cursor <- x_left
    for (k in seq_along(in_names)) {
      frac        <- widths[k] / total_w
      child_right <- cursor + frac * (x_right - x_left)
      assign_x(in_names[k], cursor, child_right)
      cursor <- child_right
    }
  }

  outlet_node <- node_names[node_meta$type[match(node_names, node_meta$name)] == "outlet"][1]
  assign_x(outlet_node, 0, 1)

  layout_df <- data.frame(
    name  = node_names,
    x     = x_pos[node_names],
    y     = -depth[node_names],
    stringsAsFactors = FALSE
  )

  # Merge type and label from node_meta
  layout_df <- merge(layout_df, node_meta[, c("name", "type", "label")],
                     by = "name", all.x = TRUE)

  # Post-layout spacing: enforce min_gap between nodes on the same depth row
  for (row_y in unique(layout_df$y)) {
    idx <- which(layout_df$y == row_y)
    if (length(idx) <= 1) next

    idx <- idx[order(layout_df$x[idx])]
    for (j in 2:length(idx)) {
      gap <- layout_df$x[idx[j]] - layout_df$x[idx[j - 1]]
      if (gap < min_gap) {
        shift <- (min_gap - gap) / 2
        layout_df$x[idx[j]]     <- layout_df$x[idx[j]]     + shift
        layout_df$x[idx[j - 1]] <- layout_df$x[idx[j - 1]] - shift
      }
    }
  }

  # Rescale x to [0.05, 0.95]
  x_range <- range(layout_df$x, na.rm = TRUE)
  if (diff(x_range) > 0) {
    layout_df$x <- 0.05 + 0.9 * (layout_df$x - x_range[1]) / diff(x_range)
  }

  layout_df
}


#' Compute edge geometry for schematic plotting
#'
#' Given a simplified graph and its layout, returns a data frame of L-bend
#' edge segments suitable for `geom_segment()`.
#'
#' @param g_simple Simplified directed igraph.
#' @param layout_df Layout data frame as returned by [hierarchical_layout()].
#'
#' @return Data frame with columns `x`, `y`, `xend`, `yend`, `ymid`,
#'   `n_reaches`.
#'
#' @export
edge_geometry <- function(g_simple, layout_df) {
  do.call(rbind, lapply(seq_len(igraph::ecount(g_simple)), \(i) {
    e  <- igraph::ends(g_simple, i)
    fr <- layout_df[layout_df$name == e[1], ]
    to <- layout_df[layout_df$name == e[2], ]
    data.frame(
      x         = fr$x,
      y         = fr$y,
      xend      = to$x,
      yend      = to$y,
      ymid      = (fr$y + to$y) / 2,
      n_reaches = igraph::E(g_simple)$n_reaches[i],
      stringsAsFactors = FALSE
    )
  }))
}
