# ============================================================ #
# Tool:         plot_schematic
# Description:  Renders a top-to-bottom schematic with L-bend edges,
#               reach-count badges, and colour-coded node boxes; optionally
#               annotates gauge boxes with model names and appends a rain
#               gauge panel.
# Flode Module: reach.network
# Author:       Jon Payne
# Created:      2026-04-01
# Modified:     2026-04-01 - JP: initial creation
# Tier:         3
# Inputs:       simplified igraph; layout_df; optional model_meta and
#               rain_gauges data frames
# Outputs:      ggplot2 object
# Dependencies: ggplot2, igraph (ggplot2 acceptable for Tier 2-3)
# ============================================================ #

#' Schematic diagram of a simplified river network
#'
#' Renders a top-to-bottom flow diagram with L-bend edges and colour-coded
#' node boxes. Each edge carries a badge showing how many original reaches
#' were collapsed into it.
#'
#' If `model_meta` is supplied, gauge labels gain a third line listing the
#' models served by that gauge (e.g. `"G001\nMain Gauge\nHEC-RAS \u00b7 ISIS"`).
#' Rain gauges (those not anchored to the river network) are shown in a
#' separate column to the right with a distinct colour, connected to their
#' model annotations via dashed lines.
#'
#' @param g_simple Simplified directed `igraph` (`$graph` from
#'   [simplify_network()]).
#' @param layout_df Layout data frame as returned by [hierarchical_layout()].
#' @param model_meta Optional data frame with columns `gauge_id` and
#'   `model_name`. Default `NULL`.
#' @param rain_gauges Optional sf or data frame of rain gauges (rows with
#'   `gauge_type == "rain"`). When supplied, rain gauges are plotted as
#'   a separate panel column. Default `NULL`.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' result   <- simplify_network(g, gauge_node_map(gauges_snapped, network))
#' layout   <- hierarchical_layout(result$graph, result$node_meta)
#' p_schema <- plot_schematic(result$graph, layout)
#' }
#'
#' @export
plot_schematic <- function(g_simple, layout_df, model_meta = NULL,
                            rain_gauges = NULL) {
  # Inject model names into gauge labels
  layout_df <- .inject_model_labels(layout_df, model_meta)

  n_nodes   <- nrow(layout_df)
  max_depth <- max(abs(layout_df$y))

  # Adaptive box sizing
  box_hw <- min(0.08, 0.6 / max(table(layout_df$y)))
  box_hh <- min(0.15, 0.8 / (max_depth + 1))

  edge_geom <- edge_geometry(g_simple, layout_df)

  # Node colours (flow/stage gauges share the same palette key)
  node_fill   <- .type_fills[match_type_key(layout_df$type)]
  node_colour <- .type_colours[match_type_key(layout_df$type)]

  p <- ggplot2::ggplot() +
    # Edge segment 1: vertical drop from source node
    ggplot2::geom_segment(
      data = edge_geom,
      ggplot2::aes(x = x, y = y - box_hh, xend = x, yend = ymid),
      colour = "steelblue", linewidth = 0.4
    ) +
    # Edge segment 2: horizontal jog
    ggplot2::geom_segment(
      data = edge_geom,
      ggplot2::aes(x = x, y = ymid, xend = xend, yend = ymid),
      colour = "steelblue", linewidth = 0.4
    ) +
    # Edge segment 3: vertical drop into target (with arrow)
    ggplot2::geom_segment(
      data = edge_geom,
      ggplot2::aes(x = xend, y = ymid, xend = xend, yend = yend + box_hh),
      colour = "steelblue", linewidth = 0.4,
      arrow = ggplot2::arrow(length = ggplot2::unit(1.5, "mm"), type = "closed")
    ) +
    # Reach-count badge on each edge
    ggplot2::geom_label(
      data = edge_geom,
      ggplot2::aes(
        x     = ifelse(x == xend, x + 0.012, (x + xend) / 2),
        y     = ymid,
        label = n_reaches
      ),
      size = 1.8, label.size = 0.2,
      label.r = ggplot2::unit(1.5, "pt"),
      fill = "white", colour = "grey50",
      label.padding = ggplot2::unit(1.5, "pt")
    ) +
    # Node boxes
    ggplot2::geom_rect(
      data = layout_df,
      ggplot2::aes(
        xmin = x - box_hw, xmax = x + box_hw,
        ymin = y - box_hh, ymax = y + box_hh
      ),
      fill   = node_fill,
      colour = node_colour,
      linewidth = 0.5
    ) +
    # Node labels
    ggplot2::geom_text(
      data = layout_df,
      ggplot2::aes(x = x, y = y, label = label),
      size = 1.8, fontface = "bold", lineheight = 0.8,
      colour = "grey20"
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title    = "Simplified schematic",
      subtitle = paste0(
        n_nodes, " key nodes from ",
        igraph::ecount(g_simple) + sum(igraph::E(g_simple)$n_reaches - 1L),
        " original reaches  |  edge labels = collapsed reach count"
      )
    ) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(colour = "grey50", size = 8),
      plot.margin   = ggplot2::margin(10, 15, 10, 15)
    )

  # Append rain gauge panel if provided
  if (!is.null(rain_gauges) && nrow(rain_gauges) > 0) {
    p <- .add_rain_panel(p, rain_gauges, model_meta, box_hh)
  }

  p
}


# Internal: inject model names into layout_df labels ----------------------

#' @keywords internal
.inject_model_labels <- function(layout_df, model_meta) {
  if (is.null(model_meta) || nrow(model_meta) == 0) {
    return(layout_df)
  }

  model_summary <- tapply(
    model_meta$model_name,
    model_meta$gauge_id,
    \(x) paste(unique(x), collapse = " \u00b7 ")
  )

  # Gauge labels in layout_df start with "gauge_id\ngauge_name"
  # We need to match on gauge_id which is embedded in the label first line
  for (i in seq_len(nrow(layout_df))) {
    if (layout_df$type[i] != "gauge") next

    # Extract the gauge_id from the label (first token before \n)
    gid <- strsplit(layout_df$label[i], "\n")[[1]][1]
    if (!is.null(model_summary[[gid]])) {
      layout_df$label[i] <- paste0(layout_df$label[i], "\n",
                                    model_summary[[gid]])
    }
  }

  layout_df
}


# Internal: add rain gauge panel to right of schematic -------------------

#' @keywords internal
.add_rain_panel <- function(p, rain_gauges, model_meta, box_hh) {
  n_rain  <- nrow(rain_gauges)
  # Place rain gauges in a column at x = 1.15, evenly spaced vertically
  rain_y  <- seq(0, -(n_rain - 1) * 0.3, length.out = n_rain)

  rain_df <- data.frame(
    x     = 1.15,
    y     = rain_y,
    label = rain_gauges$gauge_name,
    id    = rain_gauges$gauge_id,
    stringsAsFactors = FALSE
  )

  if (!is.null(model_meta)) {
    model_summary <- tapply(
      model_meta$model_name,
      model_meta$gauge_id,
      \(x) paste(unique(x), collapse = " \u00b7 ")
    )
    rain_df$label <- vapply(seq_len(nrow(rain_df)), \(i) {
      gid <- rain_df$id[i]
      if (!is.null(model_summary[[gid]])) {
        paste0(rain_df$label[i], "\n", model_summary[[gid]])
      } else {
        rain_df$label[i]
      }
    }, character(1))
  }

  box_hw <- 0.07

  p +
    # Rain gauge boxes
    ggplot2::geom_rect(
      data = rain_df,
      ggplot2::aes(
        xmin = x - box_hw, xmax = x + box_hw,
        ymin = y - box_hh, ymax = y + box_hh
      ),
      fill = .type_fills[["gauge_rain"]],
      colour = .type_colours[["gauge_rain"]],
      linewidth = 0.5
    ) +
    ggplot2::geom_text(
      data = rain_df,
      ggplot2::aes(x = x, y = y, label = label),
      size = 1.8, fontface = "bold", lineheight = 0.8,
      colour = "grey20"
    ) +
    # "Rain gauges" column header
    ggplot2::annotate(
      "text", x = 1.15, y = 0.15,
      label = "Rain gauges", size = 2, colour = .type_colours[["gauge_rain"]],
      fontface = "bold"
    )
}


# Internal: map node type to palette key ----------------------------------

#' @keywords internal
match_type_key <- function(types) {
  result <- rep("other", length(types))
  result[types == "gauge"]      <- "gauge_flow"
  result[types == "confluence"] <- "confluence"
  result[types == "headwater"]  <- "headwater"
  result[types == "outlet"]     <- "outlet"
  result
}
