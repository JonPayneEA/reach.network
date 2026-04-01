# ============================================================ #
# Tool:         plot_spatial
# Description:  Renders a spatial map of OS NGD river network lines with
#               snapped gauges overlaid; distinguishes snapped, unsnapped,
#               and rain gauge types by shape and colour.
# Flode Module: reach.network
# Author:       Jon Payne
# Created:      2026-04-01
# Modified:     2026-04-01 - JP: initial creation
# Tier:         3
# Inputs:       sf LINESTRING network; sf POINT gauges (post-snap);
#               optional model_meta data frame (gauge_id, model_name)
# Outputs:      ggplot2 object
# Dependencies: sf, ggplot2 (ggplot2 acceptable for Tier 2-3 per governance)
# ============================================================ #

#' Palette constants
#'
#' Shared colour definitions used by all plot functions.
#' @keywords internal
.type_colours <- c(
  gauge_flow    = "#D85A30",
  gauge_stage   = "#D85A30",
  gauge_rain    = "#4A90D9",
  gauge_unsnap  = "#888780",
  confluence    = "#5F5E5A",
  headwater     = "#639922",
  outlet        = "#1D9E75",
  other         = "#888780"
)

#' @keywords internal
.type_fills <- c(
  gauge_flow    = "#FAECE7",
  gauge_stage   = "#FAECE7",
  gauge_rain    = "#E8F1FB",
  gauge_unsnap  = "#F1EFE8",
  confluence    = "#F1EFE8",
  headwater     = "#EAF3DE",
  outlet        = "#E1F5EE",
  other         = "#F1EFE8"
)


#' Spatial map of the river network and gauge locations
#'
#' Renders the raw river network lines with gauge points overlaid. Snapped
#' flow/stage gauges are shown as filled orange circles; unsnapped gauges and
#' rain gauges are shown with distinct styling so their status is immediately
#' clear.
#'
#' @param network An sf LINESTRING object as returned by [load_ngd_network()].
#' @param gauges An sf POINT object as returned by [snap_gauges()]. Must have
#'   columns `gauge_id`, `gauge_name`, `gauge_type`, `snapped`.
#' @param model_meta Optional data frame with columns `gauge_id` and
#'   `model_name` (long format, one row per gauge-model pair). When supplied,
#'   model names are appended to gauge labels. Default `NULL`.
#' @param label_offset Numeric. Horizontal nudge for gauge labels in map units
#'   (metres for BNG). Default `200`.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' p <- plot_spatial(network, gauges_snapped)
#' print(p)
#' }
#'
#' @export
plot_spatial <- function(network, gauges, model_meta = NULL,
                          label_offset = 200) {
  # Build display labels (optionally with model names)
  gauges <- .add_model_labels(gauges, model_meta)

  # Classify gauges for colouring
  gauges$plot_class <- chr_case(
    !gauges$snapped & gauges$gauge_type %in% c("flow", "stage"), "gauge_unsnap",
    gauges$gauge_type == "rain",                                   "gauge_rain",
    TRUE,                                                          "gauge_flow"
  )

  snapped_gauges   <- gauges[gauges$snapped, ]
  unsnapped_gauges <- gauges[!gauges$snapped & gauges$gauge_type %in% c("flow", "stage"), ]
  rain_gauges      <- gauges[gauges$gauge_type == "rain", ]

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = network, colour = "steelblue", linewidth = 0.4,
                     alpha = 0.7)

  # Snapped flow/stage gauges
  if (nrow(snapped_gauges) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = snapped_gauges,
        colour = .type_colours[["gauge_flow"]],
        fill   = .type_fills[["gauge_flow"]],
        shape = 21, size = 3, stroke = 1
      ) +
      ggplot2::geom_sf_text(
        data = snapped_gauges,
        ggplot2::aes(label = display_label),
        nudge_x = label_offset, hjust = 0, size = 2.2, colour = "grey30"
      )
  }

  # Unsnapped flow/stage gauges
  if (nrow(unsnapped_gauges) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = unsnapped_gauges,
        colour = .type_colours[["gauge_unsnap"]],
        fill   = "white",
        shape = 21, size = 3, stroke = 1, linetype = "dashed"
      ) +
      ggplot2::geom_sf_text(
        data = unsnapped_gauges,
        ggplot2::aes(label = paste0(display_label, "\n[unsnapped]")),
        nudge_x = label_offset, hjust = 0, size = 2.0, colour = "grey50"
      )
    warning(
      nrow(unsnapped_gauges),
      " gauge(s) shown as unsnapped in spatial plot."
    )
  }

  # Rain gauges
  if (nrow(rain_gauges) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = rain_gauges,
        colour = .type_colours[["gauge_rain"]],
        fill   = .type_fills[["gauge_rain"]],
        shape = 23, size = 3, stroke = 1   # diamond shape for rain
      ) +
      ggplot2::geom_sf_text(
        data = rain_gauges,
        ggplot2::aes(label = display_label),
        nudge_x = label_offset, hjust = 0, size = 2.2, colour = "grey30"
      )
  }

  p +
    ggplot2::labs(
      title    = "Spatial view",
      subtitle = paste0(
        nrow(network), " reaches  |  ",
        sum(gauges$snapped), " snapped gauge(s)",
        if (nrow(rain_gauges) > 0)
          paste0("  |  ", nrow(rain_gauges), " rain gauge(s)")
        else ""
      )
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.text     = ggplot2::element_blank(),
      axis.title    = ggplot2::element_blank(),
      panel.grid    = ggplot2::element_line(colour = "grey95"),
      plot.title    = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(colour = "grey50", size = 8)
    )
}


# Internal helper: append model names to gauge display labels --------------

#' @keywords internal
.add_model_labels <- function(gauges, model_meta) {
  gauges$display_label <- gauges$gauge_name

  if (is.null(model_meta) || nrow(model_meta) == 0) {
    return(gauges)
  }

  # Collapse model names per gauge (handles many-to-many)
  model_summary <- tapply(
    model_meta$model_name,
    model_meta$gauge_id,
    \(x) paste(unique(x), collapse = " \u00b7 ")   # middot separator
  )

  for (i in seq_len(nrow(gauges))) {
    gid <- gauges$gauge_id[i]
    if (!is.null(model_summary[[gid]])) {
      gauges$display_label[i] <- paste0(
        gauges$display_label[i], "\n", model_summary[[gid]]
      )
    }
  }

  gauges
}


# Internal helper: vectorised if-else chain (base R, no external deps) ----

#' @keywords internal
chr_case <- function(...) {
  args <- list(...)
  # args alternates: condition, value, ..., default_value (last must be TRUE)
  n    <- length(args)
  result <- rep(NA_character_, length(args[[1]]))  # length from first condition

  for (i in seq(1, n - 1, by = 2)) {
    cond <- args[[i]]
    val  <- args[[i + 1]]
    result[cond & is.na(result)] <- val
  }
  result
}
