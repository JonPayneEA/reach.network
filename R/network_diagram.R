# ============================================================ #
# Tool:         network_diagram / model_meta_template
# Description:  Top-level wrapper that runs the full pipeline from OS NGD
#               GeoPackage + gauge spatial file to saved static PNGs and
#               interactive HTML widget.
# Flode Module: reach.network
# Author:       Jon Payne
# Created:      2026-04-01
# Modified:     2026-04-01 - JP: initial creation
# Tier:         3
# Inputs:       OS NGD GeoPackage path; gauge GeoPackage/Shapefile path;
#               optional long-format model metadata CSV (gauge_id, model_name)
# Outputs:      river_network_spatial.png, river_network_schematic.png,
#               river_network_combined.png, river_network_interactive.html
# Dependencies: sf, igraph, ggplot2, patchwork, visNetwork, data.table
#               (data.table used for CSV I/O per fastverse governance;
#                all others non-fastverse, justified above in module headers)
# ============================================================ #

#' Build and save river network diagrams
#'
#' Top-level wrapper that runs the full pipeline:
#'
#' 1. Load OS NGD Water Network from GeoPackage
#' 2. Load gauge points from GeoPackage/Shapefile
#' 3. (Optionally) load model metadata from CSV
#' 4. Snap flow/stage gauges to the network
#' 5. Build directed graph using `startnode`/`endnode`
#' 6. Simplify the graph (keep confluences, gauges, headwaters, outlet)
#' 7. Compute hierarchical layout
#' 8. Produce and save: spatial PNG, schematic PNG, combined PNG, and
#'    interactive HTML
#'
#' Returns all intermediate objects invisibly so you can inspect or replot
#' any stage without re-running the full pipeline.
#'
#' @param network_path Character. Path to the OS NGD GeoPackage.
#' @param gauge_path Character. Path to the gauge GeoPackage or Shapefile.
#' @param model_meta_path Character or `NULL`. Path to a long-format CSV with
#'   columns `gauge_id` and `model_name`. See [model_meta_template()] for the
#'   expected structure. Default `NULL` (no model annotations).
#' @param output_dir Character. Directory to write output files.
#'   Created if it does not exist. Default `"."`.
#' @param tolerance_m Numeric. Snapping tolerance in metres. Default `50`.
#' @param snap_types Character vector. Gauge types to snap. Default
#'   `c("flow", "stage")`.
#' @param network_layer Character. GeoPackage layer name for the network.
#'   Default `"wtr_ntwk_waterlink"`.
#' @param gauge_type_col Character. Column in the gauge file that holds the
#'   gauge type string. Default `"gauge_type"`.
#' @param label_col Character. Column in the gauge file used for node labels.
#'   Default `"gauge_name"`.
#' @param save_png Logical. Save static PNG outputs. Default `TRUE`.
#' @param save_html Logical. Save interactive HTML widget. Default `TRUE`.
#' @param ... Additional arguments passed to [load_ngd_network()].
#'
#' @return Invisibly, a named list:
#'   \describe{
#'     \item{`network`}{Loaded sf network}
#'     \item{`gauges`}{Snapped gauges sf}
#'     \item{`model_meta`}{Model metadata data frame (or `NULL`)}
#'     \item{`g_full`}{Full directed igraph}
#'     \item{`g_simple`}{Simplified igraph}
#'     \item{`node_meta`}{Node metadata data frame}
#'     \item{`layout_df`}{Layout data frame}
#'     \item{`p_spatial`}{ggplot2 spatial plot}
#'     \item{`p_schematic`}{ggplot2 schematic plot}
#'     \item{`p_combined`}{patchwork combined plot}
#'     \item{`widget`}{visNetwork widget}
#'   }
#'
#' @examples
#' \dontrun{
#' # Minimal call — no model metadata
#' result <- network_diagram(
#'   network_path = "os_ngd_water.gpkg",
#'   gauge_path   = "gauges.gpkg",
#'   output_dir   = "outputs"
#' )
#'
#' # With model metadata
#' result <- network_diagram(
#'   network_path    = "os_ngd_water.gpkg",
#'   gauge_path      = "gauges.gpkg",
#'   model_meta_path = "model_metadata.csv",
#'   output_dir      = "outputs",
#'   tolerance_m     = 100
#' )
#'
#' # Re-plot the schematic without re-running the pipeline
#' print(result$p_schematic)
#' }
#'
#' @export
network_diagram <- function(network_path,
                             gauge_path,
                             model_meta_path  = NULL,
                             output_dir       = ".",
                             tolerance_m      = 50,
                             snap_types       = c("flow", "stage"),
                             network_layer    = "wtr_ntwk_waterlink",
                             gauge_type_col   = "gauge_type",
                             label_col        = "gauge_name",
                             save_png         = TRUE,
                             save_html        = TRUE,
                             ...) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # -- 1. Load ---------------------------------------------------------------
  message("--- Loading network ---")
  network <- load_ngd_network(network_path, layer = network_layer, ...)

  message("--- Loading gauges ---")
  gauges <- sf::st_read(gauge_path, quiet = TRUE)

  # Rename gauge_type_col to "gauge_type" if necessary
  if (gauge_type_col != "gauge_type" && gauge_type_col %in% names(gauges)) {
    names(gauges)[names(gauges) == gauge_type_col] <- "gauge_type"
  }

  # Load model metadata
  model_meta <- NULL
  if (!is.null(model_meta_path)) {
    message("--- Loading model metadata ---")
    model_meta <- as.data.frame(
      data.table::fread(model_meta_path, strip.white = TRUE)
    )
    required_cols <- c("gauge_id", "model_name")
    missing <- setdiff(required_cols, names(model_meta))
    if (length(missing) > 0) {
      stop("model_meta CSV is missing columns: ", paste(missing, collapse = ", "),
           "\nExpected: gauge_id, model_name  (see model_meta_template())")
    }
    message("  Loaded ", nrow(model_meta), " gauge-model pair(s) for ",
            length(unique(model_meta$gauge_id)), " gauge(s)")
  }

  # -- 2. Snap ---------------------------------------------------------------
  message("--- Snapping gauges ---")
  gauges <- snap_gauges(gauges, network,
                         tolerance_m = tolerance_m,
                         snap_types  = snap_types)

  gauge_map <- gauge_node_map(gauges, network, label_col = label_col)

  # Separate rain gauges for optional schematic panel
  rain_gauges <- NULL
  if ("gauge_type" %in% names(gauges)) {
    rain_rows <- !gauges$gauge_type %in% snap_types
    if (any(rain_rows)) {
      rain_gauges <- gauges[rain_rows, ]
    }
  }

  # -- 3. Build graph --------------------------------------------------------
  message("--- Building graph ---")
  g_full <- build_ngd_graph(network)

  # -- 4. Simplify -----------------------------------------------------------
  message("--- Simplifying network ---")
  simplified <- simplify_network(g_full, gauge_map)
  g_simple   <- simplified$graph
  node_meta  <- simplified$node_meta

  # -- 5. Layout -------------------------------------------------------------
  message("--- Computing layout ---")
  layout_df <- hierarchical_layout(g_simple, node_meta)

  # -- 6. Plots --------------------------------------------------------------
  message("--- Plotting ---")
  p_spatial   <- plot_spatial(network, gauges, model_meta)
  p_schematic <- plot_schematic(g_simple, layout_df, model_meta, rain_gauges)

  p_combined <- patchwork::wrap_plots(p_spatial, p_schematic,
                                       widths = c(1, 1.5)) +
    patchwork::plot_annotation(
      title    = "River network simplification",
      subtitle = paste0(
        igraph::vcount(g_full), " nodes / ",
        igraph::ecount(g_full), " reaches  \u2192  ",
        igraph::vcount(g_simple), " key nodes"
      ),
      theme = ggplot2::theme(
        plot.title    = ggplot2::element_text(face = "bold", size = 13),
        plot.subtitle = ggplot2::element_text(colour = "grey50", size = 9)
      )
    )

  # -- 7. Save ---------------------------------------------------------------
  if (save_png) {
    spatial_path   <- file.path(output_dir, "river_network_spatial.png")
    schematic_path <- file.path(output_dir, "river_network_schematic.png")
    combined_path  <- file.path(output_dir, "river_network_combined.png")

    ggplot2::ggsave(spatial_path,   p_spatial,   width = 10, height = 8,
                    dpi = 200, bg = "white")
    ggplot2::ggsave(schematic_path, p_schematic, width = 12, height = 10,
                    dpi = 200, bg = "white")
    ggplot2::ggsave(combined_path,  p_combined,  width = 18, height = 10,
                    dpi = 200, bg = "white")
    message("Saved PNGs to: ", output_dir)
  }

  widget <- NULL
  if (save_html) {
    widget       <- plot_interactive(g_simple, layout_df, model_meta, rain_gauges)
    html_path    <- file.path(output_dir, "river_network_interactive.html")
    visNetwork::visSave(widget, html_path, selfcontained = TRUE)
    message("Saved interactive HTML to: ", html_path)
  }

  message("--- Done ---")

  invisible(list(
    network     = network,
    gauges      = gauges,
    model_meta  = model_meta,
    g_full      = g_full,
    g_simple    = g_simple,
    node_meta   = node_meta,
    layout_df   = layout_df,
    p_spatial   = p_spatial,
    p_schematic = p_schematic,
    p_combined  = p_combined,
    widget      = widget
  ))
}


#' Model metadata CSV template
#'
#' Returns an empty data frame showing the expected structure of the
#' `model_meta` CSV accepted by [network_diagram()] and [plot_schematic()].
#'
#' Save it as a starting point:
#' ```r
#' write.csv(model_meta_template(), "model_metadata.csv", row.names = FALSE)
#' ```
#'
#' @param example Logical. If `TRUE` (default), includes two example rows.
#'
#' @return A data frame with columns `gauge_id` and `model_name`.
#'
#' @export
model_meta_template <- function(example = TRUE) {
  if (example) {
    data.frame(
      gauge_id   = c("G001", "G001", "G002", "P001"),
      model_name = c("HEC-RAS", "HECFM", "ISIS", "HEC-RAS"),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      gauge_id   = character(0),
      model_name = character(0),
      stringsAsFactors = FALSE
    )
  }
}
