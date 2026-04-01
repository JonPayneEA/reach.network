#' reach.network: River Network Diagram Tools
#'
#' Builds simplified schematic and spatial network diagrams from OS NGD Water
#' Network data. Key capabilities:
#'
#' - **Loading**: `load_ngd_network()` reads the OS NGD GeoPackage and
#'   normalises the CRS to EPSG:27700.
#' - **Snapping**: `snap_gauges()` snaps flow/stage gauges to the nearest
#'   river reach within a tolerance. Rain gauges are explicitly excluded from
#'   snapping as they sit anywhere in the catchment.
#' - **Graph construction**: `build_ngd_graph()` uses the `startnode`/`endnode`
#'   columns from OS NGD directly — no coordinate-matching required.
#' - **Simplification**: `simplify_network()` contracts intermediate reaches,
#'   retaining confluences, gauges, headwaters and the outlet.
#' - **Layout**: `hierarchical_layout()` places nodes top-to-bottom by
#'   topological depth with subtree-width horizontal spacing.
#' - **Plots**: `plot_spatial()`, `plot_schematic()`, `plot_interactive()`
#'   produce ggplot2 static outputs and a visNetwork HTML widget.
#' - **Wrapper**: `network_diagram()` runs the full pipeline from file paths
#'   to saved outputs.
#'
#' @keywords internal
"_PACKAGE"
