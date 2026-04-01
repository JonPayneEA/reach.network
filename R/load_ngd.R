# ============================================================ #
# Tool:         load_ngd_network
# Description:  Reads the OS NGD wtr_ntwk_waterlink layer and returns a
#               clean sf LINESTRING object in EPSG:27700.
# Flode Module: reach.network
# Author:       Jon Payne
# Created:      2026-04-01
# Modified:     2026-04-01 - JP: initial creation
# Tier:         3
# Inputs:       OS NGD Water Network GeoPackage (.gpkg)
# Outputs:      sf LINESTRING with osid, startnode, endnode columns
# Dependencies: sf (non-fastverse; required for spatial I/O — no fastverse
#               equivalent for GeoPackage reading)
# ============================================================ #

#' Load OS NGD Water Network
#'
#' Reads the `wtr_ntwk_waterlink` layer from an OS NGD GeoPackage and
#' returns a clean sf LINESTRING object in EPSG:27700 (British National Grid).
#'
#' The OS NGD Water Link layer may be stored in 3D (EPSG:7405, BNG + ODN
#' height). Z coordinates are dropped automatically so that subsequent spatial
#' operations work correctly in 2D.
#'
#' @param path Character. Path to the OS NGD GeoPackage (`.gpkg`).
#' @param layer Character. Layer name to read. Defaults to
#'   `"wtr_ntwk_waterlink"`.
#' @param crs Integer. Target EPSG code. Defaults to `27700` (BNG).
#' @param ... Additional arguments passed to [sf::st_read()], e.g.
#'   `query = "SELECT * FROM wtr_ntwk_waterlink WHERE permanence = 'Permanent'"`.
#'
#' @return An sf object with LINESTRING geometry and at minimum the columns
#'   `osid`, `startnode`, `endnode`. The column `geometry_length_m` is retained
#'   if present in the source layer.
#'
#' @details
#' **Required source columns**
#'
#' | Column | Description |
#' |--------|-------------|
#' | `osid` | Unique OS identifier (GUID) for each reach |
#' | `startnode` | OS node ID at the upstream end of the reach |
#' | `endnode` | OS node ID at the downstream end of the reach |
#'
#' If any of these columns are absent the function stops with an informative
#' error so you can verify the layer name or data version.
#'
#' @examples
#' \dontrun{
#' network <- load_ngd_network("path/to/os_ngd_water.gpkg")
#' }
#'
#' @export
load_ngd_network <- function(path, layer = "wtr_ntwk_waterlink", crs = 27700,
                              ...) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  net <- sf::st_read(path, layer = layer, quiet = TRUE, ...)

  # Drop Z if present (EPSG:7405 = BNG + ODN height)
  if (!is.na(sf::st_crs(net)) && sf::st_crs(net)$epsg == 7405) {
    net <- sf::st_zm(net, drop = TRUE, what = "ZM")
  }

  # Transform to target CRS if needed
  if (!is.na(sf::st_crs(net)) && sf::st_crs(net)$epsg != crs) {
    net <- sf::st_transform(net, crs)
  }

  # Validate required columns
  required <- c("osid", "startnode", "endnode")
  missing_cols <- setdiff(required, names(net))
  if (length(missing_cols) > 0) {
    stop(
      "Required column(s) not found in layer '", layer, "': ",
      paste(missing_cols, collapse = ", "),
      "\nAvailable columns: ", paste(names(net), collapse = ", ")
    )
  }

  # Keep only the columns needed downstream (plus geometry)
  keep_cols <- intersect(
    c("osid", "startnode", "endnode", "geometry_length_m",
      "name1_text", "catchmentname", "watertype", "permanence"),
    names(net)
  )
  net <- net[, keep_cols]

  message("Loaded ", nrow(net), " watercourse links from '", layer, "'")
  net
}
