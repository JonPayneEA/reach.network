# ============================================================ #
# Tool:         snap_gauges / gauge_node_map
# Description:  Snaps flow/stage gauges to the nearest OS NGD reach within
#               a tolerance; rain gauges are explicitly excluded.
# Flode Module: reach.network
# Author:       Jon Payne
# Created:      2026-04-01
# Modified:     2026-04-01 - JP: initial creation
# Tier:         3
# Inputs:       sf POINT gauges (with gauge_type column); sf LINESTRING network
# Outputs:      gauges sf with snapped_osid, snap_distance_m, snapped columns
# Dependencies: sf (non-fastverse; spatial nearest-feature ops)
# ============================================================ #

#' Snap gauges to the nearest river reach
#'
#' For each gauge whose `gauge_type` is in `snap_types`, finds the nearest
#' watercourse link in `network` and records its `osid` as the anchor for
#' graph construction. Gauges outside `tolerance_m` are flagged as unsnapped
#' but retained.
#'
#' **Rain gauges are not snapped.** They sit anywhere in the catchment and
#' snapping them to the nearest river line is meaningless. They appear in the
#' spatial plot at their true location and in the schematic only via model
#' metadata (not via river topology). Pass `gauge_type = "rain"` (or whichever
#' string your data uses) and ensure it is absent from `snap_types`.
#'
#' @param gauges An sf object of gauge points. Must contain:
#'   - `gauge_id`   — unique identifier
#'   - `gauge_name` — display name
#'   - `gauge_type` — type string, e.g. `"flow"`, `"stage"`, `"rain"`
#' @param network An sf LINESTRING object as returned by [load_ngd_network()].
#'   Must contain the `osid` column.
#' @param tolerance_m Numeric. Maximum snapping distance in metres. Gauges
#'   further than this from any reach are flagged as unsnapped. Default `50`.
#' @param snap_types Character vector. Gauge types that should be snapped to
#'   the network. Default `c("flow", "stage")`. All other types are left
#'   unsnapped.
#'
#' @return The input `gauges` sf object with three additional columns:
#'   \describe{
#'     \item{`snapped_osid`}{`osid` of the nearest reach (NA if unsnapped)}
#'     \item{`snap_distance_m`}{Distance in metres to the nearest reach
#'       (NA for types not in `snap_types`)}
#'     \item{`snapped`}{Logical: TRUE if snapped successfully}
#'   }
#'   Gauge geometries are **not moved** — the original point locations are
#'   preserved for the spatial plot.
#'
#' @details
#' The function also prints a summary table of snapping results and issues a
#' warning for any gauge that is eligible for snapping but falls outside the
#' tolerance.
#'
#' @examples
#' \dontrun{
#' network <- load_ngd_network("os_ngd_water.gpkg")
#' gauges  <- sf::st_read("gauges.gpkg")
#' gauges_snapped <- snap_gauges(gauges, network, tolerance_m = 100)
#' }
#'
#' @export
snap_gauges <- function(gauges, network, tolerance_m = 50,
                         snap_types = c("flow", "stage")) {
  if (!"gauge_type" %in% names(gauges)) {
    stop(
      "'gauges' must contain a 'gauge_type' column. ",
      "Add it with values such as 'flow', 'stage', or 'rain'."
    )
  }
  if (!"osid" %in% names(network)) {
    stop("'network' must contain an 'osid' column (use load_ngd_network()).")
  }

  n <- nrow(gauges)
  gauges$snapped_osid    <- NA_character_
  gauges$snap_distance_m <- NA_real_
  gauges$snapped         <- FALSE

  # Indices of gauges eligible for snapping
  eligible <- which(gauges$gauge_type %in% snap_types)
  n_rain   <- sum(!gauges$gauge_type %in% snap_types)

  if (length(eligible) == 0) {
    message("No gauges with gauge_type in c(",
            paste0('"', snap_types, '"', collapse = ", "),
            ") — nothing to snap.")
    return(gauges)
  }

  eligible_gauges <- gauges[eligible, ]

  # Nearest reach index for each eligible gauge
  nearest_idx <- sf::st_nearest_feature(eligible_gauges, network)

  # Row-wise distances (units object → drop to numeric metres)
  dists <- vapply(seq_along(eligible), \(i) {
    as.numeric(sf::st_distance(
      eligible_gauges[i, ],
      network[nearest_idx[i], ]
    ))
  }, numeric(1))

  snapped_mask <- dists <= tolerance_m

  gauges$snapped_osid[eligible[snapped_mask]]    <- network$osid[nearest_idx[snapped_mask]]
  gauges$snap_distance_m[eligible]               <- dists
  gauges$snapped[eligible[snapped_mask]]          <- TRUE

  # Report
  n_snapped   <- sum(snapped_mask)
  n_unsnapped <- sum(!snapped_mask)

  message(sprintf(
    "Snapping summary: %d eligible | %d snapped (<=%.0f m) | %d outside tolerance | %d non-snappable type(s)",
    length(eligible), n_snapped, tolerance_m, n_unsnapped, n_rain
  ))

  if (n_unsnapped > 0) {
    unsnapped_names <- gauges$gauge_name[eligible[!snapped_mask]]
    unsnapped_dists <- dists[!snapped_mask]
    warning(
      n_unsnapped, " gauge(s) outside ", tolerance_m, " m tolerance ",
      "(will appear in spatial plot but NOT anchored to the network):\n",
      paste0(
        "  ", unsnapped_names,
        " (", round(unsnapped_dists, 1), " m)",
        collapse = "\n"
      )
    )
  }

  gauges
}


#' Extract gauge-to-node mapping for graph construction
#'
#' Given snapped gauges and the full network, returns a named character vector
#' mapping gauge display labels to the `startnode` of their snapped reach.
#' This is the format expected by [simplify_network()].
#'
#' Only gauges with `snapped == TRUE` are included.
#'
#' @param gauges Snapped gauges sf as returned by [snap_gauges()].
#' @param network Network sf as returned by [load_ngd_network()].
#' @param label_col Character. Column in `gauges` used for node labels.
#'   Defaults to `"gauge_name"`.
#'
#' @return Named character vector: `c("Gauge Name" = "startnode_id", ...)`.
#'
#' @export
gauge_node_map <- function(gauges, network, label_col = "gauge_name") {
  snapped <- gauges[gauges$snapped, ]
  if (nrow(snapped) == 0) {
    return(setNames(character(0), character(0)))
  }

  # Look up the startnode of the snapped reach
  nodes <- vapply(snapped$snapped_osid, \(oid) {
    row <- which(network$osid == oid)
    if (length(row) == 0) return(NA_character_)
    # startnode is the upstream (from) end — place gauge there to avoid
    # landing on a downstream confluence
    as.character(network$startnode[row[1]])
  }, character(1))

  setNames(nodes, snapped[[label_col]])
}
