# reach.network

> River network simplification and diagram tools for OS NGD Water Network data.
> Part of the [flode](https://github.com/JonPayneEA/flode) ecosystem.

## Overview

`reach.network` builds simplified schematic and spatial network diagrams from
OS NGD (National Geographic Database) Water Network data. It is designed for
operational catchments where:

- River network data comes from the **OS NGD `wtr_ntwk_waterlink` layer**
  (GeoPackage), with directed topology encoded in `startnode`/`endnode` columns
- **Flow and stage gauges** are stored as spatial points that may not sit
  exactly on river lines — the package snaps them to the nearest reach within
  a configurable tolerance
- **Rain gauges** are explicitly excluded from snapping (they sit anywhere in
  the catchment) but can still be annotated with model metadata
- **Model metadata** links gauges to the hydrological/hydraulic models they
  serve (many-to-many; some gauges serve multiple models)

The package produces two output types:

| Output | Description |
|--------|-------------|
| Static PNG | Combined spatial + schematic view via `ggplot2` / `patchwork` |
| Interactive HTML | Zoomable / pannable widget via `visNetwork` |

---

## Installation

```r
# Install from GitHub
remotes::install_github("JonPayneEA/reach.network")
```

Dependencies: `sf`, `igraph`, `ggplot2`, `patchwork`, `visNetwork`,
`data.table`. All declared in `DESCRIPTION`.

---

## Quick start

### Minimal — no model metadata

```r
library(reach.network)

result <- network_diagram(
  network_path = "path/to/os_ngd_water.gpkg",
  gauge_path   = "path/to/gauges.gpkg",
  output_dir   = "outputs"
)
```

### With model metadata

```r
result <- network_diagram(
  network_path    = "path/to/os_ngd_water.gpkg",
  gauge_path      = "path/to/gauges.gpkg",
  model_meta_path = "path/to/model_metadata.csv",
  output_dir      = "outputs",
  tolerance_m     = 100          # snap gauges within 100 m
)
```

Saved outputs:

```
outputs/
├── river_network_spatial.png
├── river_network_schematic.png
├── river_network_combined.png
└── river_network_interactive.html
```

The function returns all intermediate objects invisibly for re-plotting:

```r
# Re-render the schematic without re-running the pipeline
print(result$p_schematic)

# Save the interactive widget separately
visNetwork::visSave(result$widget, "my_network.html")
```

### Model metadata format

Long-format CSV — one row per gauge-model pair:

```r
# See the expected structure
model_meta_template()
#>   gauge_id model_name
#> 1     G001   HEC-RAS
#> 2     G001      HECFM
#> 3     G002       ISIS
#> 4     P001   HEC-RAS

# Save as starting point
write.csv(model_meta_template(), "model_metadata.csv", row.names = FALSE)
```

---

## Step-by-step API

The `network_diagram()` wrapper calls these functions in sequence; you can
also call them individually for more control.

```r
# 1. Load OS NGD GeoPackage
network <- load_ngd_network("os_ngd_water.gpkg")

# 2. Load and snap gauges (rain gauges skipped automatically)
gauges  <- sf::st_read("gauges.gpkg")
gauges  <- snap_gauges(gauges, network, tolerance_m = 50)

# 3. Build gauge-to-node map (upstream node of snapped reach)
g_map   <- gauge_node_map(gauges, network)

# 4. Build full directed graph from startnode/endnode
g_full  <- build_ngd_graph(network)

# 5. Simplify: retain confluences, gauges, headwaters, outlet
result  <- simplify_network(g_full, g_map)

# 6. Compute hierarchical layout
layout  <- hierarchical_layout(result$graph, result$node_meta)

# 7. Plot
p_spatial   <- plot_spatial(network, gauges)
p_schematic <- plot_schematic(result$graph, layout)
widget      <- plot_interactive(result$graph, layout)
```

---

## Gauge type handling

The `gauges` spatial file must include a `gauge_type` column:

| `gauge_type` | Snapped to network? | Shown in schematic? |
|---|---|---|
| `"flow"` | Yes (within tolerance) | Yes — orange box on river topology |
| `"stage"` | Yes (within tolerance) | Yes — orange box on river topology |
| `"rain"` (or any other) | **No** | Yes — blue diamond in separate column |

Override which types are snapped via `snap_types`:

```r
snap_gauges(gauges, network, snap_types = c("flow", "stage", "level"))
```

---

## Synthetic example

A large (~75 reach, 12 gauge) synthetic catchment is included for testing
and demonstration:

```r
source(system.file("examples/synthetic_large.R", package = "reach.network"))
```

---

## Governance

| Attribute | Value |
|-----------|-------|
| Flode module | `reach.network` |
| Tier | 3 — Experimental |
| Author | Jon Payne |
| Ecosystem | fastverse + sf / igraph / visNetwork |
| R governance version | v1.3 |

Non-fastverse dependencies are justified in each file header: there are no
fastverse equivalents for GeoPackage I/O (`sf`), graph algorithms (`igraph`),
or interactive network widgets (`visNetwork`). CSV I/O uses
`data.table::fread()` per fastverse standards.

To promote to Tier 2: add `testthat` coverage ≥ 70 %, pass `lintr`, open a
GitHub issue to the Flode Steward.
