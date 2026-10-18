# USGS 3DEP Lidar Explorer

A local Shiny app that accepts an `sf` point, polygon, or multipolygon; finds every overlapping USGS 3DEP lidar project; lets the user choose among overlapping projects and individual LAS/LAZ tiles; and downloads those tiles to a chosen folder.

## Start on Windows

Install R and the packages listed below, then double-click `USGS_3DEP_Lidar_Explorer.bat`. The launcher finds `Rscript.exe`, starts the Shiny app in your browser, and keeps the R process in its command window. Keep that window open while using the app; close it or press Ctrl+C to stop the app.

The launcher uses its own folder for the app files, so you can move or copy the complete project folder without changing paths. It also works from a Windows network share. No Electron, Node.js, `shiny.exe`, or prebuilt executable is needed.

The project polygons come from layer 24 (`Lidar Point Cloud`) of the USGS `3DEPElevationIndex` map service. Tile records and bounding boxes come from the official TNM Access products API, then tile URLs are checked against each project's `0_file_download_links.txt` manifest when it is available. The map also displays layer 8 of the index as the USGS availability overlay.

<img src="https://github.com/Quantitative-Ecology-Lab-NAU/USGSlazDownloader/blob/master/Screenshot.png" width="100%"  height="auto" align="center"/>

## Start the app

Open `USGSLidarExplorer.Rproj` in RStudio, then run this from the project:

```r
source("run_app.R")
run_usgs_lidar_app()
```

Or at a command prompt (from this folder):

```text
Rscript run_app.R
```

The required packages are `shiny`, `sf`, `leaflet`, `leaflet.extras`, `curl`, `jsonlite`, `rlas`, `lidR`, `terra`, `viridisLite`, `zip`, and `future`.

```r
install.packages(c("shiny", "sf", "leaflet", "leaflet.extras", "curl", "jsonlite", "rlas", "lidR", "terra", "viridisLite", "zip", "future"))
```

## Start with an existing sf object

```r
library(sf)

my_aoi <- st_read("my_aoi.gpkg")
source("run_app.R")
run_usgs_lidar_app(my_aoi)
```

The app also lets you draw an extent, polygon, or point on the map; enter WGS84 coordinates; or upload a zipped shapefile. RDS/RData, GeoPackage, GeoJSON, and KML uploads remain supported. Uploaded objects must have a valid CRS. Polygonal areas of interest are limited to 4,000 hectares.

## Generate CHM and shaded DEM

The **Terrain & canopy** tab finds downloaded LAS/LAZ files in a folder. Choose one or more files, then generate 0.5 m GeoTIFFs for a pit-free canopy height model (CHM), TIN digital terrain model (DTM), and shaded DEM. Processing classifies ground with `ptd()`, normalizes heights with `tin()`, and shows a stage-based progress bar. The map previews the CHM using magma colors or the hillshade in grayscale over Esri World Imagery (default) or OpenTopoMap. The generated files can be downloaded together as a ZIP.

When you select more than one LAS/LAZ tile, the app builds a `lidR` catalog and produces one seamless set of rasters. You can set the number of workers (1–8; default 2), chunk size (250–2,000 m; default 250 m), and chunk buffer (0–100 m, less than half the chunk size; default 20 m). Buffered chunks reduce edge artifacts, but additional workers require more memory. Catalog processing uses temporary intermediate LAS and raster chunks, removes them after completion, and keeps the final GeoTIFFs in the generated-file folder. Select tiles from one non-overlapping acquisition; overlapping tiles are rejected to avoid conflicting catalog output.

## Download behavior

- All overlapping lidar projects remain separate so users can choose one or several vintages.
- Only tile bounding boxes that intersect the exact `sf` geometry are retained.
- Tile URLs are cross-checked against the selected USGS project download manifests when those manifests are available.
- Selected tiles are written under `<destination>/<project>/LAZ/` (or the corresponding USGS subfolder).
- Existing files with the expected byte size are skipped.
- Incomplete downloads use `.part` files and resume when the server supports byte ranges.
- Downloads retry with backoff and can run in parallel.
- `selected_download_links.txt` records the requested URLs; failures are written to `failed_downloads.csv`.
- Validation reads each downloaded header with `rlas` and opens the destination as a `lidR` catalog without loading all points into memory.

## Authoritative sources

- [USGS 3DEP Elevation Index map service](https://index.nationalmap.gov/arcgis/rest/services/3DEPElevationIndex/MapServer/)
- [USGS Lidar Explorer](https://apps.nationalmap.gov/lidar-explorer/#/)
- [The National Map Access API](https://tnmaccess.nationalmap.gov/api/v1/)
- [NOAA Interagency Elevation Inventory](https://coast.noaa.gov/inventory/)

The destination is on the computer running the R process. A remotely hosted Shiny deployment cannot write directly to a visitor's arbitrary local folder.
