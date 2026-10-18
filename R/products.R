DTM_ALGORITHMS <- c("tin", "knnidw", "kriging")
CHM_ALGORITHMS <- c("pitfree", "p2r", "dsmtin")
ITS_ALGORITHMS <- c("li2012", "dalponte2016", "silva2016")

dtm_algorithm <- function(name) {
  switch(match.arg(name, DTM_ALGORITHMS),
    tin = lidR::tin(), knnidw = lidR::knnidw(), kriging = lidR::kriging())
}

chm_algorithm <- function(name) {
  switch(match.arg(name, CHM_ALGORITHMS),
    pitfree = lidR::pitfree(thresholds = c(0, 10, 20), max_edge = c(0, 1.5)),
    p2r = lidR::p2r(), dsmtin = lidR::dsmtin())
}

validate_catalog_settings <- function(resolution, workers, chunk_size, buffer_size) {
  ok <- function(x) length(x) == 1L && is.finite(x)
  if (!ok(resolution) || resolution < 0.1 || resolution > 50) stop("Set a resolution between 0.1 and 50 m.", call. = FALSE)
  if (!ok(workers) || workers < 1 || workers > max_workers() || !ok(chunk_size) || chunk_size < 250 || chunk_size > 2000 ||
      !ok(buffer_size) || buffer_size < 0 || buffer_size > 100 || buffer_size >= chunk_size / 2) {
    stop(sprintf("Set 1–%d workers, 250–2,000 m chunks, and a 0–100 m buffer smaller than half a chunk.", max_workers()), call. = FALSE)
  }
}
GROUND_ALGORITHMS <- c("none", "ptd", "csf", "mcc")

ground_algorithm <- function(name) {
  switch(match.arg(name, GROUND_ALGORITHMS),
    none = NULL, ptd = lidR::ptd(), csf = lidR::csf(), mcc = lidR::mcc(1.5, 0.3))
}

# Tiles with no points classified as ground (class 2); reads only class-2 points.
tiles_without_ground <- function(files) {
  has_ground <- vapply(files, function(f) {
    las <- lidR::readLAS(f, select = "c", filter = "-keep_class 2 -first_n 1")
    !lidR::is.empty(las)
  }, logical(1))
  files[!has_ground]
}

# Opens the selected tiles as one catalog, classifies ground only when a selected algorithm is
# needed (some tile has no ground points), and normalizes heights with TIN.
normalized_catalog <- function(files, work_dir, chunk_size, buffer_size, workers, report, ground_algo = "none") {
  ground_fun <- ground_algorithm(ground_algo)
  files <- unique(normalizePath(files, winslash = "/", mustWork = TRUE))
  if (!length(files) || any(!grepl("\\.la[sz]$", files, ignore.case = TRUE))) {
    stop("Choose at least one LAS or LAZ file.", call. = FALSE)
  }
  for (stage in c("classified", "normalized")) dir.create(file.path(work_dir, stage), showWarnings = FALSE)
  report(0.02, sprintf("Reading headers for %d selected tile(s)", length(files)))
  catalog <- lidR::readLAScatalog(files, progress = FALSE)
  if (lidR::is.overlapping(catalog)) {
    stop("The selected tiles overlap. Choose tiles from one non-overlapping acquisition for a seamless catalog.", call. = FALSE)
  }
  setup <- function(x) {
    lidR::opt_chunk_size(x) <- chunk_size
    lidR::opt_chunk_buffer(x) <- buffer_size
    lidR::opt_progress(x) <- FALSE
    x
  }
  catalog <- setup(catalog)
  report(0.05, "Checking for classified ground points")
  missing <- tiles_without_ground(files)
  if (length(missing) && is.null(ground_fun)) {
    stop(sprintf("%d of %d selected tile(s) have no ground-classified points (e.g. %s). Choose a ground classification algorithm.",
                 length(missing), length(files), basename(missing[[1L]])), call. = FALSE)
  }
  if (length(missing)) {
    report(0.08, sprintf("Classifying ground with %s (%d tile(s) lacked ground points)", ground_algo, length(missing)))
    lidR::opt_output_files(catalog) <- file.path(work_dir, "classified", "{XLEFT}_{YBOTTOM}_classified")
    classified <- setup(lidR::classify_ground(catalog, ground_fun))
  } else {
    classified <- catalog
  }
  report(0.3, "Normalizing heights with TIN")
  lidR::opt_output_files(classified) <- file.path(work_dir, "normalized", "{XLEFT}_{YBOTTOM}_normalized")
  list(classified = classified, normalized = setup(lidR::normalize_height(classified, lidR::tin())))
}

with_catalog_workspace <- function(output_dir, workers, code, exports = list()) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  work_dir <- tempfile(".catalog-work-", tmpdir = output_dir)
  dir.create(work_dir)
  old_plan <- future::plan()
  cluster <- NULL
  on.exit({
    future::plan(old_plan)
    if (!is.null(cluster)) parallel::stopCluster(cluster)
    unlink(work_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  if (workers == 1L) {
    future::plan(future::sequential)
  } else if (length(exports)) {
    # lidR evaluates metric formulas as text on the workers, so helpers must live in their global environment.
    cluster <- parallelly::makeClusterPSOCK(workers)
    for (name in names(exports)) {
      fn <- exports[[name]]
      environment(fn) <- globalenv()
      parallel::clusterCall(cluster, assign, name, fn, envir = globalenv())
    }
    future::plan(future::cluster, workers = cluster)
  } else {
    future::plan(future::multisession, workers = workers)
  }
  code(output_dir, work_dir)
}

unique_stem <- function(output_dir, prefix, suffixes) {
  base <- paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  stem <- base
  i <- 2L
  while (any(file.exists(file.path(output_dir, paste0(stem, suffixes))))) {
    stem <- paste0(base, "_", i)
    i <- i + 1L
  }
  stem
}

res_label <- function(resolution) paste0(format(resolution, trim = TRUE, scientific = FALSE), "m")

process_dem_chm <- function(files, output_dir, resolution = 0.5, dtm_algo = "tin", chm_algo = "pitfree",
                            ground_algo = "none",
                            workers = 2L, chunk_size = 250, buffer_size = 20, progress = NULL) {
  resolution <- as.numeric(resolution); workers <- as.integer(workers)
  chunk_size <- as.numeric(chunk_size); buffer_size <- as.numeric(buffer_size)
  validate_catalog_settings(resolution, workers, chunk_size, buffer_size)
  dtm_fun <- dtm_algorithm(dtm_algo)
  chm_fun <- chm_algorithm(chm_algo)
  report <- function(value, stage) if (is.function(progress)) progress(value, stage)
  with_catalog_workspace(output_dir, workers, function(output_dir, work_dir) {
    cats <- normalized_catalog(files, work_dir, chunk_size, buffer_size, workers, report, ground_algo)
    dir.create(file.path(work_dir, "dtm"), showWarnings = FALSE)
    dir.create(file.path(work_dir, "chm"), showWarnings = FALSE)
    report(0.5, sprintf("Rasterizing the %s DEM (%s)", res_label(resolution), dtm_algo))
    classified <- cats$classified
    lidR::opt_output_files(classified) <- file.path(work_dir, "dtm", "{XLEFT}_{YBOTTOM}_dtm")
    dem <- lidR::rasterize_terrain(classified, res = resolution, algorithm = dtm_fun)
    report(0.75, sprintf("Rasterizing the %s CHM (%s)", res_label(resolution), chm_algo))
    normalized <- cats$normalized
    lidR::opt_output_files(normalized) <- file.path(work_dir, "chm", "{XLEFT}_{YBOTTOM}_chm")
    chm <- lidR::rasterize_canopy(normalized, res = resolution, algorithm = chm_fun)
    names(dem) <- "elevation_m"
    names(chm) <- "canopy_height_m"
    suffixes <- c(paste0("_DEM_", res_label(resolution), ".tif"), paste0("_CHM_", res_label(resolution), ".tif"))
    stem <- unique_stem(output_dir, "catalog", suffixes)
    out <- c(dem = file.path(output_dir, paste0(stem, suffixes[[1L]])),
             chm = file.path(output_dir, paste0(stem, suffixes[[2L]])))
    report(0.95, "Writing GeoTIFF files")
    terra::writeRaster(dem, out[["dem"]], overwrite = TRUE)
    terra::writeRaster(chm, out[["chm"]], overwrite = TRUE)
    report(1, "Complete")
    out
  })
}

its_tree_stats <- function(X, Y, Z) {
  i <- which.max(Z)
  h <- grDevices::chull(X, Y)
  x <- X[h]; y <- Y[h]
  area <- if (length(h) >= 3L) abs(sum(x * c(y[-1L], y[1L]) - c(x[-1L], x[1L]) * y)) / 2 else 0
  list(ht = max(Z), cr_area = area, cr_volume = area * mean(Z),
       x_max = X[i], y_max = Y[i], x_bray = mean(X), y_bray = mean(Y))
}

process_its <- function(files, output_dir, resolution = 0.5, its_algo = "li2012", treetop_window = 5,
                        ground_algo = "none",
                        workers = 2L, chunk_size = 250, buffer_size = 20, progress = NULL) {
  resolution <- as.numeric(resolution); workers <- as.integer(workers)
  chunk_size <- as.numeric(chunk_size); buffer_size <- as.numeric(buffer_size)
  treetop_window <- as.numeric(treetop_window)
  validate_catalog_settings(resolution, workers, chunk_size, buffer_size)
  its_algo <- match.arg(its_algo, ITS_ALGORITHMS)
  if (length(treetop_window) != 1L || !is.finite(treetop_window) || treetop_window < 1 || treetop_window > 30) {
    stop("Set a tree-top window between 1 and 30 m.", call. = FALSE)
  }
  report <- function(value, stage) if (is.function(progress)) progress(value, stage)
  with_catalog_workspace(output_dir, workers, function(output_dir, work_dir) {
    cats <- normalized_catalog(files, work_dir, chunk_size, buffer_size, workers, report, ground_algo)
    normalized <- cats$normalized
    algorithm <- if (its_algo == "li2012") {
      lidR::li2012()
    } else {
      report(0.5, "Building the canopy height model and detecting tree tops")
      chm <- lidR::rasterize_canopy(normalized, res = resolution, algorithm = lidR::p2r())
      tops <- lidR::locate_trees(chm, lidR::lmf(treetop_window))
      if (its_algo == "dalponte2016") lidR::dalponte2016(chm, tops) else lidR::silva2016(chm, tops)
    }
    dir.create(file.path(work_dir, "segmented"), showWarnings = FALSE)
    report(0.6, sprintf("Segmenting trees (%s)", its_algo))
    lidR::opt_output_files(normalized) <- file.path(work_dir, "segmented", "{XLEFT}_{YBOTTOM}_its")
    segmented <- lidR::segment_trees(normalized, algorithm, uniqueness = "bitmerge")
    lidR::opt_chunk_size(segmented) <- chunk_size
    lidR::opt_chunk_buffer(segmented) <- buffer_size
    lidR::opt_output_files(segmented) <- ""
    report(0.88, "Computing tree metrics (points and convex-hull crowns)")
    # lidR evaluates metric formulas by name, so the helper and formula live in the global environment
    # (also installed on each parallel worker by with_catalog_workspace).
    stats_fun <- its_tree_stats
    environment(stats_fun) <- globalenv()
    assign("its_tree_stats", stats_fun, envir = globalenv())
    tree_fun <- stats::as.formula("~its_tree_stats(X, Y, Z)", env = globalenv())
    points <- lidR::crown_metrics(segmented, tree_fun, geom = "point")
    crowns <- lidR::crown_metrics(segmented, tree_fun, geom = "convex")
    stem <- unique_stem(output_dir, "catalog", "_trees.gpkg")
    out <- c(trees = file.path(output_dir, paste0(stem, "_trees.gpkg")))
    report(0.95, "Writing GeoPackage")
    sf::st_write(points, out[["trees"]], layer = "tree_points", delete_dsn = TRUE, quiet = TRUE)
    sf::st_write(crowns, out[["trees"]], layer = "tree_crowns", quiet = TRUE)
    report(1, "Complete")
    out
  }, exports = list(its_tree_stats = its_tree_stats))
}

# Leaflet map of tree crowns (polygons) and tree points from a GeoPackage, colored by height with
# viridis magma (dark = low, light = high). Data are reprojected from the lidar CRS for display only.
tree_preview_map <- function(path) {
  map <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri World Imagery") |>
    leaflet::addProviderTiles(leaflet::providers$OpenTopoMap, group = "OpenTopoMap") |>
    leaflet::addLayersControl(baseGroups = c("Esri World Imagery", "OpenTopoMap"),
                              options = leaflet::layersControlOptions(collapsed = FALSE))
  if (is.null(path) || !file.exists(path)) return(map)
  crowns <- sf::st_transform(sf::st_read(path, layer = "tree_crowns", quiet = TRUE), 4326)
  points <- sf::st_transform(sf::st_read(path, layer = "tree_points", quiet = TRUE), 4326)
  if (!nrow(crowns)) return(map)
  if (nrow(crowns) > 20000L) {
    crowns <- crowns[seq_len(20000L), ]
    points <- points[seq_len(min(20000L, nrow(points))), ]
  }
  range <- range(crowns$ht, na.rm = TRUE)
  if (range[[1L]] == range[[2L]]) range <- range[[1L]] + c(0, 1)
  pal <- leaflet::colorNumeric(viridisLite::magma(256), domain = range, na.color = "transparent")
  box <- sf::st_bbox(crowns)
  map |>
    leaflet::addPolygons(data = crowns, group = "Crowns", weight = 1, color = "#444444",
                         fillColor = ~pal(ht), fillOpacity = 0.7,
                         popup = ~sprintf("ht %.1f m<br>cr_area %.1f m2<br>cr_volume %.1f m3", ht, cr_area, cr_volume)) |>
    leaflet::addCircleMarkers(data = points, group = "Tree points", radius = 2, weight = 0,
                              fillColor = ~pal(ht), fillOpacity = 1, color = "#ffffff") |>
    leaflet::addLegend(pal = pal, values = range, title = "Tree height (m)", position = "bottomright") |>
    leaflet::addLayersControl(baseGroups = c("Esri World Imagery", "OpenTopoMap"),
                              overlayGroups = c("Crowns", "Tree points"),
                              options = leaflet::layersControlOptions(collapsed = FALSE)) |>
    leaflet::fitBounds(box[["xmin"]], box[["ymin"]], box[["xmax"]], box[["ymax"]])
}

# Leaflet map with a single raster drawn in viridis::magma (dark = low, light = high),
# reprojected by leaflet from the lidar CRS.
raster_preview_map <- function(path, legend_title) {
  map <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri World Imagery") |>
    leaflet::addProviderTiles(leaflet::providers$OpenTopoMap, group = "OpenTopoMap") |>
    leaflet::addLayersControl(baseGroups = c("Esri World Imagery", "OpenTopoMap"),
                              options = leaflet::layersControlOptions(collapsed = FALSE))
  if (is.null(path) || !file.exists(path)) return(map)
  r <- preview_raster(path)
  range <- as.numeric(terra::global(r, c("min", "max"), na.rm = TRUE)[1L, ])
  if (any(!is.finite(range)) || range[[1L]] == range[[2L]]) range <- if (any(is.finite(range))) range[[1L]] + c(0, 1) else c(0, 1)
  pal <- leaflet::colorNumeric(viridisLite::magma(256), domain = range, na.color = "transparent")
  box <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(
    c(xmin = terra::xmin(r), ymin = terra::ymin(r), xmax = terra::xmax(r), ymax = terra::ymax(r)),
    crs = terra::crs(r))), 4326))
  map <- map |>
    leaflet::addRasterImage(r, colors = pal, opacity = 0.85, maxBytes = 20 * 1024 * 1024, group = "Raster") |>
    leaflet::addLegend(pal = pal, values = range, title = legend_title,     position = "bottomright") |>
        leaflet::fitBounds(box[["xmin"]], box[["ymin"]], box[["xmax"]], box[["ymax"]])
      map
    }

    zip_outputs <- function(paths, file) {
  if (is.null(paths) || !length(paths)) stop("Generate rasters first.", call. = FALSE)
  paths <- paths[file.exists(paths)]
  zip::zipr(zipfile = file, files = basename(paths), root = dirname(paths[[1L]]))
}
