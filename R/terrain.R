lidar_files_in_folder <- function(folder) {
  if (!dir.exists(folder)) return(character())
  sort(list.files(folder, pattern = "\\.la[sz]$", recursive = TRUE,
                  full.names = TRUE, ignore.case = TRUE))
}

process_lidar_file <- function(file, output_dir, resolution = 0.5, progress = NULL, name_prefix = "") {
  if (!file.exists(file) || !grepl("\\.la[sz]$", file, ignore.case = TRUE)) {
    stop("Choose an existing LAS or LAZ file.", call. = FALSE)
  }
  if (!isTRUE(all.equal(resolution, 0.5))) stop("This workflow uses 0.5 m resolution.", call. = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  report <- function(step, stage) if (is.function(progress)) progress(step, 7L, stage)
  report(1L, "Reading LAS/LAZ points")
  las <- lidR::readLAS(file)
  if (lidR::is.empty(las)) stop("This LAS/LAZ file has no points.", call. = FALSE)

  report(2L, "Classifying ground with PTD")
  classified <- lidR::classify_ground(las, lidR::ptd())
  report(3L, "Rasterizing the 0.5 m terrain model")
  dtm <- lidR::rasterize_terrain(classified, res = 0.5, algorithm = lidR::tin())
  report(4L, "Normalizing point heights with TIN")
  normalized <- lidR::normalize_height(classified, lidR::tin())
  report(5L, "Rasterizing the 0.5 m pit-free canopy")
  chm <- lidR::rasterize_canopy(
    normalized, res = 0.5,
    algorithm = lidR::pitfree(thresholds = c(0, 10, 20), max_edge = c(0, 1.5))
  )
  names(chm) <- "canopy_height_m"
  names(dtm) <- "ground_elevation_m"
  report(6L, "Computing slope, aspect, and hillshade")
  slope_aspect <- terra::terrain(dtm, v = c("slope", "aspect"), unit = "radians")
  hillshade <- terra::shade(slope = slope_aspect$slope, aspect = slope_aspect$aspect)
  names(hillshade) <- "hillshade"

  stem <- paste0(name_prefix, tools::file_path_sans_ext(basename(file)))
  output <- c(
    chm = file.path(output_dir, paste0(stem, "_CHM_0.5m.tif")),
    dtm = file.path(output_dir, paste0(stem, "_DTM_0.5m.tif")),
    hillshade = file.path(output_dir, paste0(stem, "_hillshade_0.5m.tif"))
  )
  report(7L, "Writing GeoTIFF files")
  terra::writeRaster(chm, output[["chm"]], overwrite = TRUE)
  terra::writeRaster(dtm, output[["dtm"]], overwrite = TRUE)
  terra::writeRaster(hillshade, output[["hillshade"]], overwrite = TRUE)
  output
}

process_lidar_catalog <- function(files, output_dir, workers = 2L,
                                  chunk_size = 250, buffer_size = 20, progress = NULL) {
  workers <- as.integer(workers)
  chunk_size <- as.numeric(chunk_size)
  buffer_size <- as.numeric(buffer_size)
  if (length(workers) != 1L || is.na(workers) || workers < 1L || workers > 8L ||
      length(chunk_size) != 1L || !is.finite(chunk_size) || chunk_size < 250 || chunk_size > 2000 ||
      length(buffer_size) != 1L || !is.finite(buffer_size) || buffer_size < 0 || buffer_size > 100 ||
      buffer_size >= chunk_size / 2) {
    stop("Set 1–8 workers, 250–2,000 m chunks, and a 0–100 m buffer smaller than half a chunk.", call. = FALSE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  work_dir <- tempfile(".catalog-work-", tmpdir = output_dir)
  dir.create(work_dir)
  on.exit({
    target <- normalizePath(work_dir, winslash = "/", mustWork = FALSE)
    if (startsWith(tolower(target), paste0(tolower(output_dir), "/.catalog-work-"))) {
      unlink(target, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)
  for (stage in c("classified", "normalized", "dtm_chunks", "chm_chunks")) {
    dir.create(file.path(work_dir, stage))
  }
  report <- function(value, stage) if (is.function(progress)) progress(value, stage)

  report(0.02, sprintf("Reading headers for %d selected tiles", length(files)))
  catalog <- lidR::readLAScatalog(files, progress = FALSE)
  if (lidR::is.overlapping(catalog)) {
    stop("The selected tiles overlap. Choose tiles from one non-overlapping acquisition for a seamless catalog.", call. = FALSE)
  }
  lidR::opt_chunk_size(catalog) <- chunk_size
  lidR::opt_chunk_buffer(catalog) <- buffer_size
  lidR::opt_progress(catalog) <- FALSE

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  if (workers == 1L) future::plan(future::sequential) else
    future::plan(future::multisession, workers = workers)

  report(0.08, "Classifying catalog ground with PTD")
  lidR::opt_output_files(catalog) <- file.path(work_dir, "classified", "{XLEFT}_{YBOTTOM}_classified")
  classified <- lidR::classify_ground(catalog, lidR::ptd())
  lidR::opt_chunk_size(classified) <- chunk_size
  lidR::opt_chunk_buffer(classified) <- buffer_size
  lidR::opt_progress(classified) <- FALSE

  report(0.28, "Rasterizing the 0.5 m terrain catalog")
  lidR::opt_output_files(classified) <- file.path(work_dir, "dtm_chunks", "{XLEFT}_{YBOTTOM}_dtm")
  dtm <- lidR::rasterize_terrain(classified, res = 0.5, algorithm = lidR::tin())

  report(0.48, "Normalizing catalog heights with TIN")
  lidR::opt_output_files(classified) <- file.path(work_dir, "normalized", "{XLEFT}_{YBOTTOM}_normalized")
  normalized <- lidR::normalize_height(classified, lidR::tin())
  lidR::opt_chunk_size(normalized) <- chunk_size
  lidR::opt_chunk_buffer(normalized) <- buffer_size
  lidR::opt_progress(normalized) <- FALSE

  report(0.65, "Rasterizing the 0.5 m pit-free canopy catalog")
  lidR::opt_output_files(normalized) <- file.path(work_dir, "chm_chunks", "{XLEFT}_{YBOTTOM}_chm")
  chm <- lidR::rasterize_canopy(normalized, res = 0.5,
    algorithm = lidR::pitfree(thresholds = c(0, 10, 20), max_edge = c(0, 1.5)))

  report(0.82, "Computing slope, aspect, and hillshade")
  slope_aspect <- terra::terrain(dtm, v = c("slope", "aspect"), unit = "radians")
  hillshade <- terra::shade(slope = slope_aspect$slope, aspect = slope_aspect$aspect)
  names(chm) <- "canopy_height_m"
  names(dtm) <- "ground_elevation_m"
  names(hillshade) <- "hillshade"

  base_name <- paste0("catalog_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  stem <- base_name
  suffix <- 2L
  while (any(file.exists(file.path(output_dir, paste0(stem, c("_CHM_0.5m.tif", "_DTM_0.5m.tif", "_hillshade_0.5m.tif")))))) {
    stem <- paste0(base_name, "_", suffix)
    suffix <- suffix + 1L
  }
  output <- c(
    chm = file.path(output_dir, paste0(stem, "_CHM_0.5m.tif")),
    dtm = file.path(output_dir, paste0(stem, "_DTM_0.5m.tif")),
    hillshade = file.path(output_dir, paste0(stem, "_hillshade_0.5m.tif"))
  )
  report(0.9, "Writing seamless GeoTIFF files")
  terra::writeRaster(chm, output[["chm"]], overwrite = TRUE)
  terra::writeRaster(dtm, output[["dtm"]], overwrite = TRUE)
  terra::writeRaster(hillshade, output[["hillshade"]], overwrite = TRUE)
  report(1, "Catalog processing complete")
  output
}

process_lidar_files <- function(files, output_dir, progress = NULL,
                                workers = 2L, chunk_size = 250, buffer_size = 20) {
  files <- unique(normalizePath(files, winslash = "/", mustWork = TRUE))
  if (!length(files)) stop("Choose at least one LAS/LAZ file.", call. = FALSE)
  if (any(!grepl("\\.la[sz]$", files, ignore.case = TRUE))) {
    stop("All selected inputs must be LAS or LAZ files.", call. = FALSE)
  }
  if (length(files) > 1L) {
    result <- process_lidar_catalog(files, output_dir, workers, chunk_size, buffer_size, progress)
    return(stats::setNames(list(result), sprintf("Catalog (%d tiles)", length(files))))
  }
  results <- vector("list", length(files))
  for (i in seq_along(files)) {
    results[[i]] <- process_lidar_file(files[[i]], output_dir, name_prefix = sprintf("%03d_", i),
                                      progress = function(step, total_steps, stage) {
      if (is.function(progress)) {
        progress((i - 1 + (step - 1) / total_steps) / length(files),
                 sprintf("File %d/%d — %s: %s", i, length(files), basename(files[[i]]), stage))
      }
    })
  }
  names(results) <- basename(files)
  results
}

preview_raster <- function(path, max_cells = 1800000L) {
  x <- terra::rast(path)
  if (terra::ncell(x) > max_cells) {
    factor <- ceiling(sqrt(terra::ncell(x) / max_cells))
    x <- terra::aggregate(x, fact = factor, fun = mean, na.rm = TRUE)
  }
  x
}
