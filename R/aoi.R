normalize_aoi <- function(x) {
  if (inherits(x, "sfc")) x <- sf::st_sf(geometry = x)
  if (!inherits(x, "sf")) stop("The area of interest must be an sf or sfc object.", call. = FALSE)
  if (!nrow(x)) stop("The area of interest has no features.", call. = FALSE)
  if (is.na(sf::st_crs(x))) stop("The area of interest needs a coordinate reference system (CRS).", call. = FALSE)

  x <- sf::st_zm(x, drop = TRUE, what = "ZM")
  x <- sf::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  if (!nrow(x)) stop("The area of interest contains only empty geometry.", call. = FALSE)
  x <- sf::st_transform(x, 4326)
  area_ha <- aoi_area_ha(x)
  if (area_ha > 4000) {
    stop(sprintf("The area of interest is %.1f ha; the maximum is 4,000 ha.", area_ha), call. = FALSE)
  }
  x
}

aoi_area_ha <- function(x) {
  if (!inherits(x, "sf") && !inherits(x, "sfc")) stop("Expected sf geometry.", call. = FALSE)
  geometry <- sf::st_geometry(x)
  polygons <- geometry[as.character(sf::st_geometry_type(geometry)) %in% c("POLYGON", "MULTIPOLYGON")]
  if (!length(polygons)) return(0)
  as.numeric(sum(sf::st_area(sf::st_transform(sf::st_union(polygons), 6933)))) / 10000
}

aoi_from_point <- function(longitude, latitude) {
  longitude <- as.numeric(longitude)
  latitude <- as.numeric(latitude)
  if (!is.finite(longitude) || !is.finite(latitude) || abs(longitude) > 180 || abs(latitude) > 90) {
    stop("Enter a valid longitude (-180 to 180) and latitude (-90 to 90).", call. = FALSE)
  }
  normalize_aoi(sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(longitude, latitude)), crs = 4326)))
}

aoi_from_bounds <- function(west, south, east, north) {
  values <- as.numeric(c(west, south, east, north))
  if (any(!is.finite(values)) || values[[1L]] >= values[[3L]] || values[[2L]] >= values[[4L]] ||
      any(abs(values[c(1L, 3L)]) > 180) || any(abs(values[c(2L, 4L)]) > 90)) {
    stop("Enter valid west, south, east, north bounds in decimal degrees.", call. = FALSE)
  }
  normalize_aoi(sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(
    c(xmin = values[[1L]], ymin = values[[2L]], xmax = values[[3L]], ymax = values[[4L]]),
    crs = 4326
  ))))
}

aoi_from_geojson <- function(feature) {
  json <- jsonlite::toJSON(feature, auto_unbox = TRUE, null = "null", digits = NA)
  path <- tempfile(fileext = ".geojson")
  on.exit(unlink(path), add = TRUE)
  writeLines(as.character(json), path, useBytes = TRUE)
  normalize_aoi(sf::st_read(path, quiet = TRUE))
}

read_aoi_file <- function(path, original_name = basename(path)) {
  ext <- tolower(tools::file_ext(original_name))

  if (ext == "rds") {
    return(normalize_aoi(readRDS(path)))
  }

  if (ext %in% c("rda", "rdata")) {
    env <- new.env(parent = emptyenv())
    loaded <- load(path, envir = env)
    candidates <- Filter(
      function(name) inherits(env[[name]], "sf") || inherits(env[[name]], "sfc"),
      loaded
    )
    if (!length(candidates)) stop("The R data file does not contain an sf or sfc object.", call. = FALSE)
    return(normalize_aoi(env[[candidates[[1L]]]]))
  }

  if (ext == "zip") {
    target <- tempfile("usgs-lidar-aoi-")
    dir.create(target)
    utils::unzip(path, exdir = target)
    candidates <- list.files(
      target,
      pattern = "\\.(shp|gpkg|geojson|json|kml)$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (!length(candidates)) stop("The ZIP file does not contain a supported spatial dataset.", call. = FALSE)
    return(normalize_aoi(sf::st_read(candidates[[1L]], quiet = TRUE)))
  }

  if (ext %in% c("gpkg", "geojson", "json", "kml", "shp")) {
    return(normalize_aoi(sf::st_read(path, quiet = TRUE)))
  }

  stop("Supported AOI files: RDS, RData, GeoPackage, GeoJSON, KML, or a zipped shapefile.", call. = FALSE)
}

aoi_bbox_string <- function(aoi) {
  bbox <- sf::st_bbox(normalize_aoi(aoi))
  paste(format(as.numeric(bbox[c("xmin", "ymin", "xmax", "ymax")]), scientific = FALSE, trim = TRUE), collapse = ",")
}

bounding_boxes_to_sf <- function(items) {
  geometries <- lapply(items, function(item) {
    box <- item$boundingBox
    if (is.null(box)) return(sf::st_geometrycollection())
    sf::st_as_sfc(sf::st_bbox(c(
      xmin = as.numeric(box$minX), ymin = as.numeric(box$minY),
      xmax = as.numeric(box$maxX), ymax = as.numeric(box$maxY)
    ), crs = 4326))[[1L]]
  })
  sf::st_sfc(geometries, crs = 4326)
}
