source(testthat::test_path("..", "..", "R", "utils.R"))
source(testthat::test_path("..", "..", "R", "aoi.R"))
source(testthat::test_path("..", "..", "R", "download.R"))
source(testthat::test_path("..", "..", "R", "terrain.R"))

testthat::test_that("catalog defaults match the Shiny controls", {
  defaults <- formals(process_lidar_files)
  testthat::expect_equal(defaults$workers, 2L)
  testthat::expect_equal(default_workers(), min(2L, max_workers()))
  testthat::expect_equal(defaults$chunk_size, 250)
  testthat::expect_equal(defaults$buffer_size, 20)
})

testthat::test_that("AOI normalization transforms to WGS84", {
  point <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_point(c(-112, 33)), crs = 4326))
  normalized <- normalize_aoi(sf::st_transform(point, 3857))
  testthat::expect_s3_class(normalized, "sf")
  testthat::expect_equal(sf::st_crs(normalized)$epsg, 4326)
})

testthat::test_that("AOI controls accept points and cap polygon area at 4,000 ha", {
  testthat::expect_s3_class(aoi_from_point(-112, 33), "sf")
  small <- aoi_from_bounds(-112, 33, -111.98, 33.02)
  testthat::expect_lt(aoi_area_ha(small), 4000)
  testthat::expect_error(aoi_from_bounds(-112, 33, -111.9, 33.1), "4,000 ha")
  feature <- list(type = "Feature", geometry = list(type = "Point", coordinates = c(-112, 33)), properties = list())
  testthat::expect_s3_class(aoi_from_geojson(feature), "sf")
})

testthat::test_that("LAS/LAZ discovery ignores unrelated files", {
  folder <- tempfile("lidar-folder-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  file.create(file.path(folder, c("one.las", "two.LAZ", "notes.txt")))
  testthat::expect_length(lidar_files_in_folder(folder), 2L)
})

testthat::test_that("sample LAZ generates 0.5 m CHM, terrain, and hillshade", {
  sample <- system.file("extdata", "Topography.laz", package = "lidR")
  testthat::skip_if_not(nzchar(sample))
  folder <- tempfile("terrain-products-")
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  products <- process_lidar_file(sample, folder)
  testthat::expect_true(all(file.exists(products)))
  testthat::expect_equal(unname(terra::res(terra::rast(products[["chm"]]))), c(0.5, 0.5))
  testthat::expect_equal(unname(terra::res(terra::rast(products[["dtm"]]))), c(0.5, 0.5))
})

testthat::test_that("two selected tiles use a buffered catalog and produce one raster set", {
  sample <- system.file("extdata", "Topography.laz", package = "lidR")
  testthat::skip_if_not(nzchar(sample))
  folder <- tempfile("terrain-catalog-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  las <- lidR::readLAS(sample)
  x <- range(las@data[["X"]])
  y <- range(las@data[["Y"]])
  midpoint <- mean(x)
  files <- file.path(folder, c("west.las", "east.las"))
  lidR::writeLAS(lidR::clip_rectangle(las, x[[1L]], y[[1L]], midpoint, y[[2L]]), files[[1L]])
  lidR::writeLAS(lidR::clip_rectangle(las, midpoint, y[[1L]], x[[2L]], y[[2L]]), files[[2L]])
  result <- process_lidar_files(files, file.path(folder, "generated"),
                                workers = 1, chunk_size = 250, buffer_size = 30)
  testthat::expect_length(result, 1L)
  testthat::expect_true(all(file.exists(unlist(result))))
  testthat::expect_equal(unname(terra::res(terra::rast(result[[1L]][["chm"]]))), c(0.5, 0.5))
  testthat::expect_false(any(grepl("catalog-work", list.files(file.path(folder, "generated"), all.files = TRUE))))
})

testthat::test_that("USGS project-relative paths preserve subfolders", {
  root <- "https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/Projects/AZ_Test_B20/AZ_Test"
  url <- paste0(root, "/LAZ/USGS_LPC_AZ_Test_w0001n0001.laz")
  expected <- file.path("LAZ", "USGS_LPC_AZ_Test_w0001n0001.laz")
  testthat::expect_equal(relative_tile_path(url, root), expected)
})

testthat::test_that("unsafe destination names are sanitized", {
  testthat::expect_equal(safe_name("A:B/C*D"), "A_B_C_D")
})
