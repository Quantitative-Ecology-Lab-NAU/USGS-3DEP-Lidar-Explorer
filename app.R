source(file.path("R", "utils.R"), local = TRUE)
source(file.path("R", "aoi.R"), local = TRUE)
source(file.path("R", "usgs_api.R"), local = TRUE)
source(file.path("R", "download.R"), local = TRUE)
source(file.path("R", "terrain.R"), local = TRUE)
source(file.path("R", "products.R"), local = TRUE)
source(file.path("R", "app_factory.R"), local = TRUE)

create_usgs_lidar_app(getOption("usgs.lidar.aoi", NULL))
