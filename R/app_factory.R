create_usgs_lidar_app <- function(initial_aoi = NULL) {
  assert_packages()
  if (!is.null(initial_aoi)) initial_aoi <- normalize_aoi(initial_aoi)
  default_download_dir <- Sys.getenv(
    "USGS_LIDAR_DEFAULT_DOWNLOAD_DIR",
    unset = normalizePath(getwd(), winslash = "/")
  )

  add_aoi_to_map <- function(map, x) {
    types <- unique(as.character(sf::st_geometry_type(x)))
    if (all(types %in% c("POINT", "MULTIPOINT"))) {
      leaflet::addCircleMarkers(
        map, data = x, radius = 7, color = "#c63c2f", weight = 3,
        fillColor = "#ffffff", fillOpacity = 0.9, group = "AOI"
      )
    } else {
      leaflet::addPolygons(
        map, data = x, color = "#c63c2f", weight = 3,
        fillOpacity = 0.05, group = "AOI"
      )
    }
  }

  fit_aoi <- function(map, x) {
    bbox <- sf::st_bbox(x)
    if (bbox[["xmin"]] == bbox[["xmax"]]) {
      bbox[c("xmin", "xmax")] <- bbox[["xmin"]] + c(-0.02, 0.02)
    }
    if (bbox[["ymin"]] == bbox[["ymax"]]) {
      bbox[c("ymin", "ymax")] <- bbox[["ymin"]] + c(-0.02, 0.02)
    }
    leaflet::fitBounds(map, bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
  }

  css <- "
    body { background: #f4f7f4; }
    .navbar-default { background: #173f35; border-color: #173f35; }
    .navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a { color: white; }
    .well { background: white; border: 0; box-shadow: 0 2px 12px rgba(20,55,45,.09); }
    .btn-primary { background: #177245; border-color: #177245; }
    .btn-success { background: #b75b22; border-color: #b75b22; }
    .metric { display:inline-block; margin: 0 18px 8px 0; }
    .metric b { display:block; color:#173f35; font-size:1.35em; }
    .note { color:#5e6b65; font-size:.92em; }
    #map { border-radius: 8px; box-shadow: 0 2px 12px rgba(20,55,45,.12); }
    #terrain_map { border-radius: 8px; box-shadow: 0 2px 12px rgba(20,55,45,.12); }
    .aoi-tools { display:flex; flex-wrap:wrap; gap:6px; margin-bottom:12px; }
    .aoi-tools .btn { color:#1765b3; border:1px solid #81b2ed; background:white; }
    .aoi-tools .btn:hover { background:#eaf4ff; }
    #aoi_file_container { display:none; }
    .aoi-hint { margin-top:5px; }
    #map .leaflet-draw-toolbar { display:none; }
  "

  ui <- shiny::navbarPage(
    title = "USGS 3DEP Lidar Explorer",
    header = shiny::tags$head(
      shiny::tags$style(shiny::HTML(css)),
      shiny::tags$script(shiny::HTML("$(function() {
        Shiny.addCustomMessageHandler('startLidarDraw', function(mode) {
          var tool = document.querySelector('#map .leaflet-draw-draw-' + mode);
          if (tool) tool.click();
        });
        $(document).on('click', '#upload_aoi', function() { $('#aoi_file').click(); });
      });"))
    ),
    shiny::tabPanel(
      "Explore & download",
      shiny::fluidPage(
        shiny::br(),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            width = 4,
            shiny::h4("1. Area of interest"),
            shiny::div(class = "aoi-tools",
              shiny::actionButton("draw_extent", "Extent", icon = shiny::icon("square")),
              shiny::actionButton("draw_polygon", "Polygon", icon = shiny::icon("draw-polygon")),
              shiny::actionButton("draw_point", "Point", icon = shiny::icon("map-marker-alt")),
              shiny::actionButton("enter_coords", "Enter coords", icon = shiny::icon("keyboard")),
              shiny::actionButton("clear_aoi", "Clear", icon = shiny::icon("times")),
              shiny::actionButton("upload_aoi", "Upload shapefile", icon = shiny::icon("file-upload"))
            ),
            shiny::div(id = "aoi_file_container", shiny::fileInput("aoi_file", NULL,
              accept = c(".zip", ".gpkg", ".geojson", ".json", ".kml", ".rds", ".rda", ".RData"))),
            shiny::p(class = "note aoi-hint", "Draw on the map, enter coordinates, or upload a zipped shapefile. Maximum polygon area: 4,000 ha. R users can also pass an sf object."),
            shiny::actionButton("find_projects", "Find overlapping projects", class = "btn-primary"),
            shiny::hr(),
            shiny::h4("2. Projects"),
            shiny::uiOutput("project_picker"),
            shiny::actionButton("find_tiles", "Find tiles in selected projects", class = "btn-primary"),
            shiny::hr(),
            shiny::h4("3. Tiles and destination"),
            shiny::uiOutput("tile_picker"),
            shiny::fluidRow(
              shiny::column(9, shiny::textInput("destination", "Download folder", value = default_download_dir)),
              shiny::column(3, shiny::br(), shiny::actionButton("browse", "Browse"))
            ),
            shiny::fluidRow(
              shiny::column(6, shiny::numericInput("workers", "Parallel downloads", default_workers(), min = 1, max = max_workers(), step = 1)),
              shiny::column(6, shiny::numericInput("retries", "Retries", 5, min = 1, max = 20))
            ),
            shiny::actionButton("download", "Download selected tiles", class = "btn-success"),
            shiny::actionButton("validate", "Validate downloaded LAS/LAZ"),
            shiny::br(), shiny::br(),
            shiny::uiOutput("status")
          ),
          shiny::mainPanel(
            width = 8,
            shiny::uiOutput("metrics"),
            leaflet::leafletOutput("map", height = "650px"),
            shiny::br(),
            shiny::tabsetPanel(
              shiny::tabPanel("Projects", shiny::tableOutput("project_table")),
              shiny::tabPanel("Tiles", shiny::tableOutput("tile_table")),
              shiny::tabPanel("Download results", shiny::tableOutput("download_table")),
              shiny::tabPanel("LAS/LAZ validation", shiny::tableOutput("validation_table"))
            )
          )
        )
      )
    ),
    shiny::tabPanel(
      "Terrain & canopy",
      shiny::fluidPage(
        shiny::br(),
        shiny::sidebarLayout(
          shiny::sidebarPanel(width = 4,
            shiny::h4("Generate from downloaded LAS/LAZ"),
            shiny::textInput("lidar_folder", "LAS/LAZ folder", value = default_download_dir),
            shiny::actionButton("use_download_folder", "Use download folder"),
            shiny::actionButton("scan_lidar", "Find LAS/LAZ files"),
            shiny::uiOutput("las_picker"),
            shiny::h5("Catalog processing"),
            shiny::numericInput("dem_res", "Resolution (m)", 0.5, min = 0.1, max = 50, step = 0.1),
            shiny::fluidRow(
              shiny::column(4, shiny::numericInput("catalog_workers", "Workers", default_workers(), min = 1, max = max_workers(), step = 1)),
              shiny::column(4, shiny::numericInput("catalog_chunk", "Chunk (m)", 250, min = 250, max = 2000, step = 50)),
              shiny::column(4, shiny::numericInput("catalog_buffer", "Buffer (m)", 20, min = 0, max = 100, step = 5))
            ),
            shiny::fluidRow(
              shiny::column(4, shiny::selectInput("ground_algo", "Ground classification", GROUND_ALGORITHMS, selected = "none")),
              shiny::column(4, shiny::selectInput("dtm_algo", "DEM algorithm", DTM_ALGORITHMS, selected = "tin")),
              shiny::column(4, shiny::selectInput("chm_algo", "CHM algorithm", CHM_ALGORITHMS, selected = "pitfree"))
            ),
            shiny::p(class = "note", "Tiles are checked for ground-classified points. Ground is classified (with the chosen algorithm's defaults) only if a tile lacks it; with 'none', such tiles raise an error."),
            shiny::p(class = "note", "Selected tiles are processed as one seamless catalog. More workers can use substantially more RAM."),
            shiny::textInput("raster_folder", "Generated-file folder", value = file.path(default_download_dir, "generated")),
            shiny::actionButton("generate_rasters", "Generate DEM & CHM", class = "btn-primary"),
            shiny::br(), shiny::br(),
            shiny::downloadButton("download_generated", "Export DEM & CHM (GeoTIFF)"),
            shiny::br(), shiny::br(),
            shiny::uiOutput("generation_status"),
            shiny::p(class = "note", "Ground is classified (when needed) and heights are normalized with TIN before DEM/CHM generation. Processing large areas may take several minutes and substantial memory.")
          ),
          shiny::mainPanel(width = 8,
            shiny::radioButtons("terrain_layer", "Display", choices = c("DEM", "CHM"), selected = "DEM", inline = TRUE),
            leaflet::leafletOutput("terrain_map", height = "650px"),
            shiny::p(class = "note", "Colors use viridis magma (dark = low, light = high). GeoTIFFs retain the native LAS/LAZ coordinate system; the map preview is reprojected and may be downsampled."),
            shiny::tableOutput("generated_table")
          )
        )
      )
    ),
    shiny::tabPanel(
      "Tree segmentation",
      shiny::fluidPage(
        shiny::br(),
        shiny::sidebarLayout(
          shiny::sidebarPanel(width = 4,
            shiny::h4("Individual tree segmentation"),
            shiny::p(class = "note", "Uses the LAS/LAZ folder and file list from the Terrain & canopy tab."),
            shiny::actionButton("scan_lidar_its", "Find LAS/LAZ files"),
            shiny::uiOutput("las_picker_its"),
            shiny::numericInput("its_res", "CHM resolution for tree tops (m)", 0.5, min = 0.1, max = 50, step = 0.1),
            shiny::fluidRow(
              shiny::column(4, shiny::numericInput("its_workers", "Workers", default_workers(), min = 1, max = max_workers(), step = 1)),
              shiny::column(4, shiny::numericInput("its_chunk", "Chunk (m)", 250, min = 250, max = 2000, step = 50)),
              shiny::column(4, shiny::numericInput("its_buffer", "Buffer (m)", 20, min = 0, max = 100, step = 5))
            ),
            shiny::fluidRow(
              shiny::column(6, shiny::selectInput("its_ground_algo", "Ground classification", GROUND_ALGORITHMS, selected = "none")),
              shiny::column(6, shiny::selectInput("its_algo", "ITS algorithm", ITS_ALGORITHMS, selected = "li2012"))
            ),
            shiny::conditionalPanel("input.its_algo != 'li2012'",
              shiny::numericInput("its_window", "Tree-top window (m)", 5, min = 1, max = 30, step = 0.5)),
            shiny::textInput("its_folder", "Generated-file folder", value = file.path(default_download_dir, "generated")),
            shiny::actionButton("generate_its", "Segment trees", class = "btn-primary"),
            shiny::br(), shiny::br(),
            shiny::downloadButton("download_its", "Export trees (GeoPackage)"),
            shiny::br(), shiny::br(),
            shiny::uiOutput("its_status"),
            shiny::p(class = "note", "Ground is classified only if a tile lacks it, and heights are normalized with TIN before segmentation. Output is a GeoPackage with tree_points and tree_crowns (convex hull) layers holding ht, cr_area, cr_volume, x_max, y_max, x_bray and y_bray.")
          ),
          shiny::mainPanel(width = 8,
            leaflet::leafletOutput("its_map", height = "650px"),
            shiny::p(class = "note", "Crowns and points are colored by height with viridis magma (dark = low, light = high). The GeoPackage keeps the native LAS/LAZ coordinate system; the map is reprojected for display."),
            shiny::tableOutput("its_table")
          )
        )
      )
    ),
    shiny::tabPanel(
      "About & sources",
      shiny::fluidPage(
        shiny::br(),
        shiny::div(class = "well",
          shiny::h3("What this app does"),
          shiny::p("The app queries the USGS 3DEP lidar project index, preserves every overlapping project so you can choose among vintages, queries tile-level TNM products for the exact area of interest, and downloads selected LAS/LAZ files."),
          shiny::p("Downloads use partial files, resume when supported, retry transient failures, skip complete files, preserve the USGS LAZ/LAS subfolder, and record failed URLs."),
          shiny::h4("Authoritative references"),
          shiny::tags$ul(
            shiny::tags$li(shiny::tags$a(href = paste0(USGS_INDEX_ROOT, "/"), target = "_blank", "3DEP Elevation Index map service")),
            shiny::tags$li(shiny::tags$a(href = "https://apps.nationalmap.gov/lidar-explorer/#/", target = "_blank", "USGS Lidar Explorer")),
            shiny::tags$li(shiny::tags$a(href = "https://coast.noaa.gov/inventory/", target = "_blank", "NOAA Interagency Elevation Inventory")),
            shiny::tags$li(shiny::tags$a(href = "https://tnmaccess.nationalmap.gov/api/v1/", target = "_blank", "The National Map Access API"))
          ),
          shiny::p(class = "note", "This is a local downloader. The destination folder is on the computer running R/Shiny, not necessarily the web browser's computer.")
        )
      )
    )
  )

  server <- function(input, output, session) {
    aoi <- shiny::reactiveVal(initial_aoi)
    projects <- shiny::reactiveVal(NULL)
    tiles <- shiny::reactiveVal(NULL)
    download_results <- shiny::reactiveVal(NULL)
    validation <- shiny::reactiveVal(NULL)
    message <- shiny::reactiveVal(if (is.null(initial_aoi)) "Load an area of interest to begin." else "Area of interest supplied from R.")
    available_lidar <- shiny::reactiveVal(character())
    generated <- shiny::reactiveVal(NULL)
    its_result <- shiny::reactiveVal(NULL)
    its_message <- shiny::reactiveVal("Choose downloaded LAS/LAZ files to segment individual trees.")
    generation_message <- shiny::reactiveVal("Choose downloaded LAS/LAZ files to generate terrain and canopy rasters.")

    set_aoi <- function(value, source) {
      aoi(value); projects(NULL); tiles(NULL)
      message(sprintf("%s (%s).", source, if (aoi_area_ha(value) > 0) sprintf("%.1f ha", aoi_area_ha(value)) else "point"))
      refresh_map()
    }

    shiny::observeEvent(input$draw_extent, session$sendCustomMessage("startLidarDraw", "rectangle"))
    shiny::observeEvent(input$draw_polygon, session$sendCustomMessage("startLidarDraw", "polygon"))
    shiny::observeEvent(input$draw_point, session$sendCustomMessage("startLidarDraw", "marker"))
    shiny::observeEvent(input$map_draw_new_feature, {
      tryCatch(set_aoi(aoi_from_geojson(input$map_draw_new_feature), "Map geometry selected"),
        error = function(e) { message(paste("AOI error:", conditionMessage(e))); refresh_map() })
    })
    shiny::observeEvent(input$clear_aoi, {
      aoi(NULL); projects(NULL); tiles(NULL)
      message("Area of interest cleared.")
      refresh_map()
    })
    shiny::observeEvent(input$enter_coords, {
      shiny::showModal(shiny::modalDialog(
        title = "Enter coordinates (decimal degrees, WGS84)",
        shiny::radioButtons("coord_mode", NULL, c("Point", "Extent"), inline = TRUE),
        shiny::conditionalPanel("input.coord_mode === 'Point'",
          shiny::numericInput("coord_lon", "Longitude", -112.1, min = -180, max = 180),
          shiny::numericInput("coord_lat", "Latitude", 33.4, min = -90, max = 90)),
        shiny::conditionalPanel("input.coord_mode === 'Extent'",
          shiny::fluidRow(shiny::column(6, shiny::numericInput("coord_west", "West", -112.2)),
                          shiny::column(6, shiny::numericInput("coord_east", "East", -112.1))),
          shiny::fluidRow(shiny::column(6, shiny::numericInput("coord_south", "South", 33.3)),
                          shiny::column(6, shiny::numericInput("coord_north", "North", 33.4)))),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("apply_coords", "Use coordinates", class = "btn-primary")),
        easyClose = TRUE
      ))
    })
    shiny::observeEvent(input$apply_coords, {
      tryCatch({
        value <- if (identical(input$coord_mode, "Point")) aoi_from_point(input$coord_lon, input$coord_lat) else
          aoi_from_bounds(input$coord_west, input$coord_south, input$coord_east, input$coord_north)
        set_aoi(value, "Coordinates selected")
        shiny::removeModal()
      }, error = function(e) message(paste("AOI error:", conditionMessage(e))))
    })

    shiny::observeEvent(input$aoi_file, {
      tryCatch({
        loaded <- read_aoi_file(input$aoi_file$datapath, input$aoi_file$name)
        set_aoi(loaded, paste("Loaded", input$aoi_file$name))
      }, error = function(e) message(paste("AOI error:", conditionMessage(e))))
    })

    output$map <- leaflet::renderLeaflet({
      map <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
        leaflet::addProviderTiles(leaflet::providers$Esri.NatGeoWorldMap, group = "Basemap") |>
        leaflet::addWMSTiles(
          baseUrl = paste0(USGS_INDEX_ROOT, "/WMSServer?"),
          layers = as.character(USGS_LIDAR_DISPLAY_LAYER),
          options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE),
          attribution = "USGS 3DEP", group = "USGS lidar availability"
        ) |>
        leaflet::addLayersControl(
          overlayGroups = c("USGS lidar availability", "AOI", "Projects", "Tiles"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet.extras::addDrawToolbar(
          targetGroup = "draw", position = "topleft",
          polylineOptions = FALSE, circleOptions = FALSE, circleMarkerOptions = FALSE,
          rectangleOptions = leaflet.extras::drawRectangleOptions(showArea = TRUE),
          polygonOptions = leaflet.extras::drawPolygonOptions(showArea = TRUE),
          markerOptions = leaflet.extras::drawMarkerOptions(),
          editOptions = leaflet.extras::editToolbarOptions(edit = FALSE, remove = FALSE),
          singleFeature = TRUE
        )
      if (!is.null(aoi())) {
        map <- fit_aoi(add_aoi_to_map(map, aoi()), aoi())
      }
      map
    })

    refresh_map <- function(selected_keys = NULL) {
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup("AOI") |>
        leaflet::clearGroup("draw") |>
        leaflet::clearGroup("Projects") |>
        leaflet::clearGroup("Tiles")
      if (!is.null(aoi())) {
        proxy <- add_aoi_to_map(proxy, aoi())
      }
      shown <- projects()
      if (!is.null(shown) && !is.null(selected_keys)) {
        shown <- shown[shown$project_key %in% selected_keys, , drop = FALSE]
      }
      if (!is.null(shown) && nrow(shown)) {
        proxy <- proxy |> leaflet::addPolygons(
          data = shown, color = "#177245", weight = 2, fillColor = "#55a868", fillOpacity = 0.18,
          label = ~paste0(project, " Ã¢â‚¬â€ ", ql), group = "Projects"
        )
      }
      if (!is.null(tiles()) && nrow(tiles())) {
        proxy <- proxy |> leaflet::addPolygons(
          data = tiles(), color = "#b75b22", weight = 1, fillColor = "#e69f62", fillOpacity = 0.20,
          label = ~paste0(title, " (", size, ")"), group = "Tiles"
        )
      }
      invisible(proxy)
    }

    shiny::observeEvent(input$find_projects, {
      if (is.null(aoi())) { message("Load or supply an sf area of interest first."); return() }
      message("Querying the USGS 3DEP project indexÃ¢â‚¬Â¦")
      tryCatch({
        found <- shiny::withProgress(message = "Querying USGS projects", value = 0.4, query_usgs_projects(aoi()))
        projects(found); tiles(NULL)
        message(if (nrow(found)) paste("Found", nrow(found), "overlapping project(s).") else "No overlapping USGS lidar projects were found.")
        refresh_map()
      }, error = function(e) message(paste("Project query failed:", conditionMessage(e))))
    })

    output$project_picker <- shiny::renderUI({
      x <- projects()
      if (is.null(x) || !nrow(x)) return(shiny::helpText("No project results yet."))
      labels <- sprintf("%s | %s | %s to %s", x$project, x$ql, x$collect_start_display, x$collect_end_display)
      shiny::checkboxGroupInput("project_ids", "Choose one or more overlapping projects", choices = stats::setNames(x$project_key, labels), selected = x$project_key)
    })

    shiny::observeEvent(input$project_ids, {
      if (is.null(projects())) return()
      refresh_map(if (is.null(input$project_ids)) character() else input$project_ids)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    shiny::observeEvent(input$find_tiles, {
      x <- projects()
      selected <- input$project_ids
      if (is.null(x) || !length(selected)) { message("Select at least one project first."); return() }
      chosen <- x[x$project_key %in% selected, , drop = FALSE]
      message("Querying tile-level products from The National MapÃ¢â‚¬Â¦")
      tryCatch({
        found <- shiny::withProgress(message = "Querying USGS lidar tiles", value = 0.4, query_tnm_tiles(aoi(), chosen))
        tiles(found)
        message(if (nrow(found)) paste("Found", nrow(found), "intersecting tile(s).") else "No tile records matched the selected project(s).")
        refresh_map(selected)
      }, error = function(e) message(paste("Tile query failed:", conditionMessage(e))))
    })

    output$tile_picker <- shiny::renderUI({
      x <- tiles()
      if (is.null(x) || !nrow(x)) return(shiny::helpText("No tile results yet."))
      labels <- sprintf("%s | %s | %s", x$project, x$size, basename(x$download_url))
      shiny::selectizeInput(
        "tile_ids", "Choose tiles", choices = stats::setNames(x$tile_id, labels), selected = x$tile_id,
        multiple = TRUE, options = list(plugins = list("remove_button"), maxOptions = 20000)
      )
    })

    shiny::observeEvent(input$browse, {
      if (.Platform$OS.type != "windows") { message("Paste a destination path into the download-folder field."); return() }
      chosen <- utils::choose.dir(default = input$destination, caption = "Choose lidar download folder")
      if (!is.na(chosen)) shiny::updateTextInput(session, "destination", value = normalizePath(chosen, winslash = "/"))
    })

    shiny::observeEvent(input$download, {
      x <- tiles(); selected <- input$tile_ids
      if (is.null(x) || !length(selected)) { message("Select at least one tile to download."); return() }
      selected_tiles <- x[x$tile_id %in% selected, , drop = FALSE]
      selected_projects <- projects()[projects()$project %in% selected_tiles$project, , drop = FALSE]
      message(paste("Downloading", nrow(selected_tiles), "tile(s)Ã¢â‚¬Â¦"))
      tryCatch({
        result <- shiny::withProgress(
          message = paste("Downloading", nrow(selected_tiles), "tile(s) in parallel"), value = 0.5,
          download_tiles(selected_tiles, selected_projects, input$destination, input$workers, input$retries)
        )
        download_results(result)
        available_lidar(lidar_files_in_folder(input$destination))
        shiny::updateTextInput(session, "lidar_folder", value = input$destination)
        shiny::updateTextInput(session, "raster_folder", value = file.path(input$destination, "generated"))
        counts <- table(factor(result$status, levels = c("downloaded", "skipped", "failed")))
        message(sprintf("Finished: %d downloaded, %d already complete, %d failed.", counts[[1]], counts[[2]], counts[[3]]))
      }, error = function(e) message(paste("Download failed:", conditionMessage(e))))
    })

    shiny::observeEvent(input$validate, {
      result <- download_results()
      if (is.null(result) || !nrow(result)) { message("Download tiles before running validation."); return() }
      message("Reading LAS/LAZ headers with rlas and building a lidR catalogÃ¢â‚¬Â¦")
      tryCatch({
        checked <- shiny::withProgress(message = "Validating lidar files", value = 0.4, {
          catalog <- open_lidr_catalog(input$destination)
          attr(catalog, "source_folder") <- input$destination
          inspect_las_files(result$path)
        })
        validation(checked)
        message(paste("Validated", nrow(checked), "LAS/LAZ file(s) with rlas; lidR catalog opened successfully."))
      }, error = function(e) message(paste("Validation failed:", conditionMessage(e))))
    })

    shiny::observeEvent(input$use_download_folder, {
      shiny::updateTextInput(session, "lidar_folder", value = input$destination)
      shiny::updateTextInput(session, "raster_folder", value = file.path(input$destination, "generated"))
      available_lidar(lidar_files_in_folder(input$destination))
      generation_message(sprintf("Found %d LAS/LAZ file(s).", length(available_lidar())))
    })
    shiny::observeEvent(input$scan_lidar, {
      files <- lidar_files_in_folder(input$lidar_folder)
      available_lidar(files)
      generation_message(if (length(files)) sprintf("Found %d LAS/LAZ file(s). Select files below.", length(files)) else "No LAS/LAZ files found in this folder.")
    })
    output$las_picker <- shiny::renderUI({
      files <- available_lidar()
      if (!length(files)) return(shiny::helpText("Download tiles or choose a folder, then click Find LAS/LAZ files."))
      shiny::selectizeInput("selected_lidar", "LAS/LAZ files to process",
        choices = stats::setNames(files, basename(files)), selected = files[[1L]], multiple = TRUE,
        options = list(plugins = list("remove_button"), maxOptions = 20000))
    })
    shiny::observeEvent(input$scan_lidar_its, {
      files <- lidar_files_in_folder(input$lidar_folder)
      available_lidar(files)
      its_message(if (length(files)) sprintf("Found %d LAS/LAZ file(s). Select files below.", length(files)) else "No LAS/LAZ files found in the Terrain & canopy tab folder.")
    })
    output$las_picker_its <- shiny::renderUI({
      files <- available_lidar()
      if (!length(files)) return(shiny::helpText("Choose a folder on the Terrain & canopy tab, then click Find LAS/LAZ files."))
      shiny::selectizeInput("selected_lidar_its", "LAS/LAZ files to process",
        choices = stats::setNames(files, basename(files)), selected = files[[1L]], multiple = TRUE,
        options = list(plugins = list("remove_button"), maxOptions = 20000))
    })

    run_job <- function(files, folder, title, set_message, job) {
      if (!length(files)) { set_message("Select at least one LAS/LAZ file first."); return(NULL) }
      if (!nzchar(folder)) { set_message("Enter a generated-file folder."); return(NULL) }
      set_message(sprintf("Processing %d LAS/LAZ file(s)â€¦", length(files)))
      progress <- shiny::Progress$new(session, min = 0, max = 1)
      progress$set(value = 0, message = title, detail = "Starting")
      on.exit(progress$close(), add = TRUE)
      tryCatch({
        result <- job(function(value, detail) progress$set(value = value, message = title, detail = detail))
        set_message(sprintf("Finished from %d LAS/LAZ file(s) as one catalog.", length(files)))
        result
      }, error = function(e) { set_message(paste("Generation failed:", conditionMessage(e))); NULL })
    }

    shiny::observeEvent(input$generate_rasters, {
      generated(NULL)
      generated(run_job(input$selected_lidar, trimws(input$raster_folder %||% ""), "Generating DEM and CHM", generation_message,
        function(progress) process_dem_chm(input$selected_lidar, trimws(input$raster_folder),
          resolution = input$dem_res, dtm_algo = input$dtm_algo, chm_algo = input$chm_algo,
          ground_algo = input$ground_algo,
          workers = input$catalog_workers, chunk_size = input$catalog_chunk,
          buffer_size = input$catalog_buffer, progress = progress)))
    })
    shiny::observeEvent(input$generate_its, {
      its_result(NULL)
      its_result(run_job(input$selected_lidar_its, trimws(input$its_folder %||% ""), "Segmenting trees", its_message,
        function(progress) process_its(input$selected_lidar_its, trimws(input$its_folder),
          resolution = input$its_res, its_algo = input$its_algo, treetop_window = input$its_window %||% 5,
          ground_algo = input$its_ground_algo,
          workers = input$its_workers, chunk_size = input$its_chunk,
          buffer_size = input$its_buffer, progress = progress)))
    })
    output$generation_status <- shiny::renderUI(shiny::div(class = "alert alert-info", generation_message()))
    output$its_status <- shiny::renderUI(shiny::div(class = "alert alert-info", its_message()))

    output$terrain_map <- leaflet::renderLeaflet({
      result <- generated()
      layer <- if (identical(input$terrain_layer, "CHM")) "chm" else "dem"
      tryCatch(
        raster_preview_map(if (is.null(result)) NULL else result[[layer]],
                           if (layer == "chm") "Canopy height (m)" else "Elevation (m)"),
        error = function(e) { generation_message(paste("Preview failed:", conditionMessage(e))); raster_preview_map(NULL, "") })
    })
    output$its_map <- leaflet::renderLeaflet({
      result <- its_result()
      tryCatch(tree_preview_map(if (is.null(result)) NULL else result[["trees"]]),
        error = function(e) { its_message(paste("Preview failed:", conditionMessage(e))); tree_preview_map(NULL) })
    })
    output_table <- function(result, labels) {
      if (is.null(result)) return(NULL)
      data.frame(product = unname(labels[names(result)]), file = unname(result), row.names = NULL)
    }
    output$generated_table <- shiny::renderTable(output_table(generated(), c(dem = "DEM", chm = "CHM")), striped = TRUE)
    output$its_table <- shiny::renderTable(output_table(its_result(), c(trees = "Tree points + crowns (GeoPackage)")), striped = TRUE)
    output$download_generated <- shiny::downloadHandler(
      filename = function() paste0("usgs-lidar-dem-chm-", format(Sys.Date(), "%Y%m%d"), ".zip"),
      content = function(file) zip_outputs(generated(), file)
    )
    output$download_its <- shiny::downloadHandler(
      filename = function() paste0("usgs-lidar-its-", format(Sys.Date(), "%Y%m%d"), ".zip"),
      content = function(file) zip_outputs(its_result(), file)
    )

    output$status <- shiny::renderUI(shiny::div(class = "alert alert-info", message()))
    output$metrics <- shiny::renderUI({
      project_count <- if (is.null(projects())) 0L else nrow(projects())
      tile_count <- if (is.null(tiles())) 0L else nrow(tiles())
      total_size <- if (is.null(tiles()) || !nrow(tiles())) 0 else sum(tiles()$size_bytes, na.rm = TRUE)
      shiny::div(
        shiny::span(class = "metric", shiny::tags$b(project_count), "overlapping projects"),
        shiny::span(class = "metric", shiny::tags$b(tile_count), "intersecting tiles"),
        shiny::span(class = "metric", shiny::tags$b(format_bytes(total_size)), "available download")
      )
    })
    output$project_table <- shiny::renderTable({
      x <- projects(); if (is.null(x) || !nrow(x)) return(NULL)
      sf::st_drop_geometry(x)[, c("project", "workunit", "ql", "collect_start_display", "collect_end_display", "horiz_crs", "vert_crs", "geoid"), drop = FALSE]
    }, striped = TRUE, bordered = FALSE)
    output$tile_table <- shiny::renderTable({
      x <- tiles(); if (is.null(x) || !nrow(x)) return(NULL)
      head(sf::st_drop_geometry(x)[, c("project", "title", "format", "size", "publication_date", "manifest_verified"), drop = FALSE], 200)
    }, striped = TRUE, bordered = FALSE)
    output$download_table <- shiny::renderTable(download_results(), striped = TRUE, bordered = FALSE)
    output$validation_table <- shiny::renderTable(validation(), striped = TRUE, bordered = FALSE)
  }

  shiny::shinyApp(ui, server)
}
