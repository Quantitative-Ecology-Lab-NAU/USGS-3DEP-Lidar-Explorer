`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}

max_workers <- function() max(1L, as.integer(parallelly::availableCores()))

# Default of 2 workers, capped at the cores available.
default_workers <- function() min(2L, max_workers())

assert_packages <- function() {
  required <- c("shiny", "sf", "leaflet", "leaflet.extras", "curl", "jsonlite", "rlas", "lidR", "terra", "viridisLite", "zip", "future", "parallelly")
  is_missing <- function(pkgs) pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  missing <- is_missing(required)
  if (length(missing)) {
    message("Installing missing R packages: ", paste(missing, collapse = ", "))
    repos <- getOption("repos")
    if (is.null(repos) || identical(unname(repos[["CRAN"]]), "@CRAN@") || !"CRAN" %in% names(repos)) {
      repos <- c(CRAN = "https://cloud.r-project.org")
    }
    tryCatch(utils::install.packages(missing, repos = repos, dependencies = NA),
             error = function(e) message("Package installation failed: ", conditionMessage(e)))
    missing <- is_missing(required)
  }
  if (length(missing)) {
    stop(
      "Could not install the required R packages: ", paste(missing, collapse = ", "),
      ". Try manually: ", paste(sprintf("install.packages('%s')", missing), collapse = "; "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

safe_name <- function(x, fallback = "item") {
  x <- trimws(as.character(x %||% fallback))
  x <- gsub("[<>:\"/\\\\|?*]+", "_", x)
  x <- gsub("[[:cntrl:]]+", "_", x)
  x <- sub("[. ]+$", "", x)
  if (!nzchar(x)) fallback else x
}

format_bytes <- function(x) {
  x <- as.numeric(x)
  if (!is.finite(x)) return("unknown")
  units <- c("B", "KB", "MB", "GB", "TB")
  i <- min(floor(log(max(x, 1), 1024)) + 1L, length(units))
  sprintf(if (i == 1L) "%.0f %s" else "%.1f %s", x / 1024^(i - 1L), units[i])
}

format_arcgis_date <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) return(NA_character_)
  if (inherits(x, "Date")) return(as.character(x))
  if (inherits(x, "POSIXt")) return(as.character(as.Date(x, tz = "UTC")))
  as.character(as.Date(as.POSIXct(as.numeric(x) / 1000, origin = "1970-01-01", tz = "UTC")))
}

http_request <- function(url, fields = NULL, method = c("GET", "POST"), retries = 4L,
                         timeout = 120) {
  method <- match.arg(method)
  last_error <- NULL

  for (attempt in seq_len(retries)) {
    result <- tryCatch({
      handle <- curl::new_handle(
        useragent = "USGS-3DEP-Shiny-Explorer/0.1",
        timeout = timeout,
        connecttimeout = 30
      )

      request_url <- url
      if (!is.null(fields)) {
        encoded <- paste(
          vapply(names(fields), function(name) {
            paste0(curl::curl_escape(name), "=", curl::curl_escape(as.character(fields[[name]])))
          }, character(1)),
          collapse = "&"
        )
        if (method == "POST") {
          curl::handle_setopt(
            handle,
            customrequest = "POST",
            postfields = encoded,
            httpheader = c("Content-Type" = "application/x-www-form-urlencoded")
          )
        } else {
          request_url <- paste0(url, if (grepl("?", url, fixed = TRUE)) "&" else "?", encoded)
        }
      }

      response <- curl::curl_fetch_memory(request_url, handle = handle)
      if (response$status_code < 200L || response$status_code >= 300L) {
        stop("HTTP ", response$status_code, " from ", url)
      }
      rawToChar(response$content)
    }, error = function(e) {
      last_error <<- e
      NULL
    })

    if (!is.null(result)) return(result)
    if (attempt < retries) Sys.sleep(min(2^attempt, 10))
  }

  stop("Request failed after ", retries, " attempts: ", conditionMessage(last_error), call. = FALSE)
}

read_json_response <- function(text) {
  parsed <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  error <- parsed[["error", exact = TRUE]]
  if (!is.null(error)) {
    details <- unlist(error$details %||% character())
    stop(
      error$message %||% "Remote service returned an error",
      if (length(details)) paste0(": ", paste(details, collapse = "; ")) else "",
      call. = FALSE
    )
  }
  parsed
}
