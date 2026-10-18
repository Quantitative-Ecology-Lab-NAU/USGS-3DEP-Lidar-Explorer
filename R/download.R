remote_file_size <- function(url) {
  tryCatch({
    handle <- curl::new_handle(
      nobody = TRUE,
      customrequest = "HEAD",
      useragent = "USGS-3DEP-Shiny-Explorer/0.1",
      timeout = 45,
      followlocation = TRUE
    )
    response <- curl::curl_fetch_memory(url, handle = handle)
    headers <- curl::parse_headers_list(response$headers)
    value <- headers[["content-length"]]
    if (is.null(value)) NA_real_ else as.numeric(value)
  }, error = function(e) NA_real_)
}

relative_tile_path <- function(url, project_link) {
  clean_url <- utils::URLdecode(sub("[?#].*$", "", url))
  clean_project <- sub("/+$", "", utils::URLdecode(project_link %||% ""))
  if (nzchar(clean_project) && startsWith(tolower(clean_url), paste0(tolower(clean_project), "/"))) {
    relative <- substring(clean_url, nchar(clean_project) + 2L)
  } else {
    relative <- basename(clean_url)
  }
  parts <- strsplit(relative, "/", fixed = TRUE)[[1L]]
  parts <- vapply(parts[nzchar(parts)], safe_name, character(1))
  do.call(file.path, as.list(parts))
}

download_one_tile <- function(job, destination, retries = 5L) {
  project_dir <- file.path(destination, safe_name(job$project, "Unknown_project"))
  local_path <- file.path(project_dir, relative_tile_path(job$url, job$project_link))
  dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
  part_path <- paste0(local_path, ".part")
  expected <- suppressWarnings(as.numeric(job$size_bytes))
  if (!is.finite(expected)) expected <- remote_file_size(job$url)

  if (file.exists(local_path)) {
    actual <- file.info(local_path)$size
    if (!is.finite(expected) || identical(as.numeric(actual), expected)) {
      return(data.frame(status = "skipped", url = job$url, path = local_path, error = "", stringsAsFactors = FALSE))
    }
    if (!file.exists(part_path) || file.info(part_path)$size < actual) {
      if (file.exists(part_path)) unlink(part_path)
      file.rename(local_path, part_path)
    }
  }

  last_error <- NULL
  for (attempt in seq_len(retries)) {
    success <- tryCatch({
      existing <- if (file.exists(part_path)) file.info(part_path)$size else 0
      if (is.finite(expected) && existing > expected) {
        unlink(part_path)
        existing <- 0
      }

      handle <- curl::new_handle(
        useragent = "USGS-3DEP-Shiny-Explorer/0.1",
        timeout = 0,
        connecttimeout = 45,
        low_speed_limit = 1024,
        low_speed_time = 120,
        followlocation = TRUE
      )
      mode <- "wb"
      if (existing > 0) {
        curl::handle_setheaders(handle, Range = paste0("bytes=", existing, "-"))
        mode <- "ab"
      }
      curl::curl_download(job$url, part_path, mode = mode, quiet = TRUE, handle = handle)

      downloaded <- file.info(part_path)$size
      if (is.finite(expected) && downloaded != expected) {
        if (downloaded > expected) unlink(part_path)
        stop("Size mismatch: downloaded ", downloaded, " bytes; expected ", expected)
      }
      if (file.exists(local_path)) unlink(local_path)
      if (!file.rename(part_path, local_path)) stop("Could not finalize the downloaded file.")
      TRUE
    }, error = function(e) {
      last_error <<- e
      FALSE
    })

    if (success) {
      return(data.frame(status = "downloaded", url = job$url, path = local_path, error = "", stringsAsFactors = FALSE))
    }
    if (attempt < retries) Sys.sleep(min(2^attempt, 20))
  }

  data.frame(
    status = "failed", url = job$url, path = local_path,
    error = conditionMessage(last_error), stringsAsFactors = FALSE
  )
}

download_tiles <- function(tiles, projects, destination, workers = 2L, retries = 5L) {
  destination <- normalizePath(destination, winslash = "/", mustWork = FALSE)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(destination)) stop("Could not create destination folder: ", destination, call. = FALSE)

  project_links <- setNames(sub("/+$", "", projects$lpc_link), projects$project)
  jobs <- lapply(seq_len(nrow(tiles)), function(i) {
    project_name <- tiles$project[[i]]
    link_index <- match(project_name, names(project_links))
    project_link <- if (is.na(link_index)) "" else unname(project_links[[link_index]])
    list(
      url = tiles$download_url[[i]],
      size_bytes = tiles$size_bytes[[i]],
      project = project_name,
      project_link = project_link
    )
  })
  writeLines(vapply(jobs, `[[`, character(1), "url"), file.path(destination, "selected_download_links.txt"))

  worker_count <- max(1L, min(as.integer(workers), length(jobs)))
  if (worker_count == 1L) {
    results <- lapply(jobs, download_one_tile, destination = destination, retries = retries)
  } else {
    cluster <- parallel::makePSOCKcluster(worker_count)
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    parallel::clusterExport(
      cluster,
      c("download_one_tile", "remote_file_size", "relative_tile_path", "safe_name", "%||%"),
      envir = environment()
    )
    results <- parallel::parLapplyLB(
      cluster, jobs, download_one_tile,
      destination = destination, retries = retries
    )
  }

  results <- do.call(rbind, results)
  failed_path <- file.path(destination, "failed_downloads.csv")
  failed <- results[results$status == "failed", , drop = FALSE]
  if (nrow(failed)) {
    utils::write.csv(failed, failed_path, row.names = FALSE)
  } else if (file.exists(failed_path)) {
    unlink(failed_path)
  }
  results
}

inspect_las_files <- function(paths) {
  paths <- unique(paths[file.exists(paths) & grepl("\\.la[sz]$", paths, ignore.case = TRUE)])
  if (!length(paths)) return(data.frame())
  rows <- lapply(paths, function(path) {
    tryCatch({
      header <- rlas::read.lasheader(path)
      data.frame(
        file = basename(path),
        points = as.numeric(header[["Number of point records"]] %||% NA_real_),
        min_z = as.numeric(header[["Min Z"]] %||% NA_real_),
        max_z = as.numeric(header[["Max Z"]] %||% NA_real_),
        status = "Readable",
        stringsAsFactors = FALSE
      )
    }, error = function(e) data.frame(
      file = basename(path), points = NA_real_, min_z = NA_real_, max_z = NA_real_,
      status = conditionMessage(e), stringsAsFactors = FALSE
    ))
  })
  do.call(rbind, rows)
}

open_lidr_catalog <- function(destination) {
  # This deliberately uses lidR after download; it reads headers, not all points.
  lidR::readLAScatalog(destination, recursive = TRUE)
}
