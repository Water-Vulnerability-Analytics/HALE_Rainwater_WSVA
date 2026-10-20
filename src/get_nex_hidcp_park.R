#' Pull NEX-HIDCP-CMIP6 daily climate data for a park
#'
#' Downloads NASA's NEX-HIDCP-CMIP6 daily downscaled projections (250 m,
#' statewide Hawaii) and keeps only the grid cells that touch a park boundary.
#' Output is a hive-partitioned parquet dataset that reads as one table with
#' `arrow::open_dataset(output_dir)`.
#'
#' Why whole files are downloaded: every NetCDF stores each day as a single
#' compressed chunk covering the whole state (`_ChunkSizes = 1, 1520, 2288`),
#' and neither NASA's portal nor the S3 bucket offers server-side subsetting.
#' Reading even one pixel means fetching every chunk, i.e. the whole ~300 MB
#' file. To keep that fast and small on disk, each worker streams one file at a
#' time: download -> read the park's lat/lon window -> write parquet -> delete
#' the NetCDF. Disk use stays near `workers` x 300 MB, and a rerun skips files
#' that already have output, so an interrupted pull resumes where it stopped.
#'
#' Output layout (read it all with `arrow::open_dataset(output_dir)`):
#' - `variable=<var>/scenario=<scen>/gcm=<gcm>/<source file>.parquet`, with
#'   columns `cell_id`, `date`, `year`, `month`, `day` and `value`. `value` is
#'   precipitation in mm/day (`pr`) or temperature in degC (`tasmax`, `tasmin`),
#'   stored as float32 like the source.
#' - `_cells.parquet`: one row per grid cell with `cell_id`, cell-center `lon`
#'   and `lat`, their NetCDF indices, and `coverage_fraction` (share of the cell
#'   inside the park, for area-weighted park means).
#'
#' Dates follow each GCM's own calendar. KACE-1-0-G, UKESM1-0-LL and both
#' HadGEM3 models use 360-day years, so `date` is `NA` on their Feb 29
#' (non-leap years) and Feb 30; `year`/`month`/`day` are always filled. Thirteen
#' GCMs (CanESM5, GFDL-ESM4, NorESM2, ...) use 365-day years with no Feb 29.
#'
#' The bucket holds 34,820 files (10 TB). About 170 GCM/scenario/variable
#' series are each missing one year, on NASA's portal as well, so a year-count
#' check is worth doing before computing annual statistics.
#'
#' @param park The 4 letter park code, used with `getParkBoundary()`.
#' @param output_dir Folder for the parquet dataset, e.g.
#'   `here::here("hi_datasets", "HALE")`. Use a local disk, not OneDrive: the
#'   output for the full set is large.
#' @param park_boundary Optional sf polygon(s) to use instead of downloading
#'   the boundary with `getParkBoundary(park)`.
#' @param variables Any of `"pr"`, `"tasmax"`, `"tasmin"`.
#' @param scenarios Any of `"historical"` (1950-2014), `"ssp126"`, `"ssp245"`,
#'   `"ssp370"`, `"ssp585"` (2015-2100). Defaults to the futures only: for the
#'   historical period, pull the observations every GCM was bias-corrected to
#'   with `get_hcdp_obs_park()` instead of 30 models' hindcasts.
#' @param gcms GCM names, or `NULL` for all 30.
#' @param years Years to keep, or `NULL` for all. For example
#'   `c(1985:2014, 2035:2065)`.
#' @param files Optional file table from `list_nex_hidcp_files()` (filtered
#'   however you like). When given, `variables`, `scenarios`, `gcms` and `years`
#'   are ignored.
#' @param workers Number of files downloaded and processed at once.
#' @param tmp_dir Folder for in-flight NetCDF downloads. Keep it on a local disk.
#' @param overwrite If `TRUE`, re-pull files that already have output.
#'
#' @return Invisibly, a data.table with one row per file processed in this run
#'   (`error` is `NA` for successes), or `NULL` if every file was already done.
#'
#' @examples
#' \dontrun{
#' source("setup.R")
#'
#' # Observed training data for the historical period (HCDP, 1990-2014)
#' get_hcdp_obs_park(park = "HALE", output_dir = "hi_datasets/HALE")
#'
#' # Future precipitation from all 30 GCMs
#' hale_files <- list_nex_hidcp_files(scenarios = c("ssp245", "ssp370", "ssp585"),
#'                                    variables = "pr", years = 2035:2064)
#' get_nex_hidcp_park(park = "HALE", output_dir = "hi_datasets/HALE", files = hale_files)
#'
#' # Read back: daily park-mean precipitation per GCM and scenario
#' cells <- arrow::read_parquet("hi_datasets/HALE/_cells.parquet")
#' arrow::open_dataset("hi_datasets/HALE") %>%
#'   dplyr::filter(variable == "pr") %>%
#'   dplyr::left_join(dplyr::select(cells, cell_id, coverage_fraction), by = "cell_id") %>%
#'   dplyr::group_by(gcm, scenario, date) %>%
#'   dplyr::summarize(pr_mm = sum(value * coverage_fraction) / sum(coverage_fraction)) %>%
#'   dplyr::collect()
#' }
get_nex_hidcp_park <- function(park = "HALE",
                               output_dir,
                               park_boundary = NULL,
                               variables = c("pr", "tasmax", "tasmin"),
                               scenarios = c("ssp126", "ssp245", "ssp370", "ssp585"),
                               gcms = NULL,
                               years = NULL,
                               files = NULL,
                               workers = 8,
                               tmp_dir = file.path(tempdir(), "hi_datasets"),
                               overwrite = FALSE){

  start_clock_time <- Sys.time()

  if (!requireNamespace("CFtime", quietly = TRUE)) {
    stop("get_nex_hidcp_park() needs the CFtime package to decode model calendars: install.packages(\"CFtime\")")
  }

  # Catch accidental nesting (e.g. hi_datasets/HALE/HALE) before creating anything.
  if (basename(output_dir) == basename(dirname(output_dir))) {
    stop("output_dir looks nested (", output_dir, "). Point it at the park folder itself, ",
         "e.g. here::here(\"hi_datasets\", \"", park, "\").")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  # Absolute paths, so parallel workers write to the same place whatever their
  # working directory.
  output_dir <- normalizePath(output_dir)
  tmp_dir <- normalizePath(tmp_dir)

  # A synced folder would try to upload every parquet file as it lands. The
  # folder has to exist first so normalizePath() can follow symlinks like data/.
  if (grepl("OneDrive|CloudStorage|Dropbox", normalizePath(output_dir))) {
    warning("output_dir is in a cloud-synced folder and the parquet output can be large. Consider a local folder.")
  }

  # List the files to pull ----------------------------------------------------

  if (is.null(files)) {
    files <- list_nex_hidcp_files(gcms = gcms,
                                  scenarios = scenarios,
                                  variables = variables,
                                  years = years)
  }

  # copy() so adding columns below doesn't modify the caller's table.
  files <- data.table::copy(data.table::as.data.table(files))

  if (nrow(files) == 0) {
    stop("No NEX-HIDCP-CMIP6 files match the requested gcms, scenarios, variables and years.")
  }

  missing_cols <- setdiff(c("gcm", "scenario", "variable", "url"), names(files))
  if (length(missing_cols) > 0) {
    stop("files is missing column(s): ", paste(missing_cols, collapse = ", "))
  }
  if (!"size" %in% names(files)) files[, size := NA_real_]

  # Hive-style folders let arrow::open_dataset() add variable, scenario and
  # gcm as columns without storing them in every row.
  files[, out_path := file.path(output_dir,
                                paste0("variable=", variable),
                                paste0("scenario=", scenario),
                                paste0("gcm=", gcm),
                                sub("\\.nc$", ".parquet", basename(url)))]

  todo <- if (overwrite) files else files[!file.exists(out_path)]

  message(sprintf("%s: %d of %d files to pull (%.1f GB to download); %d already done.",
                  park, nrow(todo), nrow(files), sum(todo$size, na.rm = TRUE) / 1e9,
                  nrow(files) - nrow(todo)))

  if (nrow(todo) == 0) return(invisible(NULL))

  # Park grid cells -----------------------------------------------------------

  # Every file shares one statewide grid, so the park cells are found once and
  # saved. Reruns reuse them, which keeps cell_id stable across runs.
  cells_path <- file.path(output_dir, "_cells.parquet")

  if (file.exists(cells_path)) {

    cells <- data.table::as.data.table(arrow::read_parquet(cells_path))

    if (!identical(unique(cells$park), park)) {
      stop(cells_path, " was built for ", paste(unique(cells$park), collapse = ", "),
           ", not ", park, ". Use a different output_dir.")
    }

  } else {

    if (is.null(park_boundary)) {
      park_boundary <- getParkBoundary(park = park)
    }

    # The grid comes from the first file to pull. It stays in tmp_dir, and the
    # worker that gets this file will skip downloading it again.
    first_nc <- file.path(tmp_dir, basename(todo$url[1]))
    download_nex_hidcp_file(todo$url[1], first_nc, todo$size[1])

    nc <- ncdf4::nc_open(first_nc)

    cells <- get_nex_hidcp_cells(lon = ncdf4::ncvar_get(nc, "lon"),
                                 lat = ncdf4::ncvar_get(nc, "lat"),
                                 park_boundary = park_boundary)

    # Coastal cells can touch the park but hold only ocean (fill values).
    # Read one day and drop cells without data.
    window <- nex_hidcp_window(cells)
    first_day <- ncdf4::ncvar_get(nc,
                                  todo$variable[1],
                                  start = c(window$lon_start, window$lat_start, 1),
                                  count = c(window$lon_count, window$lat_count, 1),
                                  collapse_degen = FALSE)
    ncdf4::nc_close(nc)

    cells <- cells[!is.na(as.vector(first_day)[window$window_index])]
    cells[, `:=`(cell_id = .I, park = park)]
    data.table::setcolorder(cells, c("park", "cell_id"))

    arrow::write_parquet(cells, cells_path)
    message(sprintf("Saved %d grid cells for %s to %s", nrow(cells), park, cells_path))
  }

  window <- nex_hidcp_window(cells)
  cells <- data.table::data.table(cell_id = cells$cell_id,
                                  lon_index = cells$lon_index,
                                  lat_index = cells$lat_index,
                                  lon = cells$lon,
                                  lat = cells$lat,
                                  window_index = window$window_index)
  window$window_index <- NULL

  # Download and extract in parallel ------------------------------------------

  old_plan <- future::plan()
  future::plan(future::multisession, workers = workers)
  on.exit(future::plan(old_plan), add = TRUE)

  # A small work queue rather than furrr, so each worker starts a new file the
  # moment it finishes one and progress can be reported while the run goes.
  results <- vector("list", nrow(todo))
  running <- list()
  next_row <- 1L
  n_done <- 0L
  n_failed <- 0L
  bytes_done <- 0
  last_report <- Sys.time()
  loop_start <- Sys.time()

  while (next_row <= nrow(todo) || length(running) > 0) {

    while (length(running) < workers && next_row <= nrow(todo)) {

      file_args <- list(url = todo$url[next_row],
                        size = todo$size[next_row],
                        variable = todo$variable[next_row],
                        out_path = todo$out_path[next_row],
                        cells = cells,
                        window = window,
                        tmp_dir = tmp_dir)

      running[[as.character(next_row)]] <- future::future(
        tryCatch(do.call(process_nex_hidcp_file, file_args),
                 error = function(e) {
                   data.table::data.table(url = file_args$url, error = conditionMessage(e))
                 })
      )

      next_row <- next_row + 1L
    }

    finished <- names(running)[vapply(running, future::resolved, logical(1))]

    for (id in finished) {

      row_i <- as.integer(id)

      # A crashed worker (e.g. out of memory) errors here instead of in the
      # tryCatch above; record it as a failed file and keep going.
      results[[row_i]] <- tryCatch(
        future::value(running[[id]]),
        error = function(e) data.table::data.table(url = todo$url[row_i], error = conditionMessage(e))
      )
      running[[id]] <- NULL

      n_done <- n_done + 1L

      if (!is.na(results[[row_i]]$error)) {
        n_failed <- n_failed + 1L
        message("Failed: ", basename(todo$url[row_i]), ": ", results[[row_i]]$error)
      } else {
        bytes_done <- bytes_done + max(0, todo$size[row_i], na.rm = TRUE)
      }
    }

    # Report once a minute: done, failures, throughput and time left.
    if (as.numeric(difftime(Sys.time(), last_report, units = "secs")) >= 60 ||
        n_done == nrow(todo)) {

      elapsed <- as.numeric(difftime(Sys.time(), loop_start, units = "secs"))
      message(sprintf("%s  %d/%d files (%d failed), %.0f MB/s, about %.1f h left",
                      format(Sys.time(), "%H:%M"), n_done, nrow(todo), n_failed,
                      bytes_done / 1e6 / elapsed,
                      (nrow(todo) - n_done) * elapsed / max(n_done, 1) / 3600))
      last_report <- Sys.time()
    }

    if (length(finished) == 0) Sys.sleep(0.5)
  }

  results <- data.table::rbindlist(results, fill = TRUE)
  results <- merge(todo[, .(gcm, scenario, variable, url)], results, by = "url", sort = FALSE)

  end_clock_time <- Sys.time()

  message(sprintf("Finished %d files (%d failed) in %.1f hours. Rerun to retry failures.",
                  nrow(results), sum(!is.na(results$error)),
                  as.numeric(difftime(end_clock_time, start_clock_time, units = "hours"))))

  invisible(results)
}


#' List NEX-HIDCP-CMIP6 files on NASA's public S3 bucket
#'
#' Reads the bucket listing only; nothing is downloaded. The bucket
#' (`nasa-nex-hidcp-cmip6`, AWS us-west-2) holds the same files as
#' https://data.nas.nasa.gov/nex-hidcp-cmip6.
#'
#' @param gcms GCM names, or `NULL` for every GCM in the bucket.
#' @param scenarios Any of `"historical"`, `"ssp126"`, `"ssp245"`, `"ssp370"`,
#'   `"ssp585"`.
#' @param variables Any of `"pr"`, `"tasmax"`, `"tasmin"`.
#' @param years Years to keep, or `NULL` for all.
#' @param base_url Bucket URL.
#'
#' @return A data.table with one row per NetCDF file: `gcm`, `scenario`,
#'   `member`, `variable`, `year`, `size` (bytes) and `url`.
list_nex_hidcp_files <- function(gcms = NULL,
                                 scenarios = c("historical", "ssp126", "ssp245", "ssp370", "ssp585"),
                                 variables = c("pr", "tasmax", "tasmin"),
                                 years = NULL,
                                 base_url = "https://nasa-nex-hidcp-cmip6.s3.amazonaws.com"){

  # S3 returns at most 1,000 keys per page; follow the continuation token.
  list_bucket <- function(prefix, delimiter = NULL) {

    pages <- list()
    token <- NULL

    repeat {
      query <- c(`list-type` = "2", prefix = prefix, delimiter = delimiter,
                 `continuation-token` = token)
      page_url <- paste0(base_url, "/?",
                         paste0(names(query), "=", curl::curl_escape(query), collapse = "&"))

      resp <- curl::curl_fetch_memory(page_url)
      if (resp$status_code != 200) {
        stop("S3 listing failed (HTTP ", resp$status_code, "): ", page_url)
      }

      page <- xml2::xml_ns_strip(xml2::read_xml(resp$content))
      pages[[length(pages) + 1]] <- page

      if (xml2::xml_text(xml2::xml_find_first(page, "IsTruncated")) != "true") break
      token <- xml2::xml_text(xml2::xml_find_first(page, "NextContinuationToken"))
    }

    pages
  }

  if (is.null(gcms)) {
    gcms <- list_bucket(prefix = "", delimiter = "/") %>%
      purrr::map(~ xml2::xml_text(xml2::xml_find_all(.x, "CommonPrefixes/Prefix"))) %>%
      unlist() %>%
      sub("/$", "", .)
  }

  keys <- purrr::map(gcms, function(gcm_name) {
    list_bucket(prefix = paste0(gcm_name, "/")) %>%
      purrr::map(~ data.table::data.table(
        object_key = xml2::xml_text(xml2::xml_find_all(.x, "Contents/Key")),
        size = as.numeric(xml2::xml_text(xml2::xml_find_all(.x, "Contents/Size")))
      )) %>%
      data.table::rbindlist()
  }) %>%
    data.table::rbindlist()

  if (nrow(keys) == 0) stop("No files found for GCM(s): ", paste(gcms, collapse = ", "))

  # Keys look like {gcm}/{scenario}/{member}/{variable}/{variable}_day_..._{year}.nc
  keys <- keys[grepl("^([^/]+/){4}[^/]+\\.nc$", object_key)]
  keys[, c("gcm", "scenario", "member", "variable", "file") := data.table::tstrsplit(object_key, "/", fixed = TRUE)]
  keys[, year := as.integer(stringr::str_match(file, "_(\\d{4})(?:_v[0-9.]+)?\\.nc$")[, 2])]

  files <- keys %>%
    dplyr::filter(scenario %in% scenarios,
                  variable %in% variables,
                  is.null(years) | year %in% years) %>%
    dplyr::mutate(url = paste0(base_url, "/", object_key)) %>%
    dplyr::arrange(variable, scenario, gcm, member, year) %>%
    dplyr::select(gcm, scenario, member, variable, year, size, url)

  data.table::as.data.table(files)
}


#' Find the NEX-HIDCP-CMIP6 grid cells that touch a park
#'
#' @param lon,lat Longitude and latitude vectors from a NEX-HIDCP-CMIP6 file.
#'   Longitude may be 0-360, as it is in the files.
#' @param park_boundary sf polygon(s) of the park.
#'
#' @return A data.table with one row per cell: `lon_index` and `lat_index`
#'   (1-based positions in the NetCDF grid), cell-center `lon` (-180 to 180)
#'   and `lat`, and `coverage_fraction`.
get_nex_hidcp_cells <- function(lon, lat, park_boundary){

  grid_lon <- ifelse(lon > 180, lon - 360, lon)
  grid_lat <- as.numeric(lat)
  dx <- abs(grid_lon[2] - grid_lon[1])
  dy <- abs(grid_lat[2] - grid_lat[1])

  grid <- terra::rast(nrows = length(grid_lat), ncols = length(grid_lon),
                      xmin = min(grid_lon) - dx / 2, xmax = max(grid_lon) + dx / 2,
                      ymin = min(grid_lat) - dy / 2, ymax = max(grid_lat) + dy / 2,
                      crs = "EPSG:4326")

  park <- terra::vect(sf::st_transform(park_boundary, 4326))

  # exact = TRUE returns every cell the polygon touches plus the share of the
  # cell it covers.
  hits <- terra::cells(grid, park, exact = TRUE)
  row_col <- terra::rowColFromCell(grid, hits[, "cell"])

  # terra counts rows down from the north edge; NetCDF lat can run either way.
  lat_index <- if (grid_lat[1] < grid_lat[length(grid_lat)]) {
    length(grid_lat) - row_col[, 1] + 1L
  } else {
    row_col[, 1]
  }

  cells <- data.table::data.table(lon_index = as.integer(row_col[, 2]),
                                  lat_index = as.integer(lat_index),
                                  coverage_fraction = hits[, "weights"])

  # Separate park polygons can share a cell; add their pieces together.
  cells <- cells[, .(coverage_fraction = pmin(1, sum(coverage_fraction))),
                 by = .(lon_index, lat_index)]
  cells[, `:=`(lon = grid_lon[lon_index], lat = grid_lat[lat_index])]
  data.table::setorder(cells, lat_index, lon_index)

  cells[]
}


#' Smallest NetCDF lat/lon window holding a set of cells
#'
#' @param cells Table with `lon_index` and `lat_index` columns.
#'
#' @return A list with the window's `start`/`count` along lon and lat, and
#'   `window_index`: each cell's position in the flattened lon x lat window
#'   (lon varies fastest, as `ncdf4::ncvar_get()` returns it).
nex_hidcp_window <- function(cells){

  lon_start <- min(cells$lon_index)
  lat_start <- min(cells$lat_index)
  lon_count <- max(cells$lon_index) - lon_start + 1L

  list(lon_start = lon_start,
       lon_count = lon_count,
       lat_start = lat_start,
       lat_count = max(cells$lat_index) - lat_start + 1L,
       window_index = (cells$lon_index - lon_start + 1L) +
         (cells$lat_index - lat_start) * lon_count)
}


#' Download one NEX-HIDCP-CMIP6 file with retries
#'
#' @param url File URL.
#' @param dest Local path to write.
#' @param size Expected size in bytes (from the S3 listing), or `NA` to skip
#'   the size check.
#' @param retries Attempts before giving up.
download_nex_hidcp_file <- function(url, dest, size = NA, retries = 3){

  # Already here (e.g. the file used to find the park cells).
  if (file.exists(dest) && (is.na(size) || file.size(dest) == size)) return(invisible(dest))

  for (attempt in seq_len(retries)) {

    # resume = TRUE continues a partial file after a dropped connection, and
    # the low-speed limit abandons a stalled one so it can be retried.
    res <- tryCatch(
      curl::multi_download(url, dest, resume = TRUE, progress = FALSE,
                           low_speed_limit = 100000, low_speed_time = 60),
      error = function(e) NULL
    )

    size_ok <- file.exists(dest) && (is.na(size) || file.size(dest) == size)
    if (!is.null(res) && isTRUE(res$success) && size_ok) return(invisible(dest))

    # A file larger than expected can't be resumed; start it over.
    if (file.exists(dest) && !is.na(size) && file.size(dest) > size) unlink(dest)

    Sys.sleep(2^attempt)
  }

  stop("Download failed after ", retries, " attempts: ", url)
}


#' Download one NEX-HIDCP-CMIP6 file and keep only the park cells
#'
#' Worker for `get_nex_hidcp_park()`. Downloads the file, reads the park's
#' lat/lon window for every day, writes one parquet file and deletes the
#' NetCDF.
#'
#' @param url,size File URL and expected size in bytes.
#' @param variable `"pr"`, `"tasmax"` or `"tasmin"`.
#' @param out_path Parquet file to write.
#' @param cells Park cells with `cell_id`, `lon_index`, `lat_index`, `lon`,
#'   `lat` and `window_index`.
#' @param window `nex_hidcp_window()` start/count values for `cells`.
#' @param tmp_dir Folder for the NetCDF while it is processed.
#'
#' @return A one-row data.table describing the file (`error` is `NA`).
process_nex_hidcp_file <- function(url, size, variable, out_path, cells, window, tmp_dir){

  start_clock_time <- Sys.time()

  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  nc_path <- file.path(tmp_dir, basename(url))
  on.exit(unlink(nc_path), add = TRUE)

  download_nex_hidcp_file(url, nc_path, size)

  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc), add = TRUE, after = FALSE)

  # All files should share one grid; stop rather than mislabel cells.
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  lon <- ifelse(lon > 180, lon - 360, lon)

  if (max(cells$lon_index) > length(lon) || max(cells$lat_index) > length(lat) ||
      max(abs(lon[cells$lon_index] - cells$lon)) > 1e-6 ||
      max(abs(lat[cells$lat_index] - cells$lat)) > 1e-6) {
    stop("Grid in ", basename(url), " does not match the park cell table.")
  }

  dim_names <- purrr::map_chr(nc$var[[variable]]$dim, "name")
  if (!identical(dim_names, c("lon", "lat", "time"))) {
    stop("Unexpected dimensions in ", basename(url), ": ", paste(dim_names, collapse = ", "))
  }

  # The whole file is decompressed either way (one chunk per statewide day),
  # so read every day of the park window in one call.
  arr <- ncdf4::ncvar_get(nc,
                          variable,
                          start = c(window$lon_start, window$lat_start, 1),
                          count = c(window$lon_count, window$lat_count, -1),
                          collapse_degen = FALSE)

  n_time <- dim(arr)[3]
  dim(arr) <- c(window$lon_count * window$lat_count, n_time)
  values <- arr[cells$window_index, , drop = FALSE]

  # Decode time with the file's own calendar (standard, 365_day or 360_day).
  time_units <- ncdf4::ncatt_get(nc, "time", "units")$value
  calendar <- ncdf4::ncatt_get(nc, "time", "calendar")
  calendar <- if (calendar$hasatt) calendar$value else "standard"

  stamps <- CFtime::as_timestamp(
    CFtime::CFtime(time_units, calendar, ncdf4::ncvar_get(nc, "time")),
    "date"
  )

  units <- ncdf4::ncatt_get(nc, variable, "units")$value

  # values is cells x days, so as.vector() runs through every cell for day 1,
  # then day 2, and so on.
  n_cells <- nrow(cells)
  results_dt <- data.table::data.table(
    cell_id = rep.int(cells$cell_id, n_time),
    date = rep(as.Date(stamps, format = "%Y-%m-%d"), each = n_cells),
    year = rep(as.integer(substr(stamps, 1, 4)), each = n_cells),
    month = rep(as.integer(substr(stamps, 6, 7)), each = n_cells),
    day = rep(as.integer(substr(stamps, 9, 10)), each = n_cells),
    value = as.vector(values)
  )

  # float32 matches the source precision and halves the size of value.
  results_tbl <- arrow::arrow_table(
    results_dt,
    schema = arrow::schema(cell_id = arrow::int32(),
                           date = arrow::date32(),
                           year = arrow::int16(),
                           month = arrow::int8(),
                           day = arrow::int8(),
                           value = arrow::float32())
  )
  results_tbl$metadata$units <- units
  results_tbl$metadata$calendar <- calendar
  results_tbl$metadata$source_url <- url

  # Write under a dot-prefixed name, which open_dataset() ignores, then rename,
  # so an interrupted run never leaves a half-written file that looks done.
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  tmp_out <- file.path(dirname(out_path), paste0(".", basename(out_path), ".tmp"))
  arrow::write_parquet(results_tbl, tmp_out, compression = "zstd")
  file.rename(tmp_out, out_path)

  data.table::data.table(
    url = url,
    out_path = out_path,
    n_days = n_time,
    n_rows = nrow(results_dt),
    units = units,
    calendar = calendar,
    seconds = round(as.numeric(difftime(Sys.time(), start_clock_time, units = "secs")), 1),
    error = NA_character_
  )
}
