#' Read one year of HCDP daily rainfall at the park cells and write parquet
#'
#' Worker for `get_hcdp_obs_park()`. Defined at the top level, not nested
#' inside `get_hcdp_obs_park()`: a nested version briefly existed and a run
#' hung after every file had already been written correctly (interrupting was
#' safe; nothing was lost). Checking directly, `future`/`globals` did not turn
#' out to bundle the outer function's other local variables (e.g.
#' `park_boundary`) into what gets sent to a worker just because of the
#' nesting -- only variables the worker body actually references by name --
#' so nesting was not confirmed as that hang's cause. It's kept top-level
#' anyway because it matches the `download_nex_hidcp_file()`/
#' `process_nex_hidcp_file()` pair in `get_nex_hidcp_park.R`, makes this
#' function's inputs fully explicit (`year`, `out_path`, `cells`, `base_url`,
#' `retries`) and callable/testable on its own, and removes any chance of
#' picking up an outer variable by accident in a later edit. The timeout in
#' `get_hcdp_obs_park()` is what actually guarantees a hang can't recur
#' silently, whatever the cause.
#'
#' @param year Year to pull.
#' @param out_path Parquet file to write.
#' @param cells Table with `cell_id`, `lon`, `lat`.
#' @param base_url HCDP daily rainfall map folder.
#' @param retries Attempts per day before the year fails.
#'
#' @return A one-row data.table describing the year (`error` is `NA`).
process_hcdp_obs_year <- function(year, out_path, cells, base_url, retries = 3, last_date = Sys.Date() - 1){

  start_clock_time <- Sys.time()

  # Skip GDAL's directory listing and HEAD requests: each day then costs one
  # small ranged read of the rows the park covers. The HTTP_* timeouts bound
  # a single stalled connection so it fails instead of hanging forever -- the
  # retry loop below only helps once a request can actually fail.
  terra::setGDALconfig(c("GDAL_DISABLE_READDIR_ON_OPEN=EMPTY_DIR",
                         "CPL_VSIL_CURL_USE_HEAD=NO",
                         "CPL_VSIL_CURL_ALLOWED_EXTENSIONS=.tif",
                         "GDAL_HTTP_MERGE_CONSECUTIVE_RANGES=YES",
                         "GDAL_HTTP_CONNECTTIMEOUT=15",
                         "GDAL_HTTP_TIMEOUT=30",
                         "GDAL_HTTP_LOW_SPEED_LIMIT=1",
                         "GDAL_HTTP_LOW_SPEED_TIME=30"))

  dates <- seq(as.Date(paste0(year, "-01-01")), min(as.Date(paste0(year, "-12-31")), as.Date(last_date)), by = "day")
  xy <- cbind(cells$lon, cells$lat)

  values <- vapply(dates, function(d) {
    url <- paste0("/vsicurl/", base_url, "/", format(d, "%Y/%m"),
                  "/rainfall_new_day_statewide_data_map_", format(d, "%Y_%m_%d"), ".tif")
    for (attempt in seq_len(retries)) {
      v <- tryCatch(terra::extract(terra::rast(url), xy)[[1]], error = function(e) NULL)
      if (!is.null(v)) return(v)
      Sys.sleep(2^attempt)
    }
    # A day the portal does not have (HTTP 404, e.g. 2021-07-23) is kept as missing (NA)
    status <- tryCatch(httr::status_code(httr::HEAD(sub("^/vsicurl/", "", url))), error = function(e) NA)
    if (identical(status, 404L)) {
      message("HCDP has no map for ", format(d), "; stored as NA")
      return(rep(NA_real_, nrow(cells)))
    }
    stop("Could not read ", format(d), " after ", retries, " attempts.")
  }, numeric(nrow(cells)))

  # values is cells x days, so as.vector() runs through every cell for day 1,
  # then day 2, and so on, matching get_nex_hidcp_park() output.
  n_cells <- nrow(cells)
  results_dt <- data.table::data.table(
    cell_id = rep.int(cells$cell_id, length(dates)),
    date = rep(dates, each = n_cells),
    year = rep(as.integer(format(dates, "%Y")), each = n_cells),
    month = rep(as.integer(format(dates, "%m")), each = n_cells),
    day = rep(as.integer(format(dates, "%d")), each = n_cells),
    value = as.vector(values)
  )

  results_tbl <- arrow::arrow_table(
    results_dt,
    schema = arrow::schema(cell_id = arrow::int32(),
                           date = arrow::date32(),
                           year = arrow::int16(),
                           month = arrow::int8(),
                           day = arrow::int8(),
                           value = arrow::float32())
  )
  results_tbl$metadata$units <- "mm/day"
  results_tbl$metadata$calendar <- "standard"
  results_tbl$metadata$source_url <- base_url

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  tmp_out <- file.path(dirname(out_path), paste0(".", basename(out_path), ".tmp"))
  arrow::write_parquet(results_tbl, tmp_out, compression = "zstd")
  file.rename(tmp_out, out_path)

  data.table::data.table(
    year = year,
    n_days = length(dates),
    n_rows = nrow(results_dt),
    minutes = round(as.numeric(difftime(Sys.time(), start_clock_time, units = "mins")), 1),
    error = NA_character_
  )
}


#' Pull the observed daily rainfall NEX-HIDCP-CMIP6 was trained on, for a park
#'
#' NEX-HIDCP-CMIP6 bias-corrects every GCM to the Hawaii Climate Data Portal
#' (HCDP) daily 250 m rainfall maps for 1990-2014 (Thrasher et al. 2026). For
#' the historical period this pulls those observations directly instead of 30
#' models' hindcasts: one statewide GeoTIFF per day, on exactly the same grid as
#' NEX-HIDCP, so the park's cells line up with `get_nex_hidcp_park()` output.
#'
#' The TIFFs are stored one row per block, so reading only the park's cells
#' over HTTP fetches a few KB per day (~0.35 s); nothing is downloaded whole.
#' HCDP daily maps start on 1990-01-01.
#'
#' Output goes into the same parquet dataset as `get_nex_hidcp_park()`, with
#' the same columns, one file per year:
#' `variable=pr/scenario=observed/gcm=HCDP/pr_day_HCDP_observed_<year>.parquet`.
#' `_cells.parquet` is reused if it exists, so `cell_id` matches the futures.
#'
#' @param park The 4 letter park code, used with `getParkBoundary()`.
#' @param output_dir Folder for the parquet dataset (the same one used for the
#'   NEX-HIDCP futures).
#' @param park_boundary Optional sf polygon(s); only needed when
#'   `_cells.parquet` doesn't exist yet. Defaults to `getParkBoundary(park)`.
#' @param years Years to pull (1990 onward). NEX-HIDCP's training period is
#'   1990-2014.
#' @param workers Number of years pulled at once.
#' @param last_date Last day to pull (default yesterday). A year that ends after it is
#'   written as a partial year; rerun that year with `overwrite = TRUE` to extend it.
#' @param overwrite If `TRUE`, re-pull years that already have output.
#' @param timeout_min Wall-clock limit on the whole pull, in minutes. If it's
#'   hit, the function stops waiting and returns `NULL` with a message instead
#'   of hanging indefinitely -- years already written to disk are unaffected,
#'   and rerunning picks up only what's still missing. Default: generous but
#'   bounded, `max(20, 15 * ceiling(years to pull / workers))`.
#'
#' @return Invisibly, a data.table with one row per year pulled in this run
#'   (`error` is `NA` for successes), or `NULL` if `timeout_min` was reached.
get_hcdp_obs_park <- function(park = "HALE",
                              output_dir,
                              park_boundary = NULL,
                              years = 1990:2014,
                              workers = 8,
                              overwrite = FALSE,
                              last_date = Sys.Date() - 1,
                              timeout_min = NULL){

  start_clock_time <- Sys.time()

  base_url <- "https://api.hcdp.ikewai.org/files/download/production/rainfall/new/day/statewide/data_map"

  if (min(years) < 1990) stop("HCDP daily rainfall maps start in 1990.")

  # Catch accidental nesting (e.g. hi_datasets/HALE/HALE) before creating anything.
  if (basename(output_dir) == basename(dirname(output_dir))) {
    stop("output_dir looks nested (", output_dir, "). Point it at the park folder itself, ",
         "e.g. here::here(\"hi_datasets\", \"", park, "\").")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir)

  out_paths <- file.path(output_dir, "variable=pr", "scenario=observed", "gcm=HCDP",
                         paste0("pr_day_HCDP_observed_", years, ".parquet"))
  todo <- if (overwrite) seq_along(years) else which(!file.exists(out_paths))

  message(sprintf("%s: %d of %d years of HCDP observed rainfall to pull; %d already done.",
                  park, length(todo), length(years), length(years) - length(todo)))

  if (length(todo) == 0) return(invisible(NULL))

  # Park cells -----------------------------------------------------------------

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

    # Same grid as NEX-HIDCP: build the lon/lat vectors from the first day and
    # drop cells with no data (ocean).
    first_day <- terra::rast(paste0("/vsicurl/", base_url, "/", years[1], "/01/rainfall_new_day_statewide_data_map_",
                                    years[1], "_01_01.tif"))
    lon <- terra::xFromCol(first_day, seq_len(terra::ncol(first_day)))
    lat <- rev(terra::yFromRow(first_day, seq_len(terra::nrow(first_day))))

    cells <- get_nex_hidcp_cells(lon = lon, lat = lat, park_boundary = park_boundary)
    cells <- cells[!is.na(terra::extract(first_day, cbind(cells$lon, cells$lat))[[1]])]
    cells[, `:=`(cell_id = .I, park = park)]
    data.table::setcolorder(cells, c("park", "cell_id"))

    arrow::write_parquet(cells, cells_path)
    message(sprintf("Saved %d grid cells for %s to %s", nrow(cells), park, cells_path))
  }

  # Pull years in parallel -----------------------------------------------------

  old_plan <- future::plan()
  future::plan(future::multisession, workers = min(workers, length(todo)))
  on.exit(future::plan(old_plan), add = TRUE)

  # Subset here, not inside a worker closure: process_hcdp_obs_year() only
  # needs cell_id/lon/lat, and passing it as a plain argument keeps each
  # dispatched task small and independent of anything else in this function's
  # environment (see that function's own docs for why it lives at the top
  # level instead of being nested in here).
  obs_cells <- cells[, .(cell_id, lon, lat)]

  # Hard ceiling on the whole pull. A day normally takes well under a second,
  # so this is deliberately generous -- it exists so a stall of any kind (a
  # network hang the per-request timeouts above didn't catch, a wedged
  # worker, anything) makes this function fail loudly after a bounded wait
  # instead of hanging forever, which is what actually happened once before.
  # Confirmed directly: setTimeLimit() here does interrupt a genuinely stuck
  # future_map() call, and the on.exit() below still runs on that interrupt,
  # which resets the plan and kills the worker processes cleanly -- nothing
  # is left running in the background. Parquet files are written by each
  # worker as it finishes, before anything is returned here, so a timeout
  # costs nothing already on disk; rerunning just picks up what's missing.
  if (is.null(timeout_min)) {
    timeout_min <- max(20, 15 * ceiling(length(todo) / min(workers, length(todo))))
  }
  setTimeLimit(elapsed = timeout_min * 60, transient = TRUE)
  on.exit(setTimeLimit(), add = TRUE)

  results <- tryCatch(
    furrr::future_map(todo, function(i) {
      tryCatch(
        process_hcdp_obs_year(year = years[i], out_path = out_paths[i],
                              cells = obs_cells, base_url = base_url, last_date = last_date),
        error = function(e) data.table::data.table(year = years[i], error = conditionMessage(e))
      )
    }, .options = furrr::furrr_options(seed = NULL)) %>%
      data.table::rbindlist(fill = TRUE),
    error = function(e) {
      message("get_hcdp_obs_park() hit its ", timeout_min, "-minute limit. Years that had ",
              "already finished were written normally; rerun with the same arguments to pick ",
              "up whatever is still missing. (", conditionMessage(e), ")")
      NULL
    }
  )

  if (is.null(results)) return(invisible(NULL))

  failed <- results[!is.na(error)]
  if (nrow(failed) > 0) {
    message("Failed years (rerun to retry): ", paste(failed$year, collapse = ", "))
  }

  message(sprintf("Finished %d years (%d failed) in %.1f minutes.",
                  nrow(results), nrow(failed),
                  as.numeric(difftime(Sys.time(), start_clock_time, units = "mins"))))

  invisible(results)
}
