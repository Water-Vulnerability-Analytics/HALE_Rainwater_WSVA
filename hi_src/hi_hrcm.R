#' Hourly rainfall from the Hawaiʻi Regional Climate Model (HRCM)
#'
#' Zhang, C. (2017). Very fine resolution dynamically downscaled climate data for
#' Hawaii. USGS ScienceBase, doi:10.21429/1vke-9325. WRF v3.7 on 0.8-km grids for Maui
#' and Hawaiʻi Island: a present-day run (1990-2009) and RCP4.5 and RCP8.5 runs for the
#' late century (2080-2099), stored with 1990-2009 dates. Per the project final report
#' (Wang, Y., 2017, USGS agreement G15AP00166, ScienceBase 5a623f5de4b06e28e9c3d8b9), the
#' present-day run is driven by MERRA reanalysis and NOAA SST, and each future run adds the
#' monthly multi-model-mean change (2080-2099 minus 1990-2009) of 12 CMIP5 models to that
#' forcing (pseudo-global warming). The report does not name the 12 models. The zarr stores (`maui_present`, `maui_rcp45`,
#' `maui_rcp85`, `hawaii_*`) are read anonymously from the USGS Open Storage Network
#' through GDAL's Zarr driver, so only the cells needed are downloaded.
#'
#' Rain: `RAINNC` is the accumulated total grid-scale precipitation (mm). The runs use
#' no cumulus scheme (`CU_PHYSICS = 0`), so it is all the rain. WRF resets it every
#' `BUCKET_MM` (100 mm) and counts the resets in `I_RAINNC`:
#' total = RAINNC + BUCKET_MM x I_RAINNC. Hourly rain is the difference between hours.
#' Where the total drops (an accumulation restart: once in maui_rcp45, at 2008-01-01
#' 00:00 UTC), that hour's rain is unknown and set to NA.
#'
#' Time: the stored "hours since 1990-01-01" were passed through a US daylight-saving
#' time zone. Each year one label repeats on the spring daylight-saving date and one is
#' skipped on the fall date (first Sunday in April and last Sunday in October through
#' 2006; second Sunday in March and first Sunday in November from 2007), so the labels
#' between them run one hour early. `hi_hrcm_times()` checks those dates and replaces the
#' 0- and 2-hour steps with 1-hour steps. (A one-off check of the shortwave field, not
#' rerun here, agreed: with the raw labels, solar noon at HALE fell at 22.5 UTC in January
#' but 21.3 UTC in July; after the fix, 22.3 UTC in July.) A real gap (24 missing records
#' on 1999-03-16 UTC in the present-day run) stays a gap.

#' GDAL data source name for one HRCM run
hi_hrcm_dsn <- function(run, endpoint = "https://usgs.osn.mghpcc.org/mdmf/gdp") {
  sprintf('ZARR:"/vsicurl/%s/%s.zarr"', endpoint, run)
}

#' Evaluate `expr` with GDAL told not to list remote directories (much faster over /vsicurl)
hi_hrcm_no_listing <- function(expr) {
  old <- Sys.getenv("GDAL_DISABLE_READDIR_ON_OPEN", NA)
  Sys.setenv(GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR")
  on.exit(if (is.na(old)) Sys.unsetenv("GDAL_DISABLE_READDIR_ON_OPEN") else Sys.setenv(GDAL_DISABLE_READDIR_ON_OPEN = old))
  expr
}

#' Read one HRCM array (dimensions west_east, south_north[, Time]) as a plain R array
hi_hrcm_read <- function(dsn, variable, offset = NULL, count = NULL) {
  # stars warns that the stored Time coordinate is not monotonic (the daylight-saving
  # artifact). Times are rebuilt by hi_hrcm_times(), so that one warning is muffled here.
  quiet <- function(expr) withCallingHandlers(expr, warning = function(w) {
    if (grepl("Offsets not monotonically increasing", conditionMessage(w))) invokeRestart("muffleWarning")
  })
  r <- quiet(if (is.null(offset)) stars::read_mdim(dsn, variable = variable, proxy = FALSE) else
    stars::read_mdim(dsn, variable = variable, offset = offset, count = count, proxy = FALSE))
  x <- r[[1]]
  attr(x, "units") <- NULL
  unclass(x)
}

#' Corrected UTC times of an HRCM run (see the note on daylight saving above)
hi_hrcm_times <- function(dsn) {
  v <- as.numeric(hi_hrcm_read(dsn, "Time"))
  d <- diff(v)
  flips <- d[d %in% c(0, 2)]
  if (length(flips)) {
    stopifnot("daylight-saving steps do not pair up" =
                flips[1] == 0 && sum(flips == 0) == sum(flips == 2) && all(diff(flips) != 0))
    # The flips must fall on the US daylight-saving dates: the first Sunday in April and
    # last Sunday in October through 2006, the second Sunday in March and first Sunday
    # in November from 2007
    raw <- as.Date(as.POSIXct("1990-01-01", tz = "UTC") + 3600 * v)
    nth_sunday <- function(y, m, k) {              # k-th Sunday of month m in year y; k = -1 for the last
      days <- as.Date(sprintf("%d-%02d-01", y, m)) + 0:30
      sun <- days[format(days, "%m") == sprintf("%02d", m) & as.POSIXlt(days)$wday == 0]
      if (k > 0) sun[k] else sun[length(sun)]
    }
    on_dst_date <- function(x, spring) vapply(seq_along(x), function(i) {
      y <- as.integer(format(x[i], "%Y"))
      want <- if (spring) { if (y < 2007) nth_sunday(y, 4, 1) else nth_sunday(y, 3, 2) }
              else { if (y < 2007) nth_sunday(y, 10, -1) else nth_sunday(y, 11, 1) }
      x[i] == want
    }, logical(1))
    stopifnot("time-label flips are not on the US daylight-saving dates" =
                all(on_dst_date(raw[which(d == 0)], TRUE)) && all(on_dst_date(raw[which(d == 2)], FALSE)))
  }
  hours <- v[1] + c(0, cumsum(ifelse(d %in% c(0, 2), 1, d)))
  times <- as.POSIXct("1990-01-01", tz = "UTC") + 3600 * hours
  stopifnot(all(diff(as.numeric(times)) > 0))
  times
}

#' Nearest HRCM grid cell (or n x n block around it) for each site
#'
#' @return data.table: `id_col`, `we`, `sn` (0-based indices for GDAL offsets),
#'   `center_km` (distance from the site to the nearest cell centre), and each cell's
#'   centre `lon`, `lat` and distance from the site `dist_km`.
hi_hrcm_cells <- function(pts, dsn, id_col = "system", n = 1) {
  lon <- hi_hrcm_read(dsn, "XLONG")
  lat <- hi_hrcm_read(dsn, "XLAT")
  xy <- sf::st_coordinates(sf::st_transform(pts, 4326))
  h <- (n - 1L) %/% 2L
  out <- data.table::rbindlist(lapply(seq_len(nrow(xy)), function(i) {
    dist_km <- 6371 * sqrt(((lon - xy[i, 1]) * cos(xy[i, 2] * pi / 180) * pi / 180)^2 +
                             ((lat - xy[i, 2]) * pi / 180)^2)
    k <- arrayInd(which.min(dist_km), dim(lon))
    g <- as.matrix(expand.grid(we = k[1] + (-h:h), sn = k[2] + (-h:h)))
    data.table::data.table(id = pts[[id_col]][i], we = g[, 1] - 1L, sn = g[, 2] - 1L, center_km = min(dist_km),
                           lon = lon[g], lat = lat[g], dist_km = dist_km[g])
  }))
  data.table::setnames(out, "id", id_col)
  out[]
}

#' Hourly rainfall at given HRCM cells from one run
#'
#' Reads `RAINNC` and `I_RAINNC` for the smallest window holding all the cells, in
#' time slabs aligned with the zarr chunks, and differences the accumulated total.
#' @param cells data frame with 0-based `we`, `sn` (from `hi_hrcm_cells()`); duplicates are dropped.
#' @param run `"maui_present"`, `"maui_rcp45"` or `"maui_rcp85"` (or a `hawaii_*` run).
#' @param slab_hours hours read per request (a multiple of the 3,158-hour chunk).
#' @return data.table: `we`, `sn`, `time_utc` (end of the hour), `rain_mm` (rain during the
#'   hour; NA for the first record and across gaps).
hi_hrcm_cell_rain <- function(cells, run = "maui_present", slab_hours = 8L * 3158L) {
  hi_hrcm_no_listing({
    cells <- unique(data.table::as.data.table(cells)[, .(we = as.integer(we), sn = as.integer(sn))])
    dsn <- hi_hrcm_dsn(run)
    info <- jsonlite::fromJSON(sf::gdal_utils("mdiminfo", dsn, quiet = TRUE), simplifyVector = TRUE)
    bucket <- as.numeric(info$attributes$BUCKET_MM)
    stopifnot("expected no cumulus scheme" = identical(as.numeric(info$attributes$CU_PHYSICS), 0), length(bucket) == 1)
    times <- hi_hrcm_times(dsn)
    nt <- length(times)
    we0 <- min(cells$we); sn0 <- min(cells$sn)
    nwe <- max(cells$we) - we0 + 1L; nsn <- max(cells$sn) - sn0 + 1L
    ci <- cbind(cells$we - we0 + 1L, cells$sn - sn0 + 1L)
    total <- do.call(rbind, lapply(seq(0L, nt - 1L, by = slab_hours), function(t0) {
      cnt <- min(slab_hours, nt - t0)
      a <- hi_hrcm_read(dsn, "RAINNC", offset = c(we0, sn0, t0), count = c(nwe, nsn, cnt))
      b <- hi_hrcm_read(dsn, "I_RAINNC", offset = c(we0, sn0, t0), count = c(nwe, nsn, cnt))
      a <- array(a, c(nwe, nsn, cnt)); b <- array(b, c(nwe, nsn, cnt))
      vapply(seq_len(nrow(ci)), function(j) a[ci[j, 1], ci[j, 2], ] + bucket * b[ci[j, 1], ci[j, 2], ], numeric(cnt))
    }))
  })
  total <- matrix(total, nrow = nt)
  hourly <- rbind(NA_real_, diff(total))
  step_h <- c(NA, diff(as.numeric(times)) / 3600)
  hourly[which(step_h != 1), ] <- NA_real_                  # rain across a gap cannot be placed in time
  # An accumulation restart (the running total drops by more than 1 mm, e.g. maui_rcp45
  # at 2008-01-01 00:00 UTC) leaves that hour's rain unknown at every cell
  restart <- which(apply(hourly, 1, function(h) any(h < -1, na.rm = TRUE)))
  if (length(restart)) {
    message(run, ": accumulation restarts at ", paste(format(times[restart], "%Y-%m-%d %H:%M UTC"), collapse = ", "),
            "; rain in those hours is set to NA")
    hourly[restart, ] <- NA_real_
  }
  hourly[!is.na(hourly) & hourly < 0 & hourly > -0.01] <- 0  # single-precision rounding
  stopifnot("negative hourly rain: bucket resets misaligned" = all(hourly >= 0, na.rm = TRUE))
  data.table::data.table(we = rep(cells$we, each = nt), sn = rep(cells$sn, each = nt),
                         time_utc = rep(times, nrow(cells)), rain_mm = as.vector(hourly))
}

#' Hourly values of instantaneous HRCM variables (e.g. 10-m wind `U10`, `V10`) at given cells
#'
#' Unlike rain, these are stored as the value at each output hour, not an accumulated
#' total, so no differencing is needed. Reads the smallest window holding all the cells, in
#' time slabs aligned with the zarr chunks.
#' @param cells data frame with 0-based `we`, `sn` (from `hi_hrcm_cells()`); duplicates are dropped.
#' @param variables HRCM variable names, e.g. `c("U10", "V10")` (m/s, along the model grid's x and y).
#' @return data.table: `we`, `sn`, `time_utc`, then one column per variable (lower case).
hi_hrcm_cell_vars <- function(cells, run = "maui_present", variables = c("U10", "V10"), slab_hours = 8L * 3158L) {
  hi_hrcm_no_listing({
    cells <- unique(data.table::as.data.table(cells)[, .(we = as.integer(we), sn = as.integer(sn))])
    dsn <- hi_hrcm_dsn(run)
    times <- hi_hrcm_times(dsn)
    nt <- length(times)
    we0 <- min(cells$we); sn0 <- min(cells$sn)
    nwe <- max(cells$we) - we0 + 1L; nsn <- max(cells$sn) - sn0 + 1L
    ci <- cbind(cells$we - we0 + 1L, cells$sn - sn0 + 1L)
    out <- data.table::data.table(we = rep(cells$we, each = nt), sn = rep(cells$sn, each = nt), time_utc = rep(times, nrow(cells)))
    for (v in variables) {
      vals <- do.call(rbind, lapply(seq(0L, nt - 1L, by = slab_hours), function(t0) {
        cnt <- min(slab_hours, nt - t0)
        a <- array(hi_hrcm_read(dsn, v, offset = c(we0, sn0, t0), count = c(nwe, nsn, cnt)), c(nwe, nsn, cnt))
        vapply(seq_len(nrow(ci)), function(j) a[ci[j, 1], ci[j, 2], ], numeric(cnt))
      }))
      out[, (tolower(v)) := as.vector(matrix(vals, nrow = nt))]
    }
    out
  })
}

#' Download HRCM hourly 10-m wind for given cells, one parquet per run
#'
#' Writes `dir/<run>_wind.parquet` (`we`, `sn`, `time_utc`, `u10`, `v10`). A file already on
#' disk is not downloaded again, unless `overwrite = TRUE`.
#' @return data.table, one row per run: `run`, `file`, `status`, `mb`.
hi_hrcm_download_wind <- function(cells, dir, runs = "maui_present", overwrite = FALSE) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  data.table::rbindlist(lapply(runs, function(run) {
    f <- file.path(dir, paste0(run, "_wind.parquet"))
    status <- "on disk"
    if (overwrite || !file.exists(f)) {
      tmp <- paste0(f, ".part")
      arrow::write_parquet(hi_hrcm_cell_vars(cells, run, c("U10", "V10")), tmp)
      file.rename(tmp, f)
      status <- "downloaded"
    }
    data.table::data.table(run = run, file = f, status = status, mb = file.size(f) / 1e6)
  }))
}

#' Download HRCM hourly rain for the n x n cells around each site, once
#'
#' Writes `dir/_cells.csv` (each site's cells: 0-based `we`, `sn`, cell-centre `lon`,
#' `lat` and distance from the site `dist_km`) and one `dir/<run>.parquet` per run
#' (`we`, `sn`, `time_utc`, `rain_mm` for every cell in `_cells.csv`, hourly). A file
#' already on disk is not downloaded again, unless `overwrite = TRUE`. The cells are located on
#' the first run's grid; every later run is checked to have the same grid.
#' @param runs HRCM runs, e.g. `c("maui_present", "maui_rcp45", "maui_rcp85")`.
#' @param overwrite if TRUE, re-download `_cells.csv` and every run even if already on disk.
#' @return data.table, one row per run: `run`, `file`, `status` ("downloaded" or "on disk"),
#'   `mb`, and the file's contents from `hi_hrcm_summary()`.
hi_hrcm_download <- function(pts, dir, runs = "maui_present", id_col = "system", n = 3, overwrite = FALSE) {
  if (overwrite) unlink(file.path(dir, c("_cells.csv", paste0(runs, ".parquet"))))
  cells <- data.table::as.data.table(hi_cache(file.path(dir, "_cells.csv"),
                                              hi_hrcm_no_listing(hi_hrcm_cells(pts, hi_hrcm_dsn(runs[1]), id_col, n))))
  stopifnot("_cells.csv does not match the sites" = setequal(cells[[id_col]], pts[[id_col]]))
  data.table::rbindlist(lapply(runs, function(run) {
    f <- file.path(dir, paste0(run, ".parquet"))
    status <- "on disk"
    if (!file.exists(f)) {
      k <- cbind(cells$we + 1L, cells$sn + 1L)
      grid <- hi_hrcm_no_listing(list(lon = hi_hrcm_read(hi_hrcm_dsn(run), "XLONG"),
                                      lat = hi_hrcm_read(hi_hrcm_dsn(run), "XLAT")))
      stopifnot("grid differs from _cells.csv" =
                  max(abs(grid$lon[k] - cells$lon), abs(grid$lat[k] - cells$lat)) < 1e-4)
      tmp <- paste0(f, ".part")                    # a failed download leaves no partial file behind
      arrow::write_parquet(hi_hrcm_cell_rain(cells, run), tmp)
      file.rename(tmp, f)
      status <- "downloaded"
    }
    cbind(data.table::data.table(run = run, file = f, status = status, mb = file.size(f) / 1e6), hi_hrcm_summary(f))
  }))
}

#' What an HRCM rain file holds: cells, hours, first and last hour, missing hours
hi_hrcm_summary <- function(f) {
  ds <- arrow::open_dataset(f)
  s <- ds %>% dplyr::summarise(n = dplyr::n(), start = min(time_utc), end = max(time_utc)) %>% dplyr::collect()
  cell1 <- ds %>% dplyr::select(we, sn) %>% head(1) %>% dplyr::collect()
  tt <- sort(dplyr::collect(dplyr::filter(dplyr::select(ds, we, sn, time_utc), we == !!cell1$we, sn == !!cell1$sn))$time_utc)
  step_h <- diff(as.numeric(tt)) / 3600
  i <- which(step_h > 1)                         # hours strictly between tt[i] and tt[i + 1] are missing
  data.table::data.table(cells = s$n / length(tt), hours = length(tt), start = s$start, end = s$end,
                         missing_hours = sum(step_h[i] - 1),
                         gaps = if (length(i)) paste0("hours ending ", format(tt[i] + 3600, "%Y-%m-%d %H:%M"), " to ",
                                                      format(tt[i + 1] - 3600, "%Y-%m-%d %H:%M UTC"), collapse = "; ") else "")
}

#' Daily rainfall from hourly HRCM rain (one series per `id_col`), by local calendar day
#'
#' Each hour is assigned to the local day in which it starts (Hawaiʻi Standard Time,
#' UTC-10). Days with fewer than 24 hours of valid rain (the first and last day, gaps)
#' are dropped.
hi_hrcm_daily <- function(hourly, id_col = "system", tz_offset_h = -10) {
  h <- data.table::copy(hourly)[, date := as.Date(time_utc - 3600 + tz_offset_h * 3600, tz = "UTC")]
  d <- h[, .(pr = sum(rain_mm), n_valid = sum(!is.na(rain_mm))), by = c(id_col, "date")]
  d[n_valid == 24][, n_valid := NULL][, `:=`(year = as.integer(format(date, "%Y")), month = as.integer(format(date, "%m")))][]
}
