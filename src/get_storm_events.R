#' NOAA Storm Events for one state, from the NCEI yearly files
#'
#' NOAA's Storm Events Database (NCEI) lists significant weather events reported by each
#' National Weather Service office: high wind, heavy rain, flash floods, winter storms and
#' others, with date, place (county or forecast zone), narrative and damage. One file per
#' year: https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/. This function finds
#' each year's latest "details" file, downloads it, keeps one state's rows, and returns them.
#'
#' @param state state name as written in the files, e.g. "HAWAII".
#' @param years event years (zone-based records start in 1996).
#' @param out_file optional parquet cache; if it exists it is read instead (unless `overwrite`).
#' @param overwrite if TRUE, download again and rebuild `out_file`.
#' @return data.table of the state's events (columns as in the NCEI files, lower case).
get_storm_events <- function(state = "HAWAII", years = 1996:as.integer(format(Sys.Date(), "%Y")),
                             out_file = NULL, overwrite = FALSE) {
  if (!overwrite && !is.null(out_file) && file.exists(out_file)) return(data.table::as.data.table(arrow::read_parquet(out_file)))
  base <- "https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
  listing <- readLines(base, warn = FALSE)
  files <- unique(unlist(regmatches(listing, gregexpr("StormEvents_details-ftp_v1\\.0_d[0-9]{4}_c[0-9]+\\.csv\\.gz", listing))))
  ev <- data.table::rbindlist(lapply(years, function(y) {
    f <- sort(grep(paste0("_d", y, "_"), files, value = TRUE), decreasing = TRUE)[1]      # latest revision
    if (is.na(f)) return(NULL)
    tmp <- tempfile(fileext = ".csv.gz"); on.exit(unlink(tmp))
    utils::download.file(paste0(base, f), tmp, mode = "wb", quiet = TRUE)
    x <- suppressWarnings(data.table::fread(tmp, colClasses = "character", showProgress = FALSE, fill = TRUE))
    names(x) <- tolower(names(x))
    st <- toupper(state)
    x[x$state == st][, source_file := f]
  }), fill = TRUE)
  if (!is.null(out_file)) arrow::write_parquet(ev, out_file)
  ev
}
