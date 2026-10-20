#' Small utilities shared by the hi_src functions
#'
#' These helpers are not specific to any park. `hi_cache()` lets a report knit
#' offline after the first run: it evaluates an expression only when its cache
#' file is missing.

#' Evaluate `expr` once and cache the result to `path`
#'
#' The file type comes from the extension: csv, parquet, rds or gpkg.
#' `expr` is evaluated lazily, only when `path` does not exist.
hi_cache <- function(path, expr) {
  ext <- tolower(tools::file_ext(path))
  if (file.exists(path)) {
    return(switch(ext,
                  csv = as.data.frame(readr::read_csv(path, show_col_types = FALSE)),
                  parquet = arrow::read_parquet(path),
                  rds = readRDS(path),
                  gpkg = {
                    x <- sf::st_read(path, quiet = TRUE)
                    sf::st_geometry(x) <- "geometry"     # GeoPackage stores it as "geom"
                    x
                  },
                  stop("hi_cache(): unsupported file type ", ext)))
  }
  x <- force(expr)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  switch(ext,
         csv = readr::write_csv(x, path),
         parquet = arrow::write_parquet(x, path),
         rds = saveRDS(x, path),
         gpkg = sf::st_write(x, path, quiet = TRUE, delete_dsn = TRUE),
         stop("hi_cache(): unsupported file type ", ext))
  x
}

#' Longest run of TRUE values (0 if none)
hi_longest_run <- function(x) {
  r <- rle(x)
  if (any(r$values)) max(r$lengths[r$values]) else 0L
}

#' Hawaiʻi water year (November-October), labeled by the year it ends
hi_water_year <- function(year, month) year + (month >= 11L)

#' Position of a calendar month within the water year (November = 1, ..., October = 12)
hi_wy_month <- function(month) (as.integer(month) - 11L) %% 12L + 1L
