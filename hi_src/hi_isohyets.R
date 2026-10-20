#' Compare two rainfall maps and their isohyets (e.g., HCDP vs. the Rainfall Atlas)
#'
#' @param r_a,r_b SpatRasters of mean annual rainfall (mm); `r_b` is resampled to `r_a`'s grid.
#' @param line_a,line_b sf isohyet lines at `threshold_mm` (e.g., from `isohyet_line()`).
#' @param utm_epsg projected CRS for distances (Hawaiʻi: 32604 or 32605).
#' @return list: `cells` (per cell values, % difference and side of the line in `r_b`),
#'   `gap_km` (distance from points every 100 m along `line_a` to `line_b`) and `summary`.
hi_compare_isohyets <- function(r_a, r_b, line_a, line_b, threshold_mm = 2000, utm_epsg = 32604) {
  b_on_a <- terra::project(r_b, r_a, method = "near")
  g <- c(r_a, b_on_a, terra::cellSize(r_a, unit = "km"))
  names(g) <- c("a_mm", "b_mm", "km2")
  cells <- data.table::as.data.table(terra::as.data.frame(g, na.rm = TRUE))
  cells[, `:=`(pct_diff = 100 * (a_mm / b_mm - 1), b_wet = b_mm >= threshold_mm)]
  pts <- sf::st_transform(line_a, utm_epsg) %>% sf::st_line_sample(density = 1 / 100) %>% sf::st_cast("POINT")
  gap_km <- as.numeric(sf::st_distance(pts, sf::st_transform(line_b, utm_epsg))) / 1000
  summary <- list(med_pct = median(cells$pct_diff),
                  wet_km2_a = sum(cells$km2[cells$a_mm >= threshold_mm]),
                  wet_km2_b = sum(cells$km2[cells$b_mm >= threshold_mm]),
                  switch_km2 = sum(cells$km2[(cells$a_mm >= threshold_mm) != (cells$b_mm >= threshold_mm)]),
                  gap_med = median(gap_km), gap_max = max(gap_km))
  list(cells = cells, gap_km = gap_km, summary = summary)
}

#' Water-year (Nov-Oct) rain totals for every grid cell of a daily parquet store
#'
#' @param query arrow query on the store, already filtered (e.g. variable == "pr", scenario == "observed").
#' @return data.table: `cell_id`, `wy`, `mm` (sum of available days), `n` (days), `n_na` (missing days).
hi_wy_grid <- function(query) {
  query %>%
    dplyr::mutate(wy = as.integer(year) + dplyr::if_else(as.integer(month) >= 11L, 1L, 0L)) %>%
    dplyr::group_by(cell_id, wy) %>%
    dplyr::summarise(mm = sum(value, na.rm = TRUE), n = dplyr::n(), n_na = sum(is.na(value))) %>%
    dplyr::collect() %>%
    data.table::as.data.table()
}

#' Raster of mean water-year rain (mm) over a set of water years
#'
#' @param wy_grid output of `hi_wy_grid()`; `cells` the store's `_cells.parquet` (cell_id, lon, lat).
hi_wy_raster <- function(wy_grid, cells, wys) {
  m <- wy_grid[wy %in% wys, .(pr_mm = mean(mm)), by = cell_id]
  merge(data.table::as.data.table(cells)[, .(cell_id, lon, lat)], m, by = "cell_id")[, .(lon, lat, pr_mm)] %>%
    as.data.frame() %>%
    terra::rast(type = "xyz", crs = "EPSG:4326")
}
