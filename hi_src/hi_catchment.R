#' Catchment area that a fixed storage needs to meet a constant daily demand
#'
#' Runs the daily yield-after-spillage tank model (`hi_tank_yas()`) over a grid of
#' catchment areas for every rainfall series and demand, and returns the smallest area
#' with no shortfall on any day of the record. Works for any site and storage.
#'
#' @param P matrix of daily rainfall (mm), one named column per series (e.g. "Historical").
#' @param storage_gal storage volume (gallons).
#' @param gpd constant daily demands (gallons per day).
#' @param acres catchment areas to try (acres), increasing.
#' @param treated_gal,filter_gpd optional treatment step (see `hi_tank_yas()`).
#' @return data.table: `CF` (column name of `P`), `gpd`, `acres_100` (NA if no area in `acres` suffices).
hi_catchment_needed <- function(P, storage_gal, gpd, acres, eff = 0.8, m2_per_acre = 4046.856,
                                treated_gal = NULL, filter_gpd = Inf) {
  DM <- sapply(gpd, function(g) rep(g, nrow(P)))
  sims <- data.table::CJ(series_id = seq_len(ncol(P)), dem_id = seq_along(gpd), acres = acres)
  sims[, `:=`(area_m2 = acres * m2_per_acre, tank_gal = storage_gal)]
  sims <- hi_tank_yas(P, DM, sims, eff = eff, treated_gal = treated_gal, filter_gpd = filter_gpd)
  sims[, .(acres_100 = hi_smallest_meeting(acres, rel_time, 100, tol = 1e-9)),
       by = .(CF = colnames(P)[series_id], gpd = gpd[dem_id])]
}
