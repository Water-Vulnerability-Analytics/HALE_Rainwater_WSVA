#' Rainwater tank and catchment models (any roof or catchment, any daily rainfall series)
#'
#' Daily yield-after-spillage (YAS) behavioural model (Fewkes and Butler 2000;
#' Mitchell 2007): Y_t = min(D_t, V_t-1); V_t = min(V_t-1 + Q_t, S) - Y_t,
#' starting with a full tank.

#' Run many tank simulations at once
#'
#' @param P matrix of daily rainfall (mm): one column per rainfall series, rows are days.
#' @param DM matrix of daily demand (gallons): one column per demand series, same rows as `P`.
#' @param sims data.table with one row per simulation and columns `series_id` (column of `P`),
#'   `dem_id` (column of `DM`), `tank_gal` and `area_m2`.
#' @param eff collection efficiency (share of rain on the catchment that reaches the tank).
#' @return `sims` with `rel_time` (% of days demand fully met) and `rel_vol` (% of demand volume supplied).
hi_tank_yas <- function(P, DM, sims, eff = 0.8, gal_per_liter = 0.264172) {
  s <- sims$series_id; d <- sims$dem_id; S <- sims$tank_gal
  qf <- sims$area_m2 * eff * gal_per_liter          # gallons per mm of rain
  V <- S; met <- numeric(nrow(sims)); ysum <- numeric(nrow(sims)); dsum <- numeric(nrow(sims))
  for (t in seq_len(nrow(P))) {
    D <- DM[t, d]
    Y <- pmin(D, V)
    V <- pmin(V + P[t, s] * qf, S) - Y
    met <- met + (Y >= D - 1e-9); ysum <- ysum + Y; dsum <- dsum + D
  }
  sims[, `:=`(rel_time = 100 * met / nrow(P), rel_vol = 100 * ysum / dsum)]
}

#' Smallest size (tank volume or catchment area) whose reliability meets `target`
hi_smallest_meeting <- function(size, rel, target, tol = 0) {
  ok <- rel >= target - tol
  if (any(ok)) min(size[ok]) else NA_real_
}

#' Days on which a single tank falls short of a constant daily demand
hi_shortfall_days <- function(p_mm, area_m2, S, D, eff = 0.8, gal_per_liter = 0.264172) {
  V <- S; short <- logical(length(p_mm))
  for (t in seq_along(p_mm)) {
    Y <- min(D, V); short[t] <- Y < D - 1e-9
    V <- min(V + p_mm[t] * area_m2 * eff * gal_per_liter, S) - Y
  }
  short
}

#' Sequent-peak storage requirement from inflow and demand series (same units)
#'
#' K_t = max(0, K_t-1 + D_t - Q_t); required storage = max K_t. The record is run twice so a
#' deficit spanning its end is counted. Returns Inf if demand exceeds inflow over the record.
#' Monthly steps are adequate only where the storage fraction S/(AR) exceeds 0.125
#' (Fewkes and Butler 2000).
hi_sequent_peak <- function(Q, D) {
  if (sum(D) >= sum(Q)) return(Inf)
  n <- length(Q); K <- 0; K_max <- 0
  for (t in seq_len(2 * n)) {
    i <- (t - 1) %% n + 1
    K <- max(0, K + D[i] - Q[i]); K_max <- max(K_max, K)
  }
  K_max
}
