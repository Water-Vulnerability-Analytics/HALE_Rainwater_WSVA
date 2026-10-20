#' Rainwater tank and catchment models (any roof or catchment, any daily rainfall series)
#'
#' Daily yield-after-spillage (YAS) behavioural model (Fewkes and Butler 2000;
#' Mitchell 2007): Y_t = min(D_t, V_t-1); V_t = min(V_t-1 + Q_t, S) - Y_t,
#' starting with a full tank.
#'
#' Optional treatment step: with `treated_gal` set, storage `S` is split into raw-water tanks
#' (S - treated_gal) and a treated-water tank (treated_gal). Each day demand is drawn from the
#' treated tank, the catch goes into the raw tanks (spilling above their capacity), and the
#' filters move up to `filter_gpd` from the raw tanks into the treated tank. Both start full.
#' With demand at or below `filter_gpd` this gives the same result as the single tank.

#' Run many tank simulations at once
#'
#' @param P matrix of daily rainfall (mm): one column per rainfall series, rows are days.
#' @param DM matrix of daily demand (gallons): one column per demand series, same rows as `P`.
#' @param sims data.table with one row per simulation and columns `series_id` (column of `P`),
#'   `dem_id` (column of `DM`), `tank_gal` and `area_m2`.
#' @param eff collection efficiency (share of rain on the catchment that reaches the tank).
#' @param treated_gal treated-water storage within `tank_gal` (gallons); NULL for a single tank.
#' @param filter_gpd most water the filters can treat in a day (gallons).
#' @return `sims` with `rel_time` (% of days demand fully met) and `rel_vol` (% of demand volume supplied).
hi_tank_yas <- function(P, DM, sims, eff = 0.8, gal_per_liter = 0.264172, treated_gal = NULL, filter_gpd = Inf) {
  s <- sims$series_id; d <- sims$dem_id; S <- sims$tank_gal
  qf <- sims$area_m2 * eff * gal_per_liter          # gallons per mm of rain
  met <- numeric(nrow(sims)); ysum <- numeric(nrow(sims)); dsum <- numeric(nrow(sims))
  if (is.null(treated_gal)) {
    V <- S
    for (t in seq_len(nrow(P))) {
      D <- DM[t, d]
      Y <- pmin(D, V)
      V <- pmin(V + P[t, s] * qf, S) - Y
      met <- met + (Y >= D - 1e-9); ysum <- ysum + Y; dsum <- dsum + D
    }
  } else {
    St <- pmin(treated_gal, S); Sr <- S - St; Tt <- St; R <- Sr
    for (t in seq_len(nrow(P))) {
      D <- DM[t, d]
      Y <- pmin(D, Tt); Tt <- Tt - Y                 # supply from the treated tank
      R <- pmin(R + P[t, s] * qf, Sr)                # catch into the raw tanks; spill
      Fl <- pmin(filter_gpd, R, St - Tt); R <- R - Fl; Tt <- Tt + Fl   # filter into the treated tank
      met <- met + (Y >= D - 1e-9); ysum <- ysum + Y; dsum <- dsum + D
    }
  }
  sims[, `:=`(rel_time = 100 * met / nrow(P), rel_vol = 100 * ysum / dsum)]
}

#' Smallest size (tank volume or catchment area) whose reliability meets `target`
hi_smallest_meeting <- function(size, rel, target, tol = 0) {
  ok <- rel >= target - tol
  if (any(ok)) min(size[ok]) else NA_real_
}

#' Day-by-day storage and shortfalls for one rain series and a constant demand
#'
#' Same model as `hi_tank_yas()` (single tank, or raw + treated tanks with a filter limit).
#' @return data.frame with `storage` (gallons at the end of each day, all tanks) and `short`
#'   (TRUE on days demand was not fully met).
hi_tank_trace <- function(p_mm, area_m2, S, D, eff = 0.8, gal_per_liter = 0.264172, treated_gal = NULL, filter_gpd = Inf) {
  q <- area_m2 * eff * gal_per_liter
  St <- if (is.null(treated_gal)) S else min(treated_gal, S); Sr <- S - St
  Tt <- St; R <- Sr; v <- numeric(length(p_mm)); short <- logical(length(p_mm))
  for (t in seq_along(p_mm)) {
    if (is.null(treated_gal)) {
      Y <- min(D, Tt); Tt <- min(Tt + p_mm[t] * q, S) - Y
    } else {
      Y <- min(D, Tt); Tt <- Tt - Y
      R <- min(R + p_mm[t] * q, Sr)
      Fl <- min(filter_gpd, R, St - Tt); R <- R - Fl; Tt <- Tt + Fl
    }
    short[t] <- Y < D - 1e-9; v[t] <- R + Tt
  }
  data.frame(storage = v, short = short)
}

#' Days on which the tanks fall short of a constant daily demand
hi_shortfall_days <- function(p_mm, area_m2, S, D, eff = 0.8, gal_per_liter = 0.264172, treated_gal = NULL, filter_gpd = Inf) {
  hi_tank_trace(p_mm, area_m2, S, D, eff, gal_per_liter, treated_gal, filter_gpd)$short
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
