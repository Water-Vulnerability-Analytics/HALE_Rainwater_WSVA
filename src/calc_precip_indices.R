#' Yearly rainfall statistics from daily precipitation
#'
#' Summarizes daily rainfall into one row per site and Hawaii water year
#' (November-October, labeled by the year it ends, so water year 2020 is
#' Nov 2019-Oct 2020). The wet season is November-April and the dry season
#' May-October, as used by the Hawaii Climate Data Portal. Indices follow the
#' ETCCDI definitions where one exists.
#'
#' Days with no value are never counted as dry, so missing data can only
#' shorten a dry spell, not lengthen it. Years with less than `min_complete`
#' of their days observed are dropped. By default totals and day counts sum
#' the observed days only, so a missing day adds nothing (as if dry); with
#' `scale_missing = TRUE` they are scaled up by the share of days observed
#' (seasonal totals by the share observed in that season), which keeps years
#' with more gaps from looking drier.
#'
#' @param daily Data frame with `site`, `date` and `prcp_mm` columns (e.g. GHCN
#'   gauge data, or park-mean NEX-HIDCP / HCDP series).
#' @param wet_day_mm Rain (mm) at or above which a day counts as wet. 1 mm is
#'   the ETCCDI convention.
#' @param heavy_day_mm Threshold for a heavy-rain day (25.4 mm = 1 inch, the
#'   R25 index used for Hawaii by Chu et al. 2010).
#' @param min_complete Minimum share of days with data for a year to be kept.
#' @param scale_missing If TRUE, scale totals and wet/heavy-day counts to a
#'   full year (or season) by the share of days observed.
#'
#' @return A tibble with one row per `site` and `water_year`:
#'   `annual_mm`, `wet_season_mm` (Nov-Apr), `dry_season_mm` (May-Oct),
#'   `cdd` (longest run of dry days in the water year), `cdd_dry_season`,
#'   `wet_days`, `heavy_days`, `rx1day_mm` (wettest day), `rx5day_mm`
#'   (wettest 5 consecutive days), `sdii_mm` (mean rain per wet day),
#'   `pct_from_heavy_days` (share of the year's rain that fell on heavy days)
#'   `pct_complete` and `n_missing` (days with no value).
calc_precip_indices <- function(daily,
                                wet_day_mm = 1,
                                heavy_day_mm = 25.4,
                                min_complete = 0.9,
                                scale_missing = FALSE){

  # Longest run of TRUE values; NA (missing) breaks a run.
  longest_run <- function(x) {
    x[is.na(x)] <- FALSE
    r <- rle(x)
    if (any(r$values)) max(r$lengths[r$values]) else 0L
  }

  daily %>%
    dplyr::mutate(month = as.integer(format(date, "%m")),
                  water_year = as.integer(format(date, "%Y")) + (month >= 11),
                  season = ifelse(month %in% c(11, 12, 1:4), "wet", "dry")) %>%
    dplyr::arrange(site, date) %>%
    dplyr::group_by(site, water_year) %>%
    dplyr::summarize(
      n_days = dplyr::n(),
      pct_complete = mean(!is.na(prcp_mm)),
      n_missing = sum(is.na(prcp_mm)),
      f_wet = mean(!is.na(prcp_mm[season == "wet"])),
      f_dry = mean(!is.na(prcp_mm[season == "dry"])),
      annual_mm = sum(prcp_mm, na.rm = TRUE),
      wet_season_mm = sum(prcp_mm[season == "wet"], na.rm = TRUE),
      dry_season_mm = sum(prcp_mm[season == "dry"], na.rm = TRUE),
      cdd = longest_run(prcp_mm < wet_day_mm),
      cdd_dry_season = longest_run(prcp_mm[season == "dry"] < wet_day_mm),
      wet_days = sum(prcp_mm >= wet_day_mm, na.rm = TRUE),
      heavy_days = sum(prcp_mm >= heavy_day_mm, na.rm = TRUE),
      rx1day_mm = suppressWarnings(max(prcp_mm, na.rm = TRUE)),
      rx5day_mm = suppressWarnings(max(zoo::rollsum(prcp_mm, 5, na.rm = TRUE), na.rm = TRUE)),
      sdii_mm = mean(prcp_mm[prcp_mm >= wet_day_mm], na.rm = TRUE),
      pct_from_heavy_days = 100 * sum(prcp_mm[prcp_mm >= heavy_day_mm], na.rm = TRUE) / annual_mm,
      .groups = "drop"
    ) %>%
    # A full water year has 365-366 days; drop partial and gappy years.
    dplyr::filter(n_days >= 365, pct_complete >= min_complete) %>%
    # Optionally scale totals and counts to what a full year (or season) would hold
    dplyr::mutate(
      annual_mm = if (scale_missing) annual_mm / pct_complete else annual_mm,
      wet_season_mm = if (scale_missing) wet_season_mm / f_wet else wet_season_mm,
      dry_season_mm = if (scale_missing) dry_season_mm / f_dry else dry_season_mm,
      wet_days = if (scale_missing) wet_days / pct_complete else wet_days,
      heavy_days = if (scale_missing) heavy_days / pct_complete else heavy_days,
      pct_complete = round(100 * pct_complete, 1)) %>%
    dplyr::select(-n_days, -f_wet, -f_dry)
}
