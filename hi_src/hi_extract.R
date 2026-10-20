#' Site extraction and climate-future selection for NEX-HIDCP-CMIP6 stores
#'
#' Works with any parquet store written by `get_hcdp_obs_park()` /
#' `get_nex_hidcp_park()` (hive-partitioned `variable/scenario/gcm/`, plus
#' `_cells.parquet`) and any set of site points in Hawaiʻi.

#' Grid cells for each site: the nearest cell and its n x n index neighborhood
#'
#' @param pts sf points with an ID column `id_col`.
#' @param cells data frame from `_cells.parquet` (cell_id, lon, lat, lon_index, lat_index).
#' @param n neighborhood width in cells (odd; 3 gives a 3 x 3 block, ~750 m at 250 m).
#' @return data.table with columns `id_col` and `cell_id`.
hi_site_cells <- function(pts, cells, id_col = "system", n = 3) {
  xy <- sf::st_coordinates(sf::st_transform(pts, 4326))
  nearest <- vapply(seq_len(nrow(xy)), function(i) {
    cells$cell_id[which.min((cells$lon - xy[i, 1])^2 + (cells$lat - xy[i, 2])^2)]
  }, cells$cell_id[1])
  center <- cells[match(nearest, cells$cell_id), c("lon_index", "lat_index")]
  h <- (n - 1) / 2
  offsets <- expand.grid(dlon = -h:h, dlat = -h:h)
  out <- purrr::map_dfr(seq_len(nrow(pts)), function(i) {
    tibble::tibble(id = pts[[id_col]][i],
                   lon_index = center$lon_index[i] + offsets$dlon,
                   lat_index = center$lat_index[i] + offsets$dlat)
  }) %>%
    dplyr::inner_join(dplyr::select(cells, cell_id, lon_index, lat_index), by = c("lon_index", "lat_index"))
  out <- data.table::as.data.table(out)[, .(id, cell_id)]
  data.table::setnames(out, "id", id_col)
  out
}

#' Daily site precipitation = mean of each site's cells
#'
#' @param query an arrow query on the store (already filtered to the rows wanted).
#' @param site_cells output of `hi_site_cells()`.
hi_site_daily <- function(query, site_cells, id_col = "system") {
  raw <- query %>%
    dplyr::select(cell_id, scenario, gcm, date, year, month, day, value) %>%
    dplyr::collect() %>%
    data.table::as.data.table()
  raw[site_cells, on = "cell_id", allow.cartesian = TRUE, nomatch = NULL][
    , .(pr = mean(value, na.rm = TRUE)), by = c(id_col, "scenario", "gcm", "year", "month", "day", "date")]
}

#' Change in mean annual precipitation for every GCM x SSP against the observed baseline
#'
#' Annual totals are mean daily values x 365.25, because the GCMs use 360-day,
#' 365-day and Gregorian calendars.
#' @return list: `annual` (per site, series, year), `base` (observed mean), `chg` (% change per site and projection).
hi_annual_change <- function(daily, fut_years, ssp_ids, id_col = "system") {
  annual <- daily[, .(annual_mm = 365.25 * mean(pr)), by = c(id_col, "scenario", "gcm", "year")]
  base <- annual[scenario == "observed", .(base_mm = mean(annual_mm)), by = id_col]
  fut <- annual[scenario %in% ssp_ids & year %in% fut_years,
                .(fut_mm = mean(annual_mm), n_years = .N), by = c(id_col, "scenario", "gcm")]
  chg <- merge(fut, base, by = id_col)[, pct := 100 * (fut_mm / base_mm - 1)]
  list(annual = annual, base = base, chg = chg)
}

#' Drier and Wetter futures: the projections with the largest decrease and increase
#' in mean annual precipitation, averaged over all sites (NPS climate-futures approach)
#'
#' @param ssps named vector mapping scenario IDs to labels, e.g. c(ssp245 = "SSP2-4.5").
#' @return list: `park_chg` (every projection, flagged), `cf_sel`, `cf_key`, `cf_lab`.
hi_select_futures <- function(chg, ssps) {
  park_chg <- chg[, .(pct = mean(pct), n_years = min(n_years)), by = .(scenario, gcm)][order(pct)]
  cf_sel <- rbind(park_chg[1], park_chg[nrow(park_chg)])[, CF := c("Drier", "Wetter")]
  park_chg[, CF := "Other"][cf_sel, CF := i.CF, on = c("scenario", "gcm")]
  park_chg[, ssp := ssps[scenario]]
  list(park_chg = park_chg, cf_sel = cf_sel,
       cf_key = cf_sel[, .(scenario, gcm, CF)],
       cf_lab = setNames(paste0(cf_sel$gcm, " (", ssps[cf_sel$scenario], ")"), cf_sel$CF))
}

#' The same selection applied separately at each site, compared with the all-site pick
#'
#' @return list: `site_chg` (with rank, 1 = driest), `site_sel` (per site and future, with the
#'   runner-up and whether it matches `cf_sel`), `park_rk` (rank of the all-site picks at each
#'   site) and `site_cor` (correlation of annual change between sites across projections).
hi_select_futures_by_site <- function(chg, cf_sel, ssps, id_col = "system") {
  site_chg <- chg[, c(id_col, "scenario", "gcm", "pct"), with = FALSE]
  site_chg[, `:=`(rk = rank(pct), ssp = unname(ssps[scenario])), by = id_col]
  pick <- function(dir) {
    site_chg[, {
      o <- order(if (dir == "Drier") pct else -pct)
      .(scenario = scenario[o[1]], gcm = gcm[o[1]], ssp = ssp[o[1]], pct = pct[o[1]],
        next_gcm = gcm[o[2]], next_ssp = ssp[o[2]], next_pct = pct[o[2]])
    }, by = id_col][, CF := dir]
  }
  site_sel <- merge(rbind(pick("Drier"), pick("Wetter")),
                    cf_sel[, .(CF, park_scenario = scenario, park_gcm = gcm)], by = "CF")
  site_sel[, same := scenario == park_scenario & gcm == park_gcm]
  park_rk <- merge(site_chg, cf_sel[, .(scenario, gcm, CF)], by = c("scenario", "gcm"))[, c(id_col, "CF", "rk"), with = FALSE]
  wide <- data.table::dcast(site_chg, stats::as.formula(paste("scenario + gcm ~", id_col)), value.var = "pct")
  site_cor <- cor(as.matrix(wide[, -(1:2)]))
  list(site_chg = site_chg, site_sel = site_sel, park_rk = park_rk, site_cor = site_cor)
}

#' Climate futures by principal component analysis (NPS individual-projection method)
#'
#' Runyon et al. (2024) select the most divergent projections from the first two
#' principal components of the projected change values. This follows the PCA step
#' of `select_cfs_deep_dive()` in the nps_water_vulnerability project:
#' `prcomp(center = TRUE, scale. = TRUE)` on the two change variables, then the
#' projections at the minimum and maximum of each component. (That function also
#' drops low-skill models using a CMIP5 skill table, which does not cover CMIP6.)
#'
#' With two precipitation variables, one component runs along "wetter in both"
#' (loadings of the same sign) and the other along the contrast between them.
#' Each component is oriented so its loading on `x` is positive. Drier and Wetter
#' are the minimum and maximum of the same-sign component; the ends of the other
#' component are returned as "Low"/"High" plus its name.
#'
#' @param ens data.table, one row per projection, with `id_cols`, `x` and `y`.
#' @return list: `pca` (prcomp object), `rotation` (oriented loadings), `scores`
#'   (`ens` plus oriented component scores), `wet_pc` (the same-sign component),
#'   `other_pc`, `var_pct` (% of variance per component), `sel` (the four selected
#'   projections with column `pick`).
hi_select_futures_pca <- function(ens, x = "wet_pct", y = "dry_pct") {
  ens <- data.table::as.data.table(ens)
  pca <- stats::prcomp(as.matrix(ens[, c(x, y), with = FALSE]), center = TRUE, scale. = TRUE)
  sgn <- ifelse(pca$rotation[x, ] < 0, -1, 1)
  rot <- sweep(pca$rotation, 2, sgn, `*`)
  sc <- sweep(pca$x, 2, sgn, `*`)
  wet_pc <- colnames(rot)[sign(rot[x, ]) == sign(rot[y, ])][1]
  other_pc <- setdiff(colnames(rot), wet_pc)
  scores <- cbind(ens, sc)
  ends <- function(pc, lo, hi) rbind(scores[which.min(scores[[pc]])][, pick := lo],
                                     scores[which.max(scores[[pc]])][, pick := hi])
  sel <- rbind(ends(wet_pc, "Drier", "Wetter"), ends(other_pc, paste("Low", other_pc), paste("High", other_pc)))
  list(pca = pca, rotation = rot, scores = scores, wet_pc = wet_pc, other_pc = other_pc,
       var_pct = setNames(100 * pca$sdev^2 / sum(pca$sdev^2), colnames(rot)), sel = sel)
}
