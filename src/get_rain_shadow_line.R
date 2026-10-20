#' Rain-shadow line across a park from NEX-HIDCP-CMIP6 precipitation
#'
#' Builds a mean annual rainfall map from the observed HCDP rainfall pulled by
#' `get_hcdp_obs_park()` (the data NEX-HIDCP was trained on), averaged over
#' every year on disk, and returns the isohyet (line of equal rainfall) at
#' `threshold_mm`. If no observations are on disk it uses the NEX-HIDCP
#' historical hindcasts, which BCSD tunes to the same climatology. The isohyet
#' marks where the wet windward side gives way to the dry rain-shadow side.
#'
#' At HALE rainfall falls from 5,000-9,000 mm/yr at Kipahulu to ~900 mm in the
#' crater and leeward slopes, and most of that drop happens in a narrow band
#' near 156.15 W; the 2,000 mm isohyet sits in the middle of it.
#'
#' @param nex_dir Folder written by `get_hcdp_obs_park()` / `get_nex_hidcp_park()`,
#'   e.g. "hi_datasets/HALE".
#' @param threshold_mm Annual rainfall (mm) that defines the line.
#' @param main_only If `TRUE`, keep only the longest piece of the isohyet (the
#'   main windward/leeward divide) and drop small closed loops elsewhere.
#'
#' @param years Years to average (all on disk if `NULL`).
#' @return A list with `line` (sf LINESTRING/MULTILINESTRING, EPSG:4326) and
#'   `rainfall` (SpatRaster of mean annual rainfall, mm).
get_rain_shadow_line <- function(nex_dir, threshold_mm = 2000, main_only = TRUE, years = NULL){

  cells <- arrow::read_parquet(file.path(nex_dir, "_cells.parquet"))

  # Prefer the HCDP observations NEX-HIDCP was trained on
  # (`get_hcdp_obs_park()`); fall back to the GCM hindcasts if they aren't there.
  ds <- arrow::open_dataset(nex_dir)
  scenarios_on_disk <- ds %>% dplyr::distinct(scenario) %>% dplyr::collect() %>% dplyr::pull(scenario)
  use_scenario <- if ("observed" %in% scenarios_on_disk) "observed" else "historical"
  source_label <- if (use_scenario == "observed") "HCDP observed mean" else "NEX-HIDCP historical mean"

  # Annual totals per source and year, then the average over all of them.
  q <- ds %>% dplyr::filter(variable == "pr", scenario == use_scenario)
  if (!is.null(years)) q <- q %>% dplyr::filter(year %in% !!years)
  annual <- q %>%
    dplyr::group_by(gcm, year, cell_id) %>%
    dplyr::summarize(pr = sum(value)) %>%
    dplyr::group_by(cell_id) %>%
    dplyr::summarize(pr_mm = mean(pr)) %>%
    dplyr::collect()

  rainfall <- cells %>%
    dplyr::inner_join(annual, by = "cell_id") %>%
    dplyr::select(lon, lat, pr_mm) %>%
    as.data.frame() %>%
    terra::rast(type = "xyz", crs = "EPSG:4326")

  line <- isohyet_line(rainfall, threshold_mm, main_only, source = source_label)

  list(line = line, rainfall = rainfall)
}


#' Isohyet (line of equal rainfall) from a rainfall raster
#'
#' @param rainfall SpatRaster of annual rainfall (mm), e.g. from
#'   `get_rain_shadow_line()` or the Rainfall Atlas `rf_mm_ma_ann` grid.
#' @param threshold_mm Rainfall value (mm) to trace.
#' @param main_only If `TRUE`, keep only the longest piece of the line.
#' @param source Short label for the rainfall data, stored with the line.
#'
#' @return An sf line (EPSG:4326) with `isohyet_mm` and `description`.
isohyet_line <- function(rainfall, threshold_mm = 2000, main_only = TRUE, source = ""){

  line <- sf::st_as_sf(terra::as.contour(rainfall, levels = threshold_mm))

  if (main_only) {
    pieces <- sf::st_cast(sf::st_geometry(line), "LINESTRING")
    line <- sf::st_sf(geometry = pieces[which.max(sf::st_length(pieces))])
  }

  line %>%
    sf::st_transform(4326) %>%
    dplyr::mutate(isohyet_mm = threshold_mm,
                  description = paste0("Rain shadow: ", threshold_mm, " mm/yr isohyet (", source, ")")) %>%
    dplyr::select(isohyet_mm, description)
}
