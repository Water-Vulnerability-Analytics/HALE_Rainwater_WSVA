#' Find NOAA GHCN-Daily stations within a distance of a park boundary
#'
#' Same station list as `getNOAA()`, but the boundary is buffered first so
#' stations just outside the park are included, and only stations that record
#' `element` are kept. Returns station metadata only; pass the `id`s to
#' `rnoaa::meteo_pull_monitors()` (or `getNOAA()`) to download data.
#'
#' @param aoi sf polygon(s), e.g. from `getParkBoundary()`.
#' @param buffer_km Distance outside the boundary to search, in km.
#' @param element GHCN-Daily element the station must record: `"PRCP"` for
#'   rain gauges, or `"TMAX"`/`"TMIN"`.
#' @param stations Optional table from `rnoaa::ghcnd_stations()`, to skip
#'   re-downloading it (~40 s).
#'
#' @return An sf point object with station `id`, `name`, elevation (m),
#'   `inside_park`, distance to the boundary (km, 0 inside), and the first and
#'   last year the station recorded `element`.
get_ghcnd_near_park <- function(aoi,
                                buffer_km = 10,
                                element = "PRCP",
                                stations = NULL){

  if (is.null(stations)) {
    stations <- rnoaa::ghcnd_stations()
  }

  # Work in lon/lat: sf's spherical geometry gives buffers and distances in true
  # meters, where a Web Mercator projection would stretch them.
  aoi_ll <- sf::st_union(sf::st_transform(aoi, 4326))
  search_area <- sf::st_buffer(aoi_ll, buffer_km * 1000)

  stations %>%
    dplyr::filter(.data$element == !!element) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
    sf::st_filter(search_area) %>%
    dplyr::mutate(
      inside_park = as.vector(sf::st_intersects(geometry, aoi_ll, sparse = FALSE)),
      dist_to_park_km = round(as.numeric(sf::st_distance(geometry, aoi_ll)) / 1000, 1)
    ) %>%
    dplyr::select(id, name, elevation, inside_park, dist_to_park_km,
                  first_year, last_year, latitude, longitude) %>%
    dplyr::arrange(dist_to_park_km)
}
