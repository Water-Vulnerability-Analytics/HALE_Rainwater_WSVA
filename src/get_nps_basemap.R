#' Download the NPS Park Tiles basemap for an area
#'
#' Park Tiles is the National Park Service's own basemap
#' (https://www.nps.gov/maps/tools/park-tiles/). NPS serves it from its Atlas
#' tile server; the style IDs and public access token below come from NPS's
#' map library (npmap.js 4.2.1, the code behind the Park Tiles viewer). If NPS
#' rotates the token, update `nps_token` from
#' https://www.nps.gov/lib/npmap.js/4.2.1/npmap.min.js (search "atlas-user").
#'
#' @param aoi sf object to cover, e.g. from `getParkBoundary()`.
#' @param style `"standard"`, `"light"` or `"slate"`.
#' @param buffer_km Margin around `aoi`, in km.
#' @param zoom Tile zoom level, or `NULL` to let maptiles choose.
#'
#' @return A SpatRaster (RGB, Web Mercator) for `geom_nps_basemap()`.
get_nps_basemap <- function(aoi, style = "standard", buffer_km = 2, zoom = NULL){

  style_ids <- c(standard = "ck58pyquo009v01p99xebegr9",
                 light = "ck5cpia2u0auf01p9vbugvcpv",
                 slate = "ck5cpvc2e0avf01p9zaw4co8o")
  nps_token <- "pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"

  # maptiles detects the image type from ".png" in the URL. The server rejects
  # extra query parameters, so ".png" goes in a URL fragment, which curl never
  # sends.
  provider <- maptiles::create_provider(
    name = paste0("nps_park_tiles_", style),
    url = paste0("https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/",
                 style_ids[[style]], "/tiles/256/{z}/{x}/{y}?access_token=", nps_token,
                 "#.png"),
    citation = "NPS Park Tiles; (c) Mapbox (c) OpenStreetMap contributors"
  )

  # Tiles come back in the CRS of `area`; Web Mercator is their native CRS, so
  # no resampling, and it matches coord_sf(crs = 3857) in the plot.
  area <- sf::st_buffer(sf::st_union(sf::st_transform(aoi, 4326)), buffer_km * 1000) %>%
    sf::st_transform(3857) %>%
    sf::st_as_sf()

  if (is.null(zoom)) {
    maptiles::get_tiles(area, provider = provider, crop = TRUE)
  } else {
    maptiles::get_tiles(area, provider = provider, zoom = zoom, crop = TRUE)
  }
}


#' ggplot layer for a basemap from `get_nps_basemap()`
#'
#' Draws the tiles as a background image, so other layers keep their own fill
#' and color scales. Add `coord_sf(crs = 3857)` to the plot, since the tiles
#' are in Web Mercator.
#'
#' @param tiles SpatRaster from `get_nps_basemap()`.
geom_nps_basemap <- function(tiles){

  rgb <- terra::as.array(tiles[[1:3]]) / 255
  e <- terra::ext(tiles)

  annotation_raster(grDevices::as.raster(rgb),
                    xmin = e$xmin, xmax = e$xmax, ymin = e$ymin, ymax = e$ymax)
}


#' Use NPS Park Tiles as the basemaps of a mapview or leaflet map
#'
#' Replaces mapview's default basemaps (CartoDB, OpenStreetMap, ...) with the
#' four Park Tiles styles and rebuilds the layer switcher, keeping the map's
#' own layers as toggleable overlays.
#'
#' @param m A mapview object (e.g. `mapview(a) + mapview(b)`) or leaflet map.
#' @param styles Park Tiles styles to offer, first one shown by default.
#'
#' @return The same kind of object as `m`.
add_nps_basemaps <- function(m, styles = c("standard", "light", "slate", "imagery")){

  style_ids <- c(standard = "ck58pyquo009v01p99xebegr9",
                 light = "ck5cpia2u0auf01p9vbugvcpv",
                 slate = "ck5cpvc2e0avf01p9zaw4co8o",
                 imagery = "ck72fwp2642dv07o7tbqinvz4")
  nps_token <- "pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"

  lm <- if (inherits(m, "mapview")) m@map else m

  # Keep the overlay names from the existing layer switcher, then drop
  # mapview's basemaps and that switcher.
  calls <- lm$x$calls
  control <- Filter(function(cl) cl$method == "addLayersControl", calls)
  overlays <- if (length(control) > 0) unlist(control[[length(control)]]$args[[2]]) else character(0)
  lm$x$calls <- Filter(function(cl) !cl$method %in% c("addProviderTiles", "addTiles", "addLayersControl"), calls)

  group_names <- paste("NPS", tools::toTitleCase(styles))

  for (i in rev(seq_along(styles))) {
    lm <- leaflet::addTiles(
      lm,
      urlTemplate = paste0("https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/",
                           style_ids[[styles[i]]], "/tiles/256/{z}/{x}/{y}?access_token=", nps_token),
      attribution = "NPS Park Tiles | &copy; Mapbox &copy; OpenStreetMap contributors",
      group = group_names[i]
    )
  }

  # Show only the first style at start so the others don't load until picked.
  lm <- leaflet::addLayersControl(lm, baseGroups = group_names, overlayGroups = overlays,
                                  options = leaflet::layersControlOptions(collapsed = TRUE)) %>%
    leaflet::hideGroup(group_names[-1])

  if (inherits(m, "mapview")) {
    m@map <- lm
    m
  } else {
    lm
  }
}
