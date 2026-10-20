#' Geologic and ecological setting of sites in Hawaiʻi
#'
#' Looks up, at each point: geologic unit (Sherrod et al. 2007), NRCS soil map unit
#' (SSURGO), CWRM aquifer sector and system, watershed, moku (DAR), moisture zone
#' (Price et al. 2012) and land cover (Jacobi et al. 2017) from the Hawaiʻi Statewide
#' GIS Program, plus elevation from USGS. Needs internet the first time; the result is
#' cached to `cache` so later runs are offline.
#'
#' @param pts sf points with an ID column `id_col`.
#' @return data.frame, one row per point.
hi_site_setting <- function(pts, id_col, cache) {
  hi_cache(cache, {
    pts <- sf::st_transform(pts, 4326)
    xy <- sf::st_coordinates(pts)
    gis_at <- function(layer, lon, lat) {
      q <- c(geometry = paste(lon, lat, sep = ","), geometryType = "esriGeometryPoint", inSR = "4326",
             spatialRel = "esriSpatialRelIntersects", outFields = "*", returnGeometry = "false", f = "json")
      url <- paste0("https://geodata.hawaii.gov/arcgis/rest/services/", layer, "/query?",
                    paste0(names(q), "=", curl::curl_escape(q), collapse = "&"))
      f <- jsonlite::fromJSON(url)$features
      if (length(f) == 0) list() else as.list(f$attributes[1, , drop = FALSE])
    }
    pick <- function(x, col) if (is.null(x[[col]]) || is.na(x[[col]])) NA_character_ else as.character(x[[col]])
    out <- purrr::map_dfr(seq_len(nrow(pts)), function(i) {
      lon <- xy[i, 1]; lat <- xy[i, 2]
      g  <- gis_at("Terrestrial/MapServer/28", lon, lat)       # geologic units
      so <- gis_at("Terrestrial/MapServer/42", lon, lat)       # NRCS soils
      aq <- gis_at("FreshWater/MapServer/3", lon, lat)         # CWRM aquifer sectors/systems
      ws <- gis_at("FreshWater/MapServer/8", lon, lat)         # watersheds
      mk <- gis_at("FreshWater/MapServer/21", lon, lat)        # moku
      mz <- gis_at("LandUseLandCover/MapServer/6", lon, lat)   # moisture zones
      lc <- gis_at("LandUseLandCover/MapServer/16", lon, lat)  # CAH land cover
      tibble::tibble(id = pts[[id_col]][i], lon = lon, lat = lat,
                     geo_name = pick(g, "name"), geo_member = pick(g, "name0"), geo_age = pick(g, "age_range"),
                     geo_rock = pick(g, "rock_type"), geo_lith = pick(g, "lithology"),
                     mukey = pick(so, "mukey"), aquifer_sector = pick(aq, "sector"), aquifer_system = pick(aq, "system"),
                     gwma = pick(aq, "gwma"), watershed = pick(ws, "name_hwn"), swma = pick(ws, "swma"),
                     moku = pick(mk, "moku"), moisture_zone = pick(mz, "moisturezone"),
                     land_cover = pick(lc, "detailed_lc_unit"), habitat_status = pick(lc, "habitat_status"))
    })
    names(out)[1] <- id_col
    sda <- httr::POST("https://SDMDataAccess.sc.egov.usda.gov/Tabular/post.rest", encode = "json",
                      body = list(query = paste0("SELECT mukey, muname FROM mapunit WHERE mukey IN ('",
                                                 paste(unique(out$mukey), collapse = "','"), "')"),
                                  format = "JSON+COLUMNNAME"))
    soil_tab <- jsonlite::fromJSON(httr::content(sda, "text", encoding = "UTF-8"))$Table
    out$soil <- unname(setNames(soil_tab[-1, 2], soil_tab[-1, 1])[out$mukey])
    out$elev_ft <- round(3.28084 * elevatr::get_elev_point(sf::st_as_sf(out, coords = c("lon", "lat"), crs = 4326),
                                                            src = "epqs")$elevation)
    out
  })
}
