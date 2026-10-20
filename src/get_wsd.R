#' Retrieve and Process NPS Water Supply Database (WSD)
#'
#' This function loads the NPS Water Supply Database, performs spatial cleaning,
#' and allows for filtering by geography and park unit.
#'
#' @param conus Logical. If TRUE (default), filters out non-contiguous states and territories.
#' @param fill_empty_coords Logical. If TRUE (default), fills missing source coordinates 
#' with the respective park unit's centroid.
#' @param park Character vector. Optional list of four-letter park unit codes (e.g., c("ROMO", "GRCA")).
#' @param sf Logical. If TRUE (default), returns the data as an sf spatial object (CRS: 4326).
#' @param rapid Logical. If TRUE, applies "Rapid" assessment presets: forces CONUS, 
#'   fills missing coords, and filters to NPS-operated systems only (non-muncipal/hauled/tribal/concession)
#'
#' @return A data frame or sf object containing processed water supply system data.
#' @export
get_wsd <- function(conus = TRUE,
                    fill_empty_coords = FALSE,
                    park = NULL,
                    sf = TRUE,
                    rapid = FALSE) {
  
  
  if (rapid == TRUE) {
    # override these args for rapid
    fill_empty_coords <- TRUE
    sf <- TRUE
    conus <- TRUE
  }
  
  # --- Error Checking: File Existence ---
  table_path <- "data/Water_Supply_Systems/NPS_Water_Systems_Database_Joined.xlsx"
  
  if (!file.exists(table_path)) {
    stop(paste("WSD file not found at:", table_path, 
               "\nPlease check your working directory or data path."))
  }
  
  # --- Load and Clean Water Supply Database ---
  WSD_raw <- readxl::read_excel(table_path, 
                                sheet = 1, 
                                na = c("NA", "U"), 
                                skip = 1) %>%
    janitor::clean_names() %>%
    dplyr::mutate(source_longitude = as.numeric(source_longitude),
                  source_latitude = as.numeric(source_latitude))
  
  if (rapid == TRUE) {
    WSD_raw <- WSD_raw %>%
      # Filter to CONUS & NPS-Operated Systems only
      dplyr::filter(!str_detect(source_type, 
                                regex("municipal|hauled|tribal|purchased",
                                      ignore_case = TRUE)))
  }
  
  # handle coordinate filling
  if (fill_empty_coords) {
    
    # Error checking for dependency
    if (!exists("getParkSystem")) {
      warning("Function 'getParkSystem()' not found. Skipping coordinate fill.")
    } else {
      parks <- getParkSystem()
      
      WSD_raw <- WSD_raw %>% 
        dplyr::mutate(coords_flag = ifelse(is.na(source_longitude) | is.na(source_latitude),
                                           "Centroid", "Source Coordinates"))
      
      # filter missing coords to only join those 
      WSD_missing <- WSD_raw %>% 
        filter(coords_flag == "Centroid")
      
      # nothing missing, nothing to fill
      if (nrow(WSD_missing) == 0) {
        
        message("No missing coordinates found — skipping centroid fill step.")
        
      } else {
        
        # get associated park centroid and join...
        units_poly <- parks %>% 
          filter(UNIT_CODE %in% WSD_missing$park_unit) %>% 
          st_cast("POLYGON") %>% 
          st_transform(5070)   # convert to meters projection
        
        # missing coords exist, but none matched a park polygon
        if (nrow(units_poly) == 0) {
          
          warning("No matching park units found for sites with missing coordinates — skipping centroid fill.")
          
        } else {
          
          # group by park and "split"
          exploded <- units_poly %>% 
            dplyr::group_split(UNIT_CODE)
          
          clustered <- exploded %>%
            map_dfr(function(p) {
              # If a park only has 1 polygon, don't waste time on it
              if(nrow(p) <= 1) {
                p$cluster_id <- 1
                return(p)
              }
              
              # use a simplified version of the geometry just for the distance check
              p_sim <- st_simplify(p, dTolerance = 5)
              
              nb <- sf::st_is_within_distance(p_sim, dist = 30000)
              
              g <- igraph::graph_from_adj_list(nb)
              p$cluster_id <- igraph::components(g)$membership
              return(p)
            })
          
          # within each cluster (per park)
          units_clustered <- clustered %>% 
            group_by(UNIT_CODE, cluster_id) %>% 
            summarize(do_union = TRUE, .groups = "drop") %>% 
            st_cast("MULTIPOLYGON") %>% 
            # st_transform(st_crs(getParkSystem())) %>%
            mutate(cluster_id = as.character(cluster_id)) %>%
            st_join(., tigris::states() %>% st_transform(st_crs(clustered)) %>% select(STUSPS), left = TRUE) %>%
            st_point_on_surface() %>%
            st_transform(4326)
          
          WSD_missing_1 <- WSD_missing %>%
            dplyr::filter(!is.na(source_state)) %>%
            dplyr::left_join(units_clustered, by = c("park_unit" = "UNIT_CODE", "source_state" = "STUSPS")) 
          
          WSD_missing_2 <- WSD_missing %>%
            dplyr::filter(is.na(source_state)) %>%
            dplyr::left_join(units_clustered, by = c("park_unit" = "UNIT_CODE", "state" = "STUSPS")) 
          
          # Sites with so not enough information to discern which of the units it should be assigned to...
          # test <- bind_rows(WSD_missing_1, WSD_missing_2) %>%
          #   group_by(wsd_source_id) %>%
          #   mutate(count = n()) %>%
          #   filter(count > 1)
          
          WSD_missing_complete <- bind_rows(WSD_missing_1, WSD_missing_2) %>%
            distinct(wsd_source_id, .keep_all = TRUE) %>%
            mutate(source_longitude = st_coordinates(geometry)[, 1],
                   source_latitude = st_coordinates(geometry)[, 2]) %>%
            st_drop_geometry() %>%
            select(-c(geometry, cluster_id))
          
          # join to rows that had coords
          WSD_raw <- WSD_raw %>%
            filter(coords_flag == "Source Coordinates") %>% 
            bind_rows(WSD_missing_complete)
          
        }
      }
      
    }
  }
  
  # --- Filters ---
  WSD <- WSD_raw
  
  if (!is.null(park)) {
    WSD <- WSD %>% dplyr::filter(park_unit %in% park)
  }
  
  if (conus) {
    # Territories/Oconus filters
    WSD <- WSD %>% 
      dplyr::filter(!state %in% c("AK", "AS", "HI", "MP", "PR", "VI", "GU")) 
  }
  
  # --- Return as Spatial or Flat ---
  if (sf) {
    # Final check: remove records that still have no coords after filling
    WSD <- WSD %>%
      tidyr::drop_na(source_longitude, source_latitude) %>%
      sf::st_as_sf(coords = c("source_longitude", "source_latitude"),
                   crs = 4326,
                   remove = FALSE)
  }
  
  return(WSD)
}