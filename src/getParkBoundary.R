#' Import park boundary shapefile
#' 
#' This function uses the Data Store REST Api to download national park boundary 
#' shapefiles. 
#' Use this resource to get updated REST URL: https://irmaservices.nps.gov/datastore/v6/documentation/datastore-api.html#/Search_By_Reference_Code/ReferenceCodeSearch_Get
#' And this to find updated download links: https://irma.nps.gov/DataStore/Reference/Profile/2224545?lnv=True 
#' 
#' @param park The 4 digit national park code(s) for parks of interest
#' @param save Whether to save (TRUE) the resulting shapefile or not (FALSE)
#' @param path If `save = TRUE`, the file path to save the shapefile
#' 
#' @return A spatial sf object for each specified park boundary
getParkBoundary <- function(park, save = FALSE, path = NULL){
  
  parks <- getParkSystem()
  
  park_boundary <- parks %>% 
    sf::st_make_valid() %>%
    dplyr::filter(UNIT_CODE %in% park) %>%
    dplyr::group_by(UNIT_CODE,REGION,STATE,UNIT_NAME,PARKNAME) %>%
    dplyr::summarize(.groups = "keep")
  
  if(save == TRUE){
    
    for (i in 1:length(park)){
      
      #create park folder if does not already exist in 'path'
      if (!dir.exists(paste0(path, park[i]))) {
        
        dir.create(paste0(path, park[i]))
      }
      
      # save park boundary in park folder
      sf::write_sf(filter(park_boundary, UNIT_CODE == park[i]), 
                   paste0(path, "/", park[i], "/", park[i], "_park_boundary.shp"))
      
      
    }
    
    print(paste("Park boundary shapefiles saved to respective folders in:", paste0(getwd(), "/", path)))
    
  }
  
  return(park_boundary)
  
}