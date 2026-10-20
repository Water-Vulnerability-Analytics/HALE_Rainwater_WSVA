#' Import park boundary shapefile
#' 
#' This function uses the Data Store REST Api to download national park boundary 
#' shapefiles. 
#' Use this resource to get updated REST URL: https://irmaservices.nps.gov/datastore/v6/documentation/datastore-api.html#/Search_By_Reference_Code/ReferenceCodeSearch_Get
#' And this to find updated download links: https://irma.nps.gov/DataStore/Reference/Profile/2224545?lnv=True 
#' 
#' LAST UPDATED: SEPTEMBER 2025
#' 
#' @return A spatial sf object for all park boundaries
getParkSystem <- function(save = FALSE, path = NULL){
  

  downloadLink <-  "https://irma.nps.gov/DataStore/DownloadFile/733895"
  
  #download boundary 
  temp1 <- tempfile()
  download.file(downloadLink, destfile = temp1, method = "curl")
  temp2 <- tempfile()
  unzip(temp1, exdir = temp2)
  
  
  parks <- sf::st_read(paste0(temp2, "/nps_boundary.shp")) %>%
    sf::st_make_valid()
  
  # Optionally save the data
  if (save && !is.null(path)) {
    sf::st_write(parks, paste0(path, "/", "nps_park_boundaries_", Sys.Date(), ".gpkg"), delete_dsn = TRUE)
  }
  
  
  return(parks)
  
}

