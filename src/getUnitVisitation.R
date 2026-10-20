#' Retrieve NPS park visitation data
#'
#' This function uses the IRMA STATS REST Api to pull visitation data for Nation Parks
#'
#' @param units The park unit(s) 4 digit code to get visitation for. Can supply a string of park codes or single code.
#' @param startMonth The start month for visitation data, values 1-12. Default is 1 (January).
#' @param startYear The start year for visitation data.
#' @param endMonth The end month for visitation data, values 1-12. Default is 12 (December).
#' @param endYear The end year for visitation data.
#' @param allStats If TRUE, pulls all visitation statistics from the IRMA dataset. Default is FALSE.
#'
#' @return A single dataframe/tibble of visitation statistics for specified parks.
#' Dataframe contains visitation numbers for each month of each year.
getUnitVisitation <- function(units,
                              startMonth = 1,
                              startYear,
                              endMonth = 12,
                              endYear,
                              allStats = FALSE) {
  
  if (allStats == FALSE) {
    
    #base URL call
    call <- "https://irmaservices.nps.gov/stats/v1"
    
    units <- paste0(units, collapse = ",")
    
    dat <- httr::GET(paste0(call, "/visitation?unitCodes=", units, "&startMonth=",
                            startMonth, "&startYear=", startYear, "&endMonth=",
                            endMonth, "&endYear=", endYear))
    
    
    # convert content to text
    dat_text <- httr::content(dat, "text", encoding = "UTF-8")
    
    #parse data in JSON
    dat_json <- jsonlite::fromJSON(dat_text, flatten = TRUE)
    
    #convert items to data.frame
    final_df <- as_tibble(dat_json)
    
  } else {
    
    # pull full visitation dataset from IRMA
    ref <- 2317666
    
    files <- jsonlite::fromJSON(paste0("https://irmaservices.nps.gov/datastore/v8/rest/Reference/", ref, "/DigitalFiles"),
                                flatten = TRUE)
    
    target <- files[files$fileName == "Main_Data.csv", ]
    
    dat <- readr::read_csv(curl::curl(target$downloadLink[1]),
                           show_col_types = FALSE)
    
    # filter to requested units and dates
    final_df <- dat %>%
      dplyr::filter(UnitCode %in% units,
                    Year > startYear | (Year == startYear & Month >= startMonth),
                    Year < endYear | (Year == endYear & Month <= endMonth)) %>%
      dplyr::mutate(Statistic = dplyr::recode(Statistic,
                                              "TRV"   = "RecreationVisitors",
                                              "TNRV"  = "NonRecreationVisitors",
                                              "TV"    = "TotalVisitors",
                                              "TRVH"  = "RecreationVisitorHours",
                                              "TNRVH" = "NonRecreationVisitorHours",
                                              "TH"    = "TotalVisitorHours",
                                              "CL"    = "ConcessionerLodging",
                                              "CCG"   = "ConcessionerCamping",
                                              "TT"    = "TentCampers",
                                              "TRVS"  = "RVCampers",
                                              "TTRV"  = "TotalNPSCampground",
                                              "BC"    = "BackcountryCampers",
                                              "MISC"  = "MiscellaneousOvernightStays",
                                              "NROS"  = "NonRecreationOvernightStays",
                                              "TOS"   = "TotalOvernightStays")) %>%
      pivot_wider(names_from = Statistic,
                  values_from = Value)
    
  }
  
  return(final_df)
  
}