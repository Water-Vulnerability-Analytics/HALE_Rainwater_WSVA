#' National Weather Service watches, warnings and advisories for one forecast zone
#'
#' Uses the Iowa Environmental Mesonet archive of NWS alerts (VTEC events), which lists every
#' watch, warning and advisory issued for a zone: https://mesonet.agron.iastate.edu/vtec/.
#' Haleakalā's summit is NWS Honolulu zone HIZ022 ("Haleakala Summit").
#'
#' @param ugc NWS zone code, e.g. "HIZ022".
#' @param start,end first and last dates (character "YYYY-MM-DD").
#' @return data.table: `name` (e.g. "High Wind Warning"), `phenomena`, `significance`, `issue`,
#'   `expire` (UTC), `eventid`, `ugc`.
get_nws_alerts <- function(ugc, start = "2001-01-01", end = as.character(Sys.Date())) {
  url <- sprintf("https://mesonet.agron.iastate.edu/json/vtec_events_byugc.php?ugc=%s&sdate=%s&edate=%s", ugc, start, end)
  ev <- jsonlite::fromJSON(url)$events
  x <- data.table::as.data.table(ev)[, .(name, phenomena, significance, issue = as.POSIXct(issue, format = "%Y-%m-%dT%H:%M", tz = "UTC"),
                                         expire = as.POSIXct(expire, format = "%Y-%m-%dT%H:%M", tz = "UTC"), eventid, ugc)]
  x[order(issue)]
}
