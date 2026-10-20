#' Overnight capacity by place from a recreation.gov RIDB full export
#'
#' @param ridb_dir folder with Facilities_API_v1.csv, Campsites_API_v1.csv and
#'   CampsiteAttributes_API_v1.csv.
#' @param anchor_facility regex matching one facility of the park; its parent rec area
#'   defines which facilities are kept.
#' @param places named character vector: regex (matched on ASCII facility + campsite names,
#'   first match wins) -> place label.
#' @return list: `fac` (facilities in the rec area), `capacity` (per campsite with people,
#'   place, type) and `cap_place` (sites and people per night by place and type).
hi_ridb_capacity <- function(ridb_dir, anchor_facility, places) {
  # Single-threaded reads: multithreaded fread intermittently hung after heavy arrow reads in the same session
  fac <- data.table::fread(file.path(ridb_dir, "Facilities_API_v1.csv"), nThread = 1,
                           select = c("FacilityID", "ParentRecAreaID", "FacilityName", "FacilityLongitude", "FacilityLatitude"))
  rec_area <- fac[grepl(anchor_facility, FacilityName), unique(ParentRecAreaID)]
  fac <- fac[ParentRecAreaID %in% rec_area][, FacilityID := as.character(FacilityID)]
  sites <- data.table::fread(file.path(ridb_dir, "Campsites_API_v1.csv"), nThread = 1,
                             select = c("CampsiteID", "FacilityID", "CampsiteName", "CampsiteType"))[
    , `:=`(FacilityID = as.character(FacilityID), CampsiteID = as.character(CampsiteID))][FacilityID %in% fac$FacilityID]
  att <- data.table::fread(file.path(ridb_dir, "CampsiteAttributes_API_v1.csv"), nThread = 1,
                           select = c("EntityID", "AttributeName", "AttributeValue"))[
    AttributeName == "Max Num of People"][, EntityID := as.character(EntityID)][EntityID %in% sites$CampsiteID]
  capacity <- merge(merge(sites, att[, .(CampsiteID = EntityID, people = as.numeric(AttributeValue))], by = "CampsiteID"),
                    fac[, .(FacilityID, FacilityName)], by = "FacilityID")
  nm <- stringi::stri_trans_general(paste(capacity$FacilityName, capacity$CampsiteName), "Latin-ASCII")
  capacity[, place := NA_character_]
  for (p in names(places)) capacity[is.na(place) & grepl(p, nm), place := places[[p]]]
  capacity[, type := data.table::fifelse(grepl("CABIN", CampsiteType), "Cabin", "Tent")]
  list(fac = fac, capacity = capacity,
       cap_place = capacity[!is.na(place), .(sites = .N, people = sum(people)), by = .(place, type)])
}
