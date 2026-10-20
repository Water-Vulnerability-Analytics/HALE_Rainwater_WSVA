#' Recreation.gov reservations for chosen facilities, from the RIDB historical reservation files
#'
#' Recreation.gov publishes every reservation it processes, one zip per fiscal year
#' (Oct-Sep), at https://ridb.recreation.gov/download ("RIDB/Recreation.gov Historical
#' Reservation Data"). Each zip holds one large CSV. This function downloads the zips it
#' does not already have into `raw_dir`, streams each CSV through `grep` so only rows that
#' mention a requested facility ID are read, keeps rows whose facility ID matches exactly,
#' and returns them as one data.table (all columns kept as character).
#'
#' @param years fiscal years (e.g. 2017:2025).
#' @param facility_ids recreation.gov facility IDs (character or numeric).
#' @param raw_dir where the yearly zips are (or will be) saved; they are large (0.1-0.5 GB each).
#' @param out_file optional parquet cache; if it exists it is read instead (unless `overwrite`).
#' @param overwrite if TRUE, re-download every zip and rebuild `out_file`.
get_ridb_reservations <- function(years, facility_ids, raw_dir, out_file = NULL, overwrite = FALSE) {
  if (!overwrite && !is.null(out_file) && file.exists(out_file)) return(data.table::as.data.table(arrow::read_parquet(out_file)))
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  old <- options(timeout = max(3600, getOption("timeout"))); on.exit(options(old))   # large files
  ids <- as.character(facility_ids)
  res <- data.table::rbindlist(lapply(years, function(y) {
    zip <- file.path(raw_dir, paste0("reservations", y, ".zip"))
    if (overwrite || !file.exists(zip)) {
      utils::download.file(paste0("https://ridb.recreation.gov/downloads/reservations", y, ".zip"), zip, mode = "wb", quiet = TRUE)
    }
    q <- shQuote(zip)
    hdr <- names(data.table::fread(cmd = paste("unzip -p", q, "| head -n 2"), colClasses = "character"))
    rows <- data.table::fread(cmd = paste0("unzip -p ", q, " | grep -E '(^|,)\"?(", paste(ids, collapse = "|"), ")\"?(,|$)' || true"),
                              header = FALSE, colClasses = "character", col.names = hdr, fill = TRUE)
    names(rows) <- tolower(names(rows))
    rows <- rows[facilityid %in% ids]
    message("FY", y, ": ", nrow(rows), " reservations")
    rows[, fiscal_year := as.integer(y)]
  }), fill = TRUE)
  if (!is.null(out_file)) arrow::write_parquet(res, out_file)
  res
}
