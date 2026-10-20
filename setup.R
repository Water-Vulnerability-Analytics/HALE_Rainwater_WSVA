# setup.R: packages and functions for the HALE analysis.
# Trimmed from nps_water_vulnerability/setup.R to only what Reports/*.Rmd use.
# Run from the project root (the Rmds set this for you via here::here()).

packageLoad <- function(x) {
  for (i in seq_along(x)) {
    if (!x[i] %in% rownames(installed.packages())) install.packages(x[i])
    library(x[i], character.only = TRUE)
  }
}
packageInstall <- function(x) {
  missing <- setdiff(x, rownames(installed.packages()))
  if (length(missing)) install.packages(missing)
}

# Attached: the code calls their functions without a pkg:: prefix.
# (tidyverse last, so dplyr's filter/select etc. win over data.table's.)
packageLoad(c("data.table", "ggrepel", "mapview", "patchwork", "sf", "tidyverse"))

# Installed only: called as pkg::fn().
packageInstall(c(
  "arrow",       # parquet climate store
  "elevatr",     # site elevations (Quick Analysis)
  "flextable",   # Word tables (report)
  "here",        # project root
  "janitor",     # get_wsd()
  "knitr", "rmarkdown",
  "readxl",      # get_wsd()
  "remotes",     # installs rnoaa from GitHub (below)
  "scales",
  "stars",       # reads the HRCM zarr store (hi_hrcm_download())
  "SPEI",        # SPI drought index (report)
  "stringi",     # report
  "terra",       # rasters
  "trend",       # gauge trend tests (Quick Analysis)
  "zoo",         # calc_precip_indices()
  # basemaps, boundaries, NPS/OSM downloads
  "curl", "httr", "jsonlite", "leaflet", "maptiles", "xml2",
  # only for the data pulls (run_obs_pull / run_nex_pull in Quick Analysis)
  "CFtime", "furrr", "future", "ncdf4"))

# rnoaa was archived from CRAN (GHCN-Daily gauges, Quick Analysis).
if (!"rnoaa" %in% rownames(installed.packages())) remotes::install_github("ropensci/rnoaa")

# get_wsd(fill_empty_coords = TRUE) also needs igraph and tigris; HALE does not use that option.

# Source all functions in src/
invisible(lapply(list.files("src", pattern = "\\.R$", full.names = TRUE, recursive = TRUE), source))

# Reusable Hawaiʻi functions (site extraction, delta change, exposure, tank models, ratings)
invisible(lapply(list.files("hi_src", pattern = "\\.R$", full.names = TRUE), source))
