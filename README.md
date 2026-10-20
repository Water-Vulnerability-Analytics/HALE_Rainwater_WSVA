# HALE Summit District WSVA

Water Supply Vulnerability Assessment (WSVA) for the main water supply at Haleakalā National Park (HALE), the Summit District rain-catchment system. The report covers:

- **Observed rain:** the park's rain gauge and the HCDP daily rain maps, including the widening rain shadow.
- **Futures:** the park's two climate futures (Hawaiʻi Regional Climate Model, RCP 4.5 and RCP 8.5, 2080-2099), with the model's rain corrected to observed amounts.
- **Supply and demand:** visitation, recreation.gov reservations, and a daily tank model of the system's storage.
- **Other threats:** water quality, wildfire and severe storms.

## Files

```         
HALE_Summit_WSVA.Rmd             the report: settings, the values quoted in the text, and the text
HALE_Summit_WSVA_Processing.Rmd  downloads, data, analysis, figures, tables and Appendix A;
                                 knit as a child of the report
setup.R                          installs and attaches packages, sources src/ and hi_src/
src/                             data access: downloads and readers
hi_src/                          analysis functions for any site in Hawaiʻi
```

## Run it

Open the project in RStudio, which supplies pandoc, and knit **`HALE_Summit_WSVA.Rmd`**. From a terminal, in the project folder:

``` bash
Rscript -e 'rmarkdown::render("HALE_Summit_WSVA.Rmd")'
```

**Settings and assumptions** are at the top of the report's `constants` chunk: - the park; - collection efficiency, demand and catchment area, marked `{{ASSUMPTION}}` in the text; - the last year of visitation; - `rerun_downloads`.

**Downloads:** - **`rerun_downloads <- TRUE`:** the processing file's `proc-download` chunk downloads every input into `hi_datasets/`. This takes an hour or more and is needed on a first run. - **`FALSE`:** the report reads the saved copies.

`hi_datasets/` and `data/` are git-ignored.

**Not downloadable:** two inputs are read through a `data/` link to the project's shared folder: - the NPS Water Supply Database spreadsheet (`data/Water_Supply_Systems/`), an internal NPS file; - the project's archive of NPS park alerts (`data/all/nps_alerts/tidy/`).

`setup.R` installs any missing packages, including `rnoaa` from GitHub. The reservation download also uses the shell tools `unzip` and `grep`.

Figures are also saved as PNGs to `data/park/HALE/Figs/`.

## `src/`: data access

| Function | Data |
|------------------------------------|------------------------------------|
| `get_wsd()` | NPS Water Supply Database |
| `getParkBoundary()`, `getParkSystem()` | NPS park boundaries (IRMA) |
| `get_hcdp_obs_park()` | HCDP daily rain maps, the park's 250-m cells (uses grid helpers in `get_nex_hidcp_park.R`) |
| `get_ghcnd_near_park()` | GHCN-Daily rain gauges near the park |
| `calc_precip_indices()` | Yearly rain measures from daily records (totals, rain days, dry spells, heavy days) |
| `isohyet_line()` (`get_rain_shadow_line.R`) | A rain contour, used for the rain-shadow edge |
| `getUnitVisitation()` | NPS visitor statistics (IRMA) |
| `get_ridb_reservations()` | recreation.gov historical reservations |
| `get_storm_events()` | NOAA Storm Events Database |
| `get_nws_alerts()` | NWS watches, warnings and advisories (Iowa Environmental Mesonet archive) |
| `get_nps_basemap()`, `geom_nps_basemap()` | NPS Park Tiles basemaps |

## `hi_src/`: analysis

| File | Functions |
|------------------------------------|------------------------------------|
| `hi_extract.R` | `hi_site_cells()`, `hi_site_daily()`: a site's grid cells and daily rain from a parquet store |
| `hi_isohyets.R` | `hi_wy_grid()`, `hi_wy_raster()`, `hi_compare_isohyets()`: yearly rain maps and rain-shadow movement |
| `hi_hrcm.R` | `hi_hrcm_download()`, `hi_hrcm_download_wind()`, `hi_hrcm_daily()` and helpers: hourly rain and wind from the Hawaiʻi Regional Climate Model (Zhang 2017, USGS) |
| `hi_tank.R`, `hi_catchment.R` | `hi_tank_yas()` (daily tank model), `hi_catchment_needed()`, `hi_shortfall_days()` |
| `hi_setting.R` | `hi_site_setting()`: geology, soils, watershed, moisture zone, land cover, elevation |
| `hi_ridb.R` | `hi_ridb_capacity()`: recreation.gov facilities and campsite capacity |
| `hi_utils.R`, `hi_report_helpers.R` | caching, water-year helpers, number formatting, `save_fig()`, water-year plot axes |

## Inputs

Written to `hi_datasets/` by `proc-download` unless noted.

| File | What | Source |
|------------------------|------------------------|------------------------|
| `HALE_park_boundary.gpkg` | Park boundary | NPS IRMA |
| `HALE_system_setting.csv` | Physical setting of each water system | Hawaiʻi Statewide GIS, NRCS, USGS |
| `HALE/` | HCDP daily rain maps, 1990 on | Hawaiʻi Climate Data Portal |
| `HALE_ghcnd_stations.gpkg`, `HALE_ghcnd_prcp.parquet` | Rain gauges within 10 km; daily rain at the Haleakalā Ranger Station and Kula Branch Station | NOAA GHCN-Daily |
| `HALE_hrcm/` | Hourly rain (3 x 3 cells around each system) and 10-m wind (Summit District), present-day, RCP 4.5 and RCP 8.5 runs | Zhang (2017), USGS |
| `HALE_visitation_monthly.parquet` | Monthly visitor statistics, 1979 on | NPS IRMA |
| `HALE_ridb_reservations.parquet` | Hosmer Grove and summit sunrise reservations, FY2017-2025 | recreation.gov |
| `data/park/HALE/RIDBFullExport_V1_CSV/` | recreation.gov facilities and campsites | RIDB full export |
| `noaa_oni.txt`, `noaa_pdo.txt` | El Niño (Oceanic Niño Index) and Pacific Decadal Oscillation | NOAA CPC, NOAA NCEI |
| `HI_storm_events.parquet` | Storm events in Hawaiʻi, 1996 on | NOAA Storm Events Database |
| `HALE_nws_alerts_HIZ022.parquet` | NWS alerts for the Haleakalā summit zone | Iowa Environmental Mesonet |
| `HALE_nps_*.tif` | Basemaps for the report map (downloaded in `proc-figures`) | NPS Park Tiles |
| `data/Water_Supply_Systems/NPS_Water_Systems_Database_Joined.xlsx` | NPS Water Supply Database (not downloadable) | NPS Water Resources Division |
| `data/all/nps_alerts/tidy/` | NPS park alerts, daily snapshots, 2019 on (not downloadable) | project archive |
