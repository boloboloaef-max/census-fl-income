# download_data.R
# Script to download Florida county-level demographic data from the 5-year ACS
# using tidycensus (with an automatic fallback to Census Bureau direct tables
# if a Census API key is not yet installed).

library(tidycensus)
library(tidyverse)
library(sf)

data_file <- "data/fl_census.rds"

fetch_via_tidycensus <- function() {
  get_acs(
    geography = "county",
    state = "FL",
    variables = c(
      total_pop = "B01003_001",
      hispanic = "B03002_012",
      med_income = "B19013_001"
    ),
    year = 2022,
    geometry = TRUE,
    output = "wide"
  )
}

fetch_fallback <- function() {
  message("Pulling county geometries and ACS 2022 tables directly from the US Census Bureau...")
  library(tigris)
  options(tigris_use_cache = TRUE)

  fl_geom <- counties(state = "FL", cb = TRUE, year = 2022) |>
    select(GEOID, NAME, geometry)

  # Total population (B01003)
  pop_raw <- read_delim(
    "https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b01003.dat",
    delim = "|", show_col_types = FALSE
  ) |>
    filter(grepl("^0500000US12", GEO_ID)) |>
    transmute(
      GEOID = substr(GEO_ID, 10, 14),
      total_popE = as.numeric(B01003_E001),
      total_popM = as.numeric(B01003_M001)
    )

  # Hispanic or Latino population (B03002)
  hisp_raw <- read_delim(
    "https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b03002.dat",
    delim = "|", show_col_types = FALSE
  ) |>
    filter(grepl("^0500000US12", GEO_ID)) |>
    transmute(
      GEOID = substr(GEO_ID, 10, 14),
      hispanicE = as.numeric(B03002_E012),
      hispanicM = as.numeric(B03002_M012)
    )

  # Median household income (B19013)
  inc_raw <- read_delim(
    "https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b19013.dat",
    delim = "|", show_col_types = FALSE
  ) |>
    filter(grepl("^0500000US12", GEO_ID)) |>
    transmute(
      GEOID = substr(GEO_ID, 10, 14),
      med_incomeE = as.numeric(B19013_E001),
      med_incomeM = as.numeric(B19013_M001)
    )

  fl_geom |>
    left_join(pop_raw, by = "GEOID") |>
    left_join(hisp_raw, by = "GEOID") |>
    left_join(inc_raw, by = "GEOID") |>
    mutate(NAME = paste0(NAME, " County, Florida"))
}

# Try tidycensus first; if missing key or error, use Census tables fallback
fl_census <- tryCatch(
  fetch_via_tidycensus(),
  error = function(e) {
    message("tidycensus notice: ", e$message)
    fetch_fallback()
  }
)

fl_census <- fl_census |>
  mutate(
    hispanic_pct = (hispanicE / total_popE) * 100
  )

saveRDS(fl_census, file = data_file)
message("Saved Florida county census data to ", data_file)
