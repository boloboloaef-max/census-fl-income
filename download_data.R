# download_data.R
# Script to download Census demographic data from the 5-year ACS:
# 1. Florida county boundaries and demographics (data/fl_census.rds)
# 2. Florida census tracts (data/fl_tracts.rds) - 5,100+ observations
# 3. US national county demographics (data/us_counties.rds) - 3,100+ observations
#
# Uses tidycensus with an automatic fallback to Census Bureau direct table files.

library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)

fl_county_file <- "data/fl_census.rds"
fl_tract_file  <- "data/fl_tracts.rds"
us_county_file <- "data/us_counties.rds"
us_state_file  <- "data/us_states.rds"

# --- 1. Florida County Geometries & Demographics ---

fetch_fl_counties_via_tidycensus <- function() {
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

fetch_fl_counties_fallback <- function() {
  message("Pulling Florida county geometries and ACS 2022 tables directly...")
  options(tigris_use_cache = TRUE)
  fl_geom <- counties(state = "FL", cb = TRUE, year = 2022) |>
    select(GEOID, NAME, geometry)

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

# --- 2. Florida Tract Demographics ---

fetch_fl_tracts_via_tidycensus <- function() {
  get_acs(
    geography = "tract",
    state = "FL",
    variables = c(
      total_pop = "B01003_001",
      hispanic = "B03002_012",
      med_income = "B19013_001"
    ),
    year = 2022,
    geometry = FALSE,
    output = "wide"
  )
}

fetch_fl_tracts_fallback <- function() {
  message("Pulling Florida tract-level ACS 2022 tables directly...")

  pop_raw <- read_delim(
    "https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b01003.dat",
    delim = "|", show_col_types = FALSE
  ) |>
    filter(grepl("^1400000US12", GEO_ID)) |>
    transmute(
      GEOID = substr(GEO_ID, 10, 20),
      total_popE = as.numeric(B01003_E001),
      total_popM = as.numeric(B01003_M001)
    )

  hisp_raw <- read_delim(
    "https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b03002.dat",
    delim = "|", show_col_types = FALSE
  ) |>
    filter(grepl("^1400000US12", GEO_ID)) |>
    transmute(
      GEOID = substr(GEO_ID, 10, 20),
      hispanicE = as.numeric(B03002_E012),
      hispanicM = as.numeric(B03002_M012)
    )

  inc_raw <- read_delim(
    "https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b19013.dat",
    delim = "|", show_col_types = FALSE
  ) |>
    filter(grepl("^1400000US12", GEO_ID)) |>
    transmute(
      GEOID = substr(GEO_ID, 10, 20),
      med_incomeE = as.numeric(B19013_E001),
      med_incomeM = as.numeric(B19013_M001)
    )

  pop_raw |>
    left_join(hisp_raw, by = "GEOID") |>
    left_join(inc_raw, by = "GEOID") |>
    mutate(NAME = paste("Census Tract", GEOID))
}

# --- 3. US Counties Demographics & Regions ---

fetch_us_counties_via_tidycensus <- function() {
  get_acs(
    geography = "county",
    variables = c(
      total_pop = "B01003_001",
      hispanic = "B03002_012",
      med_income = "B19013_001"
    ),
    year = 2022,
    geometry = FALSE,
    output = "wide"
  )
}

fetch_us_counties_fallback <- function() {
  message("Pulling national county-level ACS 2022 tables directly...")

  pop_raw <- read_delim(
    "https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b01003.dat",
    delim = "|", show_col_types = FALSE
  ) |>
    filter(grepl("^0500000US", GEO_ID)) |>
    transmute(
      GEOID = substr(GEO_ID, 10, 14),
      total_popE = as.numeric(B01003_E001),
      total_popM = as.numeric(B01003_M001)
    )

  hisp_raw <- read_delim(
    "https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b03002.dat",
    delim = "|", show_col_types = FALSE
  ) |>
    filter(grepl("^0500000US", GEO_ID)) |>
    transmute(
      GEOID = substr(GEO_ID, 10, 14),
      hispanicE = as.numeric(B03002_E012),
      hispanicM = as.numeric(B03002_M012)
    )

  inc_raw <- read_delim(
    "https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b19013.dat",
    delim = "|", show_col_types = FALSE
  ) |>
    filter(grepl("^0500000US", GEO_ID)) |>
    transmute(
      GEOID = substr(GEO_ID, 10, 14),
      med_incomeE = as.numeric(B19013_E001),
      med_incomeM = as.numeric(B19013_M001)
    )

  pop_raw |>
    left_join(hisp_raw, by = "GEOID") |>
    left_join(inc_raw, by = "GEOID")
}

fetch_us_states_via_tidycensus <- function() {
  get_acs(
    geography = "state",
    variables = c(
      total_pop = "B01003_001",
      hispanic = "B03002_012",
      med_income = "B19013_001"
    ),
    year = 2022,
    geometry = FALSE,
    output = "wide"
  )
}

fetch_us_states_fallback <- function() {
  message("Pulling national state-level ACS 2022 tables directly...")

  pop_raw <- read_delim(
    "https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b01003.dat",
    delim = "|", show_col_types = FALSE
  ) |>
    filter(grepl("^0400000US", GEO_ID)) |>
    transmute(
      GEOID = substr(GEO_ID, 10, 11),
      total_popE = as.numeric(B01003_E001),
      total_popM = as.numeric(B01003_M001)
    )

  hisp_raw <- read_delim(
    "https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b03002.dat",
    delim = "|", show_col_types = FALSE
  ) |>
    filter(grepl("^0400000US", GEO_ID)) |>
    transmute(
      GEOID = substr(GEO_ID, 10, 11),
      hispanicE = as.numeric(B03002_E012),
      hispanicM = as.numeric(B03002_M012)
    )

  inc_raw <- read_delim(
    "https://www2.census.gov/programs-surveys/acs/summary_file/2022/table-based-SF/data/5YRData/acsdt5y2022-b19013.dat",
    delim = "|", show_col_types = FALSE
  ) |>
    filter(grepl("^0400000US", GEO_ID)) |>
    transmute(
      GEOID = substr(GEO_ID, 10, 11),
      med_incomeE = as.numeric(B19013_E001),
      med_incomeM = as.numeric(B19013_M001)
    )

  pop_raw |>
    left_join(hisp_raw, by = "GEOID") |>
    left_join(inc_raw, by = "GEOID")
}

# --- Execution & Processing ---

# Florida Counties
message("Processing Florida county data...")
fl_census <- tryCatch(
  fetch_fl_counties_via_tidycensus(),
  error = \(e) {
    message("tidycensus notice: ", e$message)
    fetch_fl_counties_fallback()
  }
) |>
  mutate(
    hispanic_pct = (hispanicE / total_popE) * 100
  )
saveRDS(fl_census, file = fl_county_file)
message("Saved Florida county data to ", fl_county_file)

# Florida Tracts (5,100+ observations)
message("Processing Florida tract data...")
fl_tracts <- tryCatch(
  fetch_fl_tracts_via_tidycensus(),
  error = \(e) {
    message("tidycensus tract notice: ", e$message)
    fetch_fl_tracts_fallback()
  }
) |>
  mutate(across(
    c(total_popE, total_popM, hispanicE, hispanicM, med_incomeE, med_incomeM),
    \(x) if_else(x < 0, NA_real_, x)
  )) |>
  mutate(
    hispanic_pct = if_else(
      !is.na(total_popE) & total_popE > 0,
      (hispanicE / total_popE) * 100,
      NA_real_
    )
  ) |>
  select(
    GEOID, NAME, total_popE, total_popM, hispanicE, hispanicM,
    med_incomeE, med_incomeM, hispanic_pct
  )
saveRDS(fl_tracts, file = fl_tract_file)
message("Saved Florida tract data (", nrow(fl_tracts), " tracts) to ", fl_tract_file)

# US National Counties (3,100+ observations)
message("Processing US national county data...")
data("fips_codes", package = "tigris")

region_map <- tibble(
  state = c(state.abb, "DC", "PR"),
  region = c(as.character(state.region), "South", "Territories")
) |>
  mutate(region = if_else(region == "North Central", "Midwest", region))

fips_clean <- fips_codes |>
  mutate(GEOID = paste0(state_code, county_code)) |>
  select(GEOID, state, state_name, county) |>
  left_join(region_map, by = "state") |>
  mutate(region = if_else(is.na(region), "Territories", region))

us_counties_raw <- tryCatch(
  fetch_us_counties_via_tidycensus(),
  error = \(e) {
    message("tidycensus US county notice: ", e$message)
    fetch_us_counties_fallback()
  }
)

us_counties <- us_counties_raw |>
  left_join(fips_clean, by = "GEOID") |>
  filter(state %in% c(state.abb, "DC")) |>
  mutate(across(
    c(total_popE, total_popM, hispanicE, hispanicM, med_incomeE, med_incomeM),
    \(x) if_else(x < 0, NA_real_, x)
  )) |>
  mutate(
    fips = GEOID,
    hispanic_pct = if_else(
      !is.na(total_popE) & total_popE > 0,
      (hispanicE / total_popE) * 100,
      NA_real_
    ),
    NAME = paste0(county, ", ", state)
  ) |>
  select(
    GEOID, fips, NAME, county, state, state_name, region,
    total_popE, total_popM, hispanicE, hispanicM,
    med_incomeE, med_incomeM, hispanic_pct
  )
saveRDS(us_counties, file = us_county_file)
message("Saved US county data (", nrow(us_counties), " counties) to ", us_county_file)

# US State Geometries & Demographics (50 states)
message("Processing US state data...")
us_states_raw <- tryCatch(
  fetch_us_states_via_tidycensus(),
  error = \(e) {
    message("tidycensus US state notice: ", e$message)
    fetch_us_states_fallback()
  }
) 

us_states <- states(cb = TRUE, year = 2022) |>
  filter(STUSPS %in% state.abb) |>
  select(GEOID, STUSPS, state_name = NAME, geometry) |>
  left_join(us_states_raw, by = "GEOID") |>
  mutate(across(
    c(total_popE, total_popM, hispanicE, hispanicM, med_incomeE, med_incomeM),
    \(x) if_else(x < 0, NA_real_, x)
  )) |>
  mutate(
    hispanic_pct = if_else(
      !is.na(total_popE) & total_popE > 0,
      (hispanicE / total_popE) * 100,
      NA_real_
    )
  ) |>
  select(
    GEOID, STUSPS, state_name, total_popE, total_popM,
    hispanicE, hispanicM, med_incomeE, med_incomeM, hispanic_pct, geometry
  )
saveRDS(us_states, file = us_state_file)
message("Saved US state data (", nrow(us_states), " states) to ", us_state_file)
