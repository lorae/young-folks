# process-ipums.R
#
# This script adds bucket columns to raw data based on specifications outlined in
# CSV files in the `lookup_tables/` directory.
# It reads data from the "ipums" table in `/db/ipums-raw.duckdb` and writes processed
# data to the "ipums-bucketed" table in `/db/ipums-processed.duckdb`.
#
# According to the Census Bureau: "A combination of SAMPLE and SERIAL provides a unique 
# identifier for every household in the IPUMS; the combination of SAMPLE, SERIAL, 
# and PERNUM uniquely identifies every person in the database."
#

# ----- Step 0: Load packages ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("readr")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 2: Prepare the new database ----- #

# Connect to the databases
con_raw <- dbConnect(duckdb::duckdb(), "data/db/ipums-raw.duckdb")
con_processed <- dbConnect(duckdb::duckdb(), "data/db/ipums-processed.duckdb")

# Create the "ipums_raw" data in the con_processed connection. Make it temporary:
# we'll eventually only save the processed data table in this connection.
copy_to(
  dest = con_processed, 
  df = tbl(con_raw, "ipums"), 
  name = "ipums_raw", 
  temporary = TRUE, 
  overwrite = TRUE
)

ipums_db <- tbl(con_processed, "ipums_raw")

# For data validation: ensure no rows are dropped
obs_count <- ipums_db |>
  summarise(count = n()) |>
  pull()

# ----- Step 3: Add columns ----- #

# "pers_id" 
ipums_db <- ipums_db |>
  # SAMPLE, SERIAL, and PERNUM uniquely identify each person
  mutate(pers_id = paste(SAMPLE, SERIAL, PERNUM, sep = "_"),
         .before = YEAR)

validate_row_counts(
  db = ipums_db,
  expected_count = obs_count,
  step_description = "pers_id column was added"
)

# "hh_id" 
ipums_db <- ipums_db |>
  # SAMPLE and SERIAL uniquely identify each household
  mutate(hh_id = paste(SAMPLE, SERIAL, sep = "_"),
         .before = YEAR) 

validate_row_counts(
  db = ipums_db,
  expected_count = obs_count,
  step_description = "hh_id column was added"
)

# "AGE_bucketed" (using lookup table rules)
age_start_time <- Sys.time()
ipums_bucketed_db <- ipums_db |>
  append_bucket_column(
    con = con_processed,
    filepath = "lookup_tables/age/age_buckets00.csv", 
    data = _, 
    input_column = "AGE", 
    id_column = "pers_id"
  ) |>
  compute(
    name = "ipums_age_bucketed", 
    temporary = TRUE
  )
age_end_time <- Sys.time()
cat("Time taken for AGE bucketing: ", age_end_time - age_start_time, "\n")

validate_row_counts(
  db = ipums_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed by age group"
)

# "HISPAN_bucketed" (using lookup table rules)
hispan_start_time <- Sys.time()
ipums_bucketed_db <- ipums_bucketed_db |>
  append_bucket_column(
    con = con_processed,
    filepath = "lookup_tables/hispan/hispan_buckets00.csv",
    data = _,
    input_column = "HISPAN",
    id_column = "pers_id"
  ) |> 
  compute(
    name = "ipums_hispan_bucketed", 
    temporary = TRUE
  )
hispan_end_time <- Sys.time()
cat("Time taken for HISPAN bucketing: ", hispan_end_time - hispan_start_time, "\n")

validate_row_counts(
  db = ipums_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed by ethnicity"
)

# "RACE_bucketed" (using lookup table rules)
race_start_time <- Sys.time()
ipums_bucketed_db <- ipums_bucketed_db |>
  append_bucket_column(
    con = con_processed,
    filepath = "lookup_tables/race/race_buckets00.csv",
    data = _,
    input_column = "RACE",
    id_column = "pers_id"
  ) |> 
  compute(
    name = "ipums_race_bucketed", 
    temporary = TRUE
  )
race_end_time <- Sys.time()
cat("Time taken for RACE bucketing: ", race_end_time - race_start_time, "\n")

validate_row_counts(
  db = ipums_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed by race"
)

# "RACE_ETH_bucket" (by combining entries in HISPAN_bucket and RACE_bucket)
ipums_bucketed_db <- ipums_bucketed_db |>
  race_eth_bucket(
    data = _
  ) |> 
  compute(
    name = "ipums_race_eth_bucketed", 
    temporary = TRUE
  )

validate_row_counts(
  db = ipums_bucketed_db,
  expected_count = obs_count,
  step_description = "data were bucketed into a combined race-ethnicity column"
)

# ----- Step 5: Save to the database ----- #

ipums_bucketed_db <- ipums_bucketed_db |>
  compute(
    name = "ipums_bucketed", 
    temporary = FALSE,
    overwrite = TRUE
  )

# ----- Step 6: Clean up ----- #

DBI::dbDisconnect(con_raw)
DBI::dbDisconnect(con_processed)
