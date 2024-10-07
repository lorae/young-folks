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
ipums_db <- tbl(con_raw, "ipums")

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

# ----- Step 4: Generate and execute SQL queries for bucketed columns ----- #

# Define the list of bucket columns to be added
bucket_columns <- list(
  list(
    lookup_filepath = "lookup_tables/age/age_buckets00.csv",
    input_column = "AGE"
  ),
  list(
    lookup_filepath = "lookup_tables/hispan/hispan_buckets00.csv",
    input_column = "HISPAN"
  ),
  list(
    lookup_filepath = "lookup_tables/race/race_buckets00.csv",
    input_column = "RACE"
  )
)

# Add bucketed columns using the lookup tables
for (bucket in bucket_columns) {
  # Load lookup table
  lookup_table <- read_csv(bucket$lookup_filepath, col_types = cols())
  
  # Split lookup table into range and value tables
  lookup_split <- split_lookup_table(bucket$lookup_filepath)
  range_lookup_table <- lookup_split$range
  value_lookup_table <- lookup_split$value
  
  # Write SQL to add the bucketed column
  start_time <- Sys.time()
  sql_query <- write_sql_query(
    range_lookup_table = range_lookup_table,
    value_lookup_table = value_lookup_table,
    col = bucket$input_column,
    table = "ipums"
  )
  
  # Execute the query to add the new column
  dbExecute(con_raw, sql_query)
  end_time <- Sys.time()
  cat("Time taken for", bucket$input_column, "bucketing: ", end_time - start_time, "\n")
  
  # Validate row count
  validate_row_counts(
    db = tbl(con_raw, "ipums"),
    expected_count = obs_count,
    step_description = glue::glue("{bucket$input_column} bucketed column was added")
  )
}

# "RACE_ETH_bucket" (by combining entries in HISPAN_bucket and RACE_bucket)
start_time <- Sys.time()
sql_query <- write_race_eth_sql_query(
  table = "ipums"
)
dbExecute(con_raw, sql_query)
end_time <- Sys.time()
cat("Time taken for race/ethnicity bucketing: ", end_time - start_time, "\n")

validate_row_counts(
  db = ipums_db,
  expected_count = obs_count,
  step_description = "data were bucketed into a combined race-ethnicity column"
)

# ----- Step 5: Save to the database ----- #

compute(ipums_db,
  name = "ipums_bucketed",
  temporary = FALSE,
  overwrite = TRUE
)

# ----- Step 6: Clean up ----- #

DBI::dbDisconnect(con_raw)
