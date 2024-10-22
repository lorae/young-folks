# ----- Step 0: Load packages ----- #

library(testthat)
library(DBI)
library(duckdb)
library(dplyr)
library("rprojroot")

# Make sure the working directory is correct
root <- find_root(is_rstudio_project)
setwd(root)

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 2: set up data ----- #

# Connect to a temporary DuckDB database
con <- dbConnect(duckdb::duckdb(), ":memory:")

# Create a mock dataset similar to your ipums_bucketed table, with updated columns
input <- data.frame(
  id = 1:6,
  AGE = c(15, 16, 18, 22, 29, 35),
  YEAR = rep(2014, 6),
  SAMPLE = rep(201401, 6),
  SERIAL = c(755807, 755807, 755808, 755809, 755810, 755811),
  RACE = c(1, 2, 3, 4, 5, 6),   # RACE as integers from 1 to 9 (for simplicity, 1 to 6 here)
  HISPAN = c(0, 1, 0, 0, 1, 0),  # HISPAN as binary (0 or 1)
  SEX = c(1, 2, 1, 2, 1, 2),     # SEX as 1 (Male) or 2 (Female)
  EDUC = c(6, 8, 10, 11, 8, 6)   # EDUC as 6, 8, 10, or 11
)

# Write the data to a DuckDB table
dbWriteTable(con, "ipums_bucketed", input)

# Define a function to create indexes for multiple columns
create_indexes <- function(con, table_name, columns) {
  for (col in columns) {
    index_query <- paste0("CREATE INDEX IF NOT EXISTS idx_", col, " ON ", table_name, "(", col, ");")
    dbExecute(con, index_query)
  }
}

# List of relevant columns to index, including the updated ones
columns_to_index <- c("AGE", "RACE", "HISPAN", "SEX", "EDUC")

# Create indexes on all relevant columns
create_indexes(con, "ipums_bucketed", columns_to_index)

# ----- Step 2: The test ----- #

test_that("AGE bucket column is created correctly with extra columns", {

  # Define the expected data frame with the updated columns
  expected <- data.frame(
    id = 1:6,
    AGE = c(15, 16, 18, 22, 29, 35),
    YEAR = rep(2014, 6),
    SAMPLE = rep(201401, 6),
    SERIAL = c(755807, 755807, 755808, 755809, 755810, 755811),
    RACE = c(1, 2, 3, 4, 5, 6),    # Matching RACE values as integers
    HISPAN = c(0, 1, 0, 0, 1, 0),  # HISPAN as binary
    SEX = c(1, 2, 1, 2, 1, 2),     # SEX as 1 or 2
    EDUC = c(6, 8, 10, 11, 8, 6),  # EDUC as 6, 8, 10, or 11
    AGE_bucketed = c("Under 16", "16-17", "18-22", "18-22", "23-29", "30+")  # Bucketed AGE column
  )
  
  query <- create_sql_query()
  
  # Execute the query
  result <- dbGetQuery(con, query)
  
  # Sort both the result and expected data by id to ensure consistency
  result <- result |> arrange(id)
  expected <- expected |> arrange(id)
  
  # Remove row names from both data frames
  row.names(result) <- NULL
  row.names(expected) <- NULL
  
  # Compare the entire result and expected data frames
  expect_equal(result, expected)
  
  # Disconnect from DuckDB when done
  dbDisconnect(con, shutdown = TRUE)
})

