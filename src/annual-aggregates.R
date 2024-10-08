# annual-aggregators.R
#
# Track rates of young adults living with parents from 2012 to 2022.
# Input: data/db/ipums-processed.duckdb
# Output: 
# - list here
# - ...

# ----- Step 0: Load packages ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("readr")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")

ipums_relate <- tbl(con, "ipums_relationships")

create_crosstabs(
  data = ipums_relate,
  weight_column = "PERWT",
  group_by_columns = c("AGE_bucket", "cohabit_parent")
) |> 
  collect() |>
  arrange(AGE_bucket, cohabit_parent) |>
  View()
