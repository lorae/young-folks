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

con <- dbConnect(duckdb::duckdb(), "data/db/ipums-processed.duckdb")

ipums_bucketed |> filter()