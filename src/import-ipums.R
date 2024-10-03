# import-ipums.R
#
# This script processes raw IPUMS data and saves it in a DuckDB file.
# 
# Current data: usa_00005 (2022). This file is .gitignored.
# TODO: replace manual data pull with an API call for reproducibility.
# TODO: There's a nearly identical version of  this script in the sister
# GitHub repo, lorae/household-size. Functionalize in `duckdata` and call 
# in both?
#
# For more on IPUMS and ipumsr: https://www.youtube.com/watch?v=OT6upQ1dBgU

# ----- Step 0: Load packages ----- #
library("dplyr")
library("duckdb")
library("ipumsr")

# ----- Step 1: Load and process IPUMS data ----- #

ddi <- read_ipums_ddi("data/ipums-microdata/usa_00005.xml")
ipums_tb <- read_ipums_micro(ddi, var_attrs = c()) 

# ----- Step 2: Save to DuckDB ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums-raw.duckdb")
dbWriteTable(con, "ipums", ipums_tb, overwrite = TRUE)
DBI::dbDisconnect(con)

# ----- Step 3: Save helpful reference documentation ---- #

data_dict_path <- ipums_view(ddi) # file path to HTML document
file.copy(
  from = data_dict_path, 
  to = "docs/ipums-data-dictionary.html",
  overwrite = TRUE
  )

