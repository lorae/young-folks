# annual-aggregators.R
#
# Track group quarters status of the surveyed population from 2012
# to 2022
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
load("docs/ipums_value_labels.RData") # and value labels

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")

ipums_relate <- tbl(con, "ipums_relationships")

# Overall HoH status
hoh_status <- create_crosstabs(
  data = ipums_relate,
  weight_column = "PERWT",
  group_by_columns = c("RELATED")
) |> 
  collect() |>
  arrange(-sum_weights) |>
  left_join(
    x = _, 
    y = value_labels_list$RELATED, 
    by = c("RELATED" = "val")
  )

# 2022 HoH status
hoh_status_2022 <- create_crosstabs(
  data = ipums_relate |> filter(YEAR == 2022),
  weight_column = "PERWT",
  group_by_columns = c("RELATED")
) |> 
  collect() |>
  arrange(-sum_weights) |>
  left_join(
    x = _, 
    y = value_labels_list$RELATED, 
    by = c("RELATED" = "val")
  )

# Data check: The sum of the sum_weights column should equal 
# the population of the US. This looks right, at 333.3 million.
sum(hoh_status_2022$sum_weights)

# The prison population of the US is about 1.9 million but 
# the "Institutional inmates" population of the US is about
# 3.6 million. My best guess is that this elevated value includes
# military members living in barracks, the incarcerated 
# population, populations living in nursing homes, mental 
# institutions, and other shelters.

# Let's write the results to a CSV to look at later.
write_csv(hoh_status_2022, "results/hoh_status_2022.csv")
write_csv(hoh_status, "results/hoh_status.csv")
