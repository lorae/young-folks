# add-relational-variables.R
# 
# Add variables describing whether children live with their parents.
#

# ----- Step 0: Load packages ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("readr")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums_processed")

# ----- Step 3: Create temporary table with hh level counts ----- #

hh_counts <- ipums_db %>%
  group_by(hh_id) %>%
  summarize(
    count_RELATE_1 = sum(if_else(RELATE == 1, 1L, 0L)),
    count_RELATE_2 = sum(if_else(RELATE == 2, 1L, 0L)),
    count_RELATE_5 = sum(if_else(RELATE == 5, 1L, 0L)),
    count_RELATE_6 = sum(if_else(RELATE == 6, 1L, 0L))
  )

# ----- Step 4: Join the counts back to the main table ----- #

ipums_with_counts <- ipums_db %>%
  left_join(hh_counts, by = "hh_id")

# ----- Step 5: Compute cohabit_parent and cohabit_parent_inlaw ----- #

ipums_final <- ipums_with_counts %>%
  mutate(
    cohabit_parent = case_when(
      RELATE == 1 ~ count_RELATE_5,
      RELATE == 2 ~ count_RELATE_6,
      RELATE == 3 ~ count_RELATE_1 + count_RELATE_2,
      TRUE ~ NA_real_
    ),
    cohabit_parent_inlaw = case_when(
      RELATE == 1 ~ count_RELATE_6,
      RELATE == 2 ~ count_RELATE_5,
      RELATE == 3 ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  select( # Remove helper columns
    -count_RELATE_1, 
    -count_RELATE_2, 
    -count_RELATE_5, 
    -count_RELATE_6
    )

# ----- Step 6: Save the computed result back to the db ----- # 

compute(
  ipums_final,
  name = "ipums_relationships",
  temporary = FALSE,
  overwrite = TRUE
)

# ----- Step 7: Close out ----- #

dbDisconnect(con)