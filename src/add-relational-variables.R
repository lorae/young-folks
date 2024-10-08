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
    count_RELATED_301 = sum(if_else(RELATED == 301, 1L, 0L)),
    count_RELATED_501 = sum(if_else(RELATED == 501, 1L, 0L)),
    count_RELATED_601 = sum(if_else(RELATE == 601, 1L, 0L))
  )

# ----- Step 4: Join the counts back to the main table ----- #

ipums_with_counts <- ipums_db %>%
  left_join(hh_counts, by = "hh_id")

# ----- Step 5: Compute cohabit_bin ----- #
# For documentation on how these rules were formulated and their caveats,
# please see `docs/how-we-classify-families.html`

ipums_final <- ipums_with_counts |>
  mutate(
    # cohabit_bin equals 0 when person is not living with parent,
    # 1 when person is living with parent, and 2 when person is living
    # in a group setting, like a prison.
        cohabit_bin = case_when(
          RELATED == 101 ~ if_else(count_RELATED_501 > 0, 1, 0),
          RELATED == 201 ~ if_else(count_RELATED_601 > 0, 1, 0),
          RELATED == 301 ~ 1,
          RELATED == 302 ~ 1,
          RELATED == 303 ~ 1,
          RELATED == 401 ~ 0,
          RELATED == 501 ~ 0,
          RELATED == 701 ~ if_else(count_RELATED_501 > 0, 1, 0),
          RELATED == 801 ~ if_else(count_RELATED_601 > 0, 1, 0),
          RELATED == 901 ~ if_else(count_RELATED_301 > 0, 1, 0),
          RELATED == 1001 ~ 0,
          RELATED == 1114 ~ if_else(count_RELATED_601 > 0, 1, 0),
          RELATED == 1115 ~ 0,
          RELATED == 1241 ~ 0,
          RELATED == 1242 ~ 1,
          RELATED == 1260 ~ 0,
          RELATED == 1270 ~ 0,
          RELATED == 1301 ~ 2,  # Special case for 1301
          TRUE ~ NA_real_  # NA for all other cases
        )
      ) |>
      select( # Remove helper columns
        -count_RELATED_301,
        -count_RELATED_501,
        -count_RELATED_601
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