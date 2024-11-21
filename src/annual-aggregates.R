# annual-aggregators.R
#
# Track rates of young adults living with parents from 2012 to 2022.
# Input: data/db/ipums-processed.duckdb
# Output: 
# - data.rda in shiny-app folder, used to populated app.R with data
#   for visualizations
# - ...

# ----- Step 0: Load packages ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("readr")
library("tidyr")
library("ggplot2")
library("purrr")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 2: Import data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")

ipums_relate <- tbl(con, "ipums_relationships") 

# ----- Step 3: Initialize labels ----- #
ownership_labels <- c(
  "0"  = "N/A",
  "12" = "Owned free and clear",
  "13" = "Owned with mortgage or loan",
  "21" = "No cash rent",
  "22" = "With cash rent"
)

cohabit_labels <- c(
  "0" = "Not living with parents", 
  "1" = "Child provides for parent", 
  "2" = "Both child and parent are dependent", 
  "3" = "Child depends on parent", 
  "9" = "Living in institution"
)

sex_labels <- c(
  "1" = "Male",
  "2" = "Female",
  "3" = "All"
)

# ----- Step 4: See what fraction of America falls under each ownership structure ----- #
own_2022_se <- estimate_with_bootstrap_se(
  data = ipums_relate |> filter(YEAR == 2022),
  f = crosstab_percent,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("percent"),
  id_cols = c("OWNERSHPD"),
  group_by = c("OWNERSHPD"),
  percent_group_by = c(),
  every_combo = TRUE
) |>
  arrange(OWNERSHPD) |>
  mutate(
    OWNERSHPD = factor(
      as.character(OWNERSHPD),            # Convert to character
      levels = names(ownership_labels),   # Use character levels
      labels = ownership_labels           # Assign corresponding labels
    )
  )

own_2012_se <- estimate_with_bootstrap_se(
  data = ipums_relate |> filter(YEAR == 2012),
  f = crosstab_percent,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("percent"),
  id_cols = c("OWNERSHPD"),
  group_by = c("OWNERSHPD"),
  percent_group_by = c(),
  every_combo = TRUE
) |>
  arrange(OWNERSHPD) |>
  mutate(
    OWNERSHPD = factor(
      as.character(OWNERSHPD),            # Convert to character
      levels = names(ownership_labels),   # Use character levels
      labels = ownership_labels           # Assign corresponding labels
    )
  )

print(own_2012_se)
print(own_2022_se)

# Save these summary tables into result
result <- list(
  own_2012_se = list(
    desc = "A table describing the fraction of Americans in 2012 by homeowner and renter status.",
    data = own_2012_se
  ),
  own_2022_se = list(
    desc = "A table describing the fraction of Americans in 2022 by homeowner and renter status.",
    data = own_2022_se
  )
)

# ----- Step 5: Tabulate cohabitation by age and homeowner/renter status ----- #
own_age_cohab_2022_se <- estimate_with_bootstrap_se(
  data = ipums_relate |> filter(YEAR == 2022),
  f = crosstab_percent,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("percent"),
  id_cols = c("AGE_bucket", "cohabit_bin", "OWNERSHPD"),
  group_by = c("AGE_bucket", "cohabit_bin", "OWNERSHPD"),
  percent_group_by = c("AGE_bucket", "OWNERSHPD"),
  every_combo = TRUE
) |>
  arrange(AGE_bucket, OWNERSHPD) |>
  mutate(
    OWNERSHPD = factor(
      as.character(OWNERSHPD),
      levels = names(ownership_labels),
      labels = ownership_labels
    ),
    cohabit_bin = factor(
      as.character(cohabit_bin),
      levels = names(cohabit_labels),
      labels = cohabit_labels
    )
  )

own_age_cohab_2012_se <- estimate_with_bootstrap_se(
  data = ipums_relate |> filter(YEAR == 2012),
  f = crosstab_percent,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("percent"),
  id_cols = c("AGE_bucket", "cohabit_bin", "OWNERSHPD"),
  group_by = c("AGE_bucket", "cohabit_bin", "OWNERSHPD"),
  percent_group_by = c("AGE_bucket", "OWNERSHPD"),
  every_combo = TRUE
) |>
  arrange(AGE_bucket, OWNERSHPD) |>
  mutate(
    OWNERSHPD = factor(
      as.character(OWNERSHPD),
      levels = names(ownership_labels),
      labels = ownership_labels
    ),
    cohabit_bin = factor(
      as.character(cohabit_bin),
      levels = names(cohabit_labels),
      labels = cohabit_labels
    )
  )

# Save these summary tables into result
result$own_age_cohab_2012_se <- list(
  desc = "A table describing the fraction of Americans in 2012 by age, cohabitation status, and homeowner/renter status. Values within a given age and homeowner/renter status add up to 100.",
  data = own_age_cohab_2012_se
)

result$own_age_cohab_2022_se <- list(
  desc = "A table describing the fraction of Americans in 2022 by age, cohabitation status, and homeowner/renter status. Values within a given age and homeowner/renter status add up to 100.",
  data = own_age_cohab_2022_se
)

save(result, file = "shiny-app/data.rda")

# ----- Step 6: Same thing, different order: Tabulate cohabitation by age and homeowner/renter status ----- #
cohab_age_own_2022_se <- estimate_with_bootstrap_se(
  data = ipums_relate |> filter(YEAR == 2022),
  f = crosstab_percent,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("percent"),
  id_cols = c("AGE_bucket", "cohabit_bin", "OWNERSHPD"),
  group_by = c("AGE_bucket", "cohabit_bin", "OWNERSHPD"),
  percent_group_by = c("AGE_bucket", "cohabit_bin"),
  every_combo = TRUE
) |>
  arrange(AGE_bucket, cohabit_bin) |>
  mutate(
    OWNERSHPD = factor(
      as.character(OWNERSHPD),
      levels = names(ownership_labels),
      labels = ownership_labels
    ),
    cohabit_bin = factor(
      as.character(cohabit_bin),
      levels = names(cohabit_labels),
      labels = cohabit_labels
    )
  )

cohab_age_own_2012_se <- estimate_with_bootstrap_se(
  data = ipums_relate |> filter(YEAR == 2012),
  f = crosstab_percent,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("percent"),
  id_cols = c("AGE_bucket", "cohabit_bin", "OWNERSHPD"),
  group_by = c("AGE_bucket", "cohabit_bin", "OWNERSHPD"),
  percent_group_by = c("AGE_bucket", "cohabit_bin"),
  every_combo = TRUE
) |>
  arrange(AGE_bucket, cohabit_bin) |>
  mutate(
    OWNERSHPD = factor(
      as.character(OWNERSHPD),
      levels = names(ownership_labels),
      labels = ownership_labels
    ),
    cohabit_bin = factor(
      as.character(cohabit_bin),
      levels = names(cohabit_labels),
      labels = cohabit_labels
    )
  )

# Save these summary tables into result
result$cohab_age_own_2012_se <- list(
  desc = "A table describing the fraction of Americans in 2012 in a given age and cohabitation group that live with their parents. Values within a given age and cohabitation status add up to 100.",
  data = cohab_age_own_2012_se
)

result$cohab_age_own_2022_se <- list(
  desc = "A table describing the fraction of Americans in 2022 in a given age and cohabitation group that live with their parents. Values within a given age and cohabitation status add up to 100.",
  data = cohab_age_own_2022_se
)

save(result, file = "shiny-app/data.rda")

# ----- Step 7: Cohabitation by race and sex ----- #
# For this step, we'll do an overall population breakdown (all races, male and female and total)
# as well as each race total and each race male and female.

# Function for generating these breakdowns using filter criteria as an input
sex_race_summarize <- function(
    year = NULL,
    race_eth_bucket = NULL,
    data = ipums_relate
) {
  # Initialize output as an empty list
  output <- list()
  
  # TODO: add checks to ensure that both year and race_eth_bucket actual arguments
  # exist in the data. Error if not.
  
  # Apply user-defined filters to the input data. Allows for NULL filter in 
  # race or year category, which will result in the full sample being used.
  data_filtered <- data
  
  if (!is.null(year)) {
    data_filtered <- data_filtered |>
      filter(YEAR == year)
  }
  
  if (!is.null(race_eth_bucket)) {
    data_filtered <- data_filtered |>
      filter(RACE_ETH_bucket == race_eth_bucket)
  }
  
  # Define encoding for factor labels
  sex_labels <- c(
    "1" = "Male",
    "2" = "Female",
    "3" = "All"
  )
  
  cohabit_labels <- c(
    "0" = "Not living with parents", 
    "1" = "Child provides for parent", 
    "2" = "Both child and parent are dependent", 
    "3" = "Child depends on parent", 
    "9" = "Living in institution"
  )
  
  # Write a description to include in output
  year_desc <- ifelse(is.null(year), "all years", year)
  race_desc <- ifelse(is.null(race_eth_bucket), "young adults of ages", paste("young", race_eth_bucket, "adults"))
  output$desc = paste0("A table summarizing cohabitation by age among ",
                       race_desc,
                       " in ",
                       year_desc,
                       ". Cohabitation status is broken down by sex: Male, Female, ",
                       "and All (combined)."
  )
  
  # Overall cohabitation breakdown, broken down by age and male/female sex
  cohabit_bysex <- estimate_with_bootstrap_se(
    data = data_filtered,
    f = crosstab_percent,
    wt_col = "PERWT",
    repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
    constant = 4/80,
    se_cols = c("percent"),
    id_cols = c("AGE_bucket", "SEX", "cohabit_bin"),
    group_by = c("AGE_bucket", "SEX", "cohabit_bin"),
    percent_group_by = c("AGE_bucket", "SEX"),
    every_combo = TRUE
  ) 
  
  # Overall cohabitation breakdown, broken down by age
  cohabit <- estimate_with_bootstrap_se(
    data = data_filtered,
    f = crosstab_percent,
    wt_col = "PERWT",
    repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
    constant = 4/80,
    se_cols = c("percent"),
    id_cols = c("AGE_bucket", "cohabit_bin"),
    group_by = c("AGE_bucket", "cohabit_bin"),
    percent_group_by = c("AGE_bucket"),
    every_combo = TRUE
  ) |>
    mutate(SEX = 3) # Add column for sex and assign SEX = 3 for "All"
  
  # Combine the overall statistics from `cohabit` (SEX = 3) with those from 
  # `cohabit_bysex` for males (SEX = 1) and females (SEX = 2) to produce the 
  # data for output
  output$data <- bind_rows(cohabit_bysex, cohabit) |>
    mutate( # Encode factor strings using `sex_labels` and `cohabit_labels`
      SEX = factor(
        as.character(SEX),
        levels = names(sex_labels),
        labels = sex_labels
      ),
      cohabit_bin = factor(
        as.character(cohabit_bin),
        levels = names(cohabit_labels),
        labels = cohabit_labels
      )
    ) |>
    arrange(AGE_bucket, SEX)
  
  return(output)
}


# Overall statistics, males and females, 2022
sex_2022_se <- estimate_with_bootstrap_se(
  data = ipums_relate |> filter(YEAR == 2022),
  f = crosstab_percent,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("percent"),
  id_cols = c("AGE_bucket", "SEX", "cohabit_bin"),
  group_by = c("AGE_bucket", "SEX", "cohabit_bin"),
  percent_group_by = c("AGE_bucket", "SEX"),
  every_combo = TRUE
) |>
  arrange(AGE_bucket, SEX) |>
  mutate(
    cohabit_bin = factor(
      as.character(cohabit_bin),
      levels = names(cohabit_labels),
      labels = cohabit_labels
    )
  )

# Overall statistics, males, females and all combined, 2022
allsex_2022_se <- estimate_with_bootstrap_se(
  data = ipums_relate |> filter(YEAR == 2022),
  f = crosstab_percent,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("percent"),
  id_cols = c("AGE_bucket", "cohabit_bin"),
  group_by = c("AGE_bucket", "cohabit_bin"),
  percent_group_by = c("AGE_bucket"),
  every_combo = TRUE
) |>
  mutate(
    cohabit_bin = factor(
      as.character(cohabit_bin),
      levels = names(cohabit_labels),
      labels = cohabit_labels
    ),
    SEX = 3 # Assign SEX = 3 for "All"
  ) |>
  # now we bind with the data for SEX = 1 and SEX = 2 from 2022
  bind_rows(sex_2022_se) |>
  arrange(AGE_bucket, SEX) |>
  mutate(
    SEX = factor(
      as.character(SEX),
      levels = names(sex_labels),
      labels = sex_labels
    )
  )

cohab_age_own_2012_se <- estimate_with_bootstrap_se(
  data = ipums_relate |> filter(YEAR == 2012),
  f = crosstab_percent,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("percent"),
  id_cols = c("AGE_bucket", "cohabit_bin", "OWNERSHPD"),
  group_by = c("AGE_bucket", "cohabit_bin", "OWNERSHPD"),
  percent_group_by = c("AGE_bucket", "cohabit_bin"),
  every_combo = TRUE
) |>
  arrange(AGE_bucket, cohabit_bin) |>
  mutate(
    OWNERSHPD = factor(
      as.character(OWNERSHPD),
      levels = names(ownership_labels),
      labels = ownership_labels
    ),
    cohabit_bin = factor(
      as.character(cohabit_bin),
      levels = names(cohabit_labels),
      labels = cohabit_labels
    )
  )


###############################
# Old code that isn't guaranteed to work anymore 
###############################

# ----- Step 3: Compute percentage cohabitation in 2022 and 2012 ----- #
cohabit_2022_se <- estimate_with_bootstrap_se(
  data = ipums_relate |> filter(YEAR == 2022),
  f = crosstab_percent,
  wt_col = "PERWT",
  repwt_cols = paste0("REPWTP", sprintf("%d", 1:80)),
  constant = 4/80,
  se_cols = c("percent"),
  id_cols = c("AGE_bucket", "cohabit_bin"),
  group_by = c("AGE_bucket", "cohabit_bin"),
  percent_group_by = c("AGE_bucket"),
  every_combo = TRUE
) |>
  mutate(
    cohabit_bin = factor(
      cohabit_bin, 
      levels = c(0, 1, 2, 3, 9)
    ),
    AGE_bucket = factor(
      AGE_bucket, 
      levels = 
        c("Under 16", 
          "16-17", 
          "17-18", 
          "18-19", 
          "20-21", 
          "22-23", 
          "24-25", 
          "26-27", 
          "28-29", 
          "30+")
    )) |>
  arrange(AGE_bucket, cohabit_bin)



cohabit_2022_with_percents <- crosstab_percent(
  data = ipums_relate |> filter(YEAR == 2022),
  weight = "PERWT",
  group_by = c("AGE_bucket", "cohabit_bin"),
  percent_group_by = c("AGE_bucket"),
  every_combo = TRUE,
  repwts = paste0("REPWTP", sprintf("%d", 1:80))
) |> 
  arrange(AGE_bucket, cohabit_bin)

cohabit_2012_with_percents <- crosstab_percent(
  data = ipums_relate |> filter(YEAR == 2012),
  weight = "PERWT",
  group_by = c("AGE_bucket", "cohabit_bin"),
  percent_group_by = c("AGE_bucket"),
  every_combo = TRUE,
  repwts = paste0("REPWTP", sprintf("%d", 1:80))
) |> 
  arrange(AGE_bucket, cohabit_bin)


# Create a function to compare percentages between two years
compare_percent_changes <- function(df1, df2) {
  # Merge the two datasets by AGE_bucket and cohabit_bin to ensure comparison on the same groups
  comparison <- df1 %>%
    inner_join(df2, by = c("AGE_bucket", "cohabit_bin"), suffix = c("_2012", "_2022")) %>%
    rowwise() %>%
    mutate(
      # Calculate the difference in percentages
      percent_difference = percent_2022 - percent_2012,
      
      # Calculate the combined standard error
      combined_standard_error = sqrt(percent_standard_error_2022^2 + percent_standard_error_2012^2),
      
      # Calculate the z-score
      z_score = percent_difference / combined_standard_error,
      
      # Determine if the change is statistically significant
      significant_change = ifelse(abs(z_score) > 1.96, "Significant", "Not Significant")
    ) %>%
    ungroup() %>%
    select(AGE_bucket, cohabit_bin, percent_2012, percent_2022, percent_difference, combined_standard_error, z_score, significant_change)
  
  return(comparison)
}

# Assuming cohabit_2012_with_percents and cohabit_2022_with_percents are your dataframes
result <- compare_percent_changes(cohabit_2012_with_percents, cohabit_2022_with_percents)

# Print the result
print(result)


# ----- Plotting ----- #

# 2022

# Define the correct order for AGE_bucket and create the plot
cohabit_2022_with_percents <- cohabit_2022_with_percents %>%
  mutate(
    AGE_bucket = factor(AGE_bucket, levels = c("Under 16", "16-17", "17-18", "18-19", "20-21", 
                                               "22-23", "24-25", "26-27", "28-29", "30+")),
    cohabit_bin = factor(cohabit_bin, levels = c(0, 1, 2, 3, 9))  # Ensure cohabit_bin is a factor
  )

# Create a stacked bar chart where each AGE_bucket is 100%
ggplot(cohabit_2022_with_percents, aes(x = AGE_bucket, y = percent, fill = cohabit_bin)) +
  geom_bar(stat = "identity", position = "fill") +  # `position = "fill"` makes each bar 100%
  scale_y_continuous(labels = scales::percent_format()) +  # Format the y-axis as percentages
  labs(
    title = "Cohabitation by Age Group, 2022",
    x = "Age Group",
    y = "Percentage",
    fill = "Cohabit Bin"
  ) +
  # Customize the legend labels
  scale_fill_manual(
    values = c("0" = "white", "1" = "#075792", "2" = "#1e81b0", "3" = "#46b1d5", "9" = "lightcoral"),
    labels = c("0" = "Not living with parents",
               "1" = "Child provides for parent",
               "2" = "Both child and parent are dependent",
               "3" = "Child depends on parent",
               "9" = "Living in institution")
  ) +
  geom_text(
    aes(label = scales::percent(percent / 100, accuracy = 0.1)), 
    position = position_fill(vjust = 0.5),  # Position text in the middle of each bar section
    size = 3  # Adjust text size as needed
  ) +
  theme_minimal() +  # Apply a clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    legend.position = "bottom"  # Move legend to the bottom
  )

# Save the plot as PNG in the results folder
ggsave("results/cohabitation_by_age_2022.png", width = 6.5, height = 5, units = "in")

##### 2012

# Define the correct order for AGE_bucket and create the plot
cohabit_2012_with_percents <- cohabit_2012_with_percents %>%
  mutate(
    AGE_bucket = factor(AGE_bucket, levels = c("Under 16", "16-17", "17-18", "18-19", "20-21", 
                                               "22-23", "24-25", "26-27", "28-29", "30+")),
    cohabit_bin = factor(cohabit_bin, levels = c(0, 1, 2, 3, 9))  # Ensure cohabit_bin is a factor
  )

# Create a stacked bar chart where each AGE_bucket is 100%
ggplot(cohabit_2012_with_percents, aes(x = AGE_bucket, y = percent, fill = cohabit_bin)) +
  geom_bar(stat = "identity", position = "fill") +  # `position = "fill"` makes each bar 100%
  scale_y_continuous(labels = scales::percent_format()) +  # Format the y-axis as percentages
  labs(
    title = "Cohabitation by Age Group, 2012",
    x = "Age Group",
    y = "Percentage",
    fill = "Cohabit Bin"
  ) +
  # Customize the legend labels
  scale_fill_manual(
    values = c("0" = "white", "1" = "#075792", "2" = "#1e81b0", "3" = "#46b1d5", "9" = "lightcoral"),
    labels = c("0" = "Not living with parents",
               "1" = "Child provides for parent",
               "2" = "Both child and parent are dependent",
               "3" = "Child depends on parent",
               "9" = "Living in institution")
  ) +
  geom_text(
    aes(label = scales::percent(percent / 100, accuracy = 0.1)), 
    position = position_fill(vjust = 0.5),  # Position text in the middle of each bar section
    size = 3  # Adjust text size as needed
  ) +
  theme_minimal() +  # Apply a clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    legend.position = "bottom"  # Move legend to the bottom
  )

# Save the plot as PNG in the results folder
ggsave("results/cohabitation_by_age_2012.png", width = 6.5, height = 5, units = "in")


