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


