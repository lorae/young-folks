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

# An initial crosstab of the rate of cohabitation by age bucket
cohabit_2022 <- crosstab_count(
  data = ipums_relate |> filter(YEAR == 2022),
  weight_column = "PERWT",
  group_by_columns = c("AGE_bucket", "cohabit_bin")
) |> 
  collect() |>
  arrange(AGE_bucket, cohabit_bin)

crosstab_percents <- function(data, group_by, counts) {
  data %>%
    group_by(across(all_of(group_by))) %>%   # Group by the specified columns
    mutate(
      percent = 100 * (!!sym(counts)) / sum(!!sym(counts), na.rm = TRUE)   # Calculate percentage within each group
    ) %>%
    ungroup()  # Ungroup to return the full data frame
}

# Example usage with your data frame:
cohabit_2022_with_percents <- crosstab_percents(
  data = cohabit_2022, 
  group_by = "AGE_bucket", 
  counts = "count"
)

print(cohabit_2022_with_percents)

library(ggplot2)

# Define the correct order for AGE_bucket and create the plot
cohabit_2022_with_percents <- cohabit_2022_with_percents %>%
  mutate(
    AGE_bucket = factor(AGE_bucket, levels = c("Under 16", "16-17", "17-18", "18-19", "20-21", 
                                               "22-23", "24-25", "26-27", "28-29", "30+")),
    cohabit_bin = factor(cohabit_bin, levels = c(0, 1, 2, NA))  # Ensure cohabit_bin is a factor
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
    values = c("0" = "lightblue", "1" = "lightgreen", "2" = "lightcoral"),
    labels = c("0" = "Not living with parents",
               "1" = "Living with at least 1 parent",
               "2" = "Living in institution")
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

# An initial crosstab of the rate of cohabitation by age bucket
cohabit_2012 <- crosstab_count(
  data = ipums_relate |> filter(YEAR == 2012),
  weight_column = "PERWT",
  group_by_columns = c("AGE_bucket", "cohabit_bin")
) |> 
  collect() |>
  arrange(AGE_bucket, cohabit_bin)


# Example usage with your data frame:
cohabit_2012_with_percents <- crosstab_percents(
  data = cohabit_2012, 
  group_by = "AGE_bucket", 
  counts = "count"
)

print(cohabit_2012_with_percents)

library(ggplot2)

# Define the correct order for AGE_bucket and create the plot
cohabit_2012_with_percents <- cohabit_2012_with_percents %>%
  mutate(
    AGE_bucket = factor(AGE_bucket, levels = c("Under 16", "16-17", "17-18", "18-19", "20-21", 
                                               "22-23", "24-25", "26-27", "28-29", "30+")),
    cohabit_bin = factor(cohabit_bin, levels = c(0, 1, 2, NA))  # Ensure cohabit_bin is a factor
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
    values = c("0" = "lightblue", "1" = "lightgreen", "2" = "lightcoral"),
    labels = c("0" = "Not living with parents",
               "1" = "Living with at least 1 parent",
               "2" = "Living in institution")
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


