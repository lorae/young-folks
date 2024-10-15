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

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 2: Import and wrangle data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")

ipums_relate <- tbl(con, "ipums_relationships") 


cohabit_2022_with_percents <- crosstab_percent(
  data = ipums_relate |> filter(YEAR == 2022),
  weight = "PERWT",
  group_by = c("AGE_bucket", "cohabit_bin"),
  percent_group_by = c("AGE_bucket"),
  every_combo = FALSE
) |> 
  arrange(AGE_bucket, cohabit_bin) |>
  collect()

cohabit_2012_with_percents <- crosstab_percent(
  data = ipums_relate |> filter(YEAR == 2012),
  weight = "PERWT",
  group_by = c("AGE_bucket", "cohabit_bin"),
  percent_group_by = c("AGE_bucket"),
  every_combo = FALSE
) |> 
  arrange(AGE_bucket, cohabit_bin) |>
  collect()


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


