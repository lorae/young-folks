# ----- Step 0: Source needed packages ----- #

library(rjson)
library(blsAPI)
library(ggplot2)
library(dplyr)
library(rlang)
library(purrr)

# ----- Step 1: Define series IDs pulled from API ----- #

# Define series IDs for monthly and quarterly data
series_ids_monthly <- c(
  'LNS14000036', 
  'LNU04000039', 
  'LNU04000042', 
  'LNU04000045', 
  'LNU04032248'
)

series_ids_quarterly <- c(
  'LNS14000036Q', 
  'LNU04000039Q', 
  'LNU04000042Q', 
  'LNU04000045Q', 
  'LNU04032248Q'
)

# API call to fetch data
fetch_data <- function(series_ids, startyear = 2020, endyear = 2024) {
  payload <- list(
    'seriesid'  = series_ids,
    'startyear' = startyear,
    'endyear'   = endyear
  )
  response <- blsAPI(payload, 2)
  json <- fromJSON(response)
  return(json)
}

# Generalized function to process data (monthly/quarterly)
process_data <- function(data, colname, interval = "monthly") {
  year <- unlist(lapply(data, function(x) x$year))
  period <- unlist(lapply(data, function(x) x$period))
  periodName <- unlist(lapply(data, function(x) x$periodName))
  value <- as.numeric(unlist(lapply(data, function(x) x$value)))
  
  # Create date column based on monthly or quarterly intervals
  if (interval == "monthly") {
    date <- as.Date(paste0("1 ", periodName, " ", year), format = "%d %B %Y")
  } else if (interval == "quarterly") {
    date <- as.Date(paste0(
      case_when(
        period == "Q01" ~ "01-01",
        period == "Q02" ~ "04-01",
        period == "Q03" ~ "07-01",
        period == "Q04" ~ "10-01"
      ), " ", year), format = "%m-%d %Y")
  }
  
  # Create a dataframe and rename the value column
  df <- tibble(value = value, date = date) |>
    rename(!!sym(colname) := value) |> 
    arrange(date)
  
  return(df)
}

# Generalized plotting function
plot_data <- function(data, y_columns, cumulative = FALSE, title, y_limits, output_file) {
  if (cumulative) {
    data <- data |> 
      mutate(across(all_of(y_columns), ~ . - first(.), .names = "{.col}_cum_diff"))
    y_columns <- paste0(y_columns, "_cum_diff")
  }
  
  plot <- ggplot(data, aes(x = date)) +
    geom_line(aes(y = !!sym(y_columns[1]), color = "All")) +
    geom_line(aes(y = !!sym(y_columns[2]), color = "White")) +
    geom_line(aes(y = !!sym(y_columns[3]), color = "Black or African American")) +
    geom_line(aes(y = !!sym(y_columns[4]), color = "Hispanic or Latino")) +
    geom_line(aes(y = !!sym(y_columns[5]), color = "Asian")) +
    labs(y = 'Unemployment Rate (%)', x = 'Date', 
         title = title,
         color = 'Group') +
    scale_y_continuous(limits = y_limits) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(output_file, plot = plot, width = 6.5, height = 4, units = "in")
}

# Fetch and process both monthly and quarterly data
json_monthly <- fetch_data(series_ids_monthly)
json_quarterly <- fetch_data(series_ids_quarterly)

# Process data for both intervals
interval_data <- function(json, interval) {
  all <- process_data(json$Results$series[[1]]$data, "All", interval)
  white <- process_data(json$Results$series[[2]]$data, "White", interval)
  black <- process_data(json$Results$series[[3]]$data, "Black", interval)
  hispanic <- process_data(json$Results$series[[4]]$data, "Hispanic", interval)
  asian <- process_data(json$Results$series[[5]]$data, "Asian", interval)
  
  data <- list(all, white, black, hispanic, asian) |> 
    reduce(full_join, by = "date")
  return(data)
}

# Prepare data
data_monthly <- interval_data(json_monthly, "monthly")
data_quarterly <- interval_data(json_quarterly, "quarterly")

# Plot data with original titles
plot_data(data_monthly, c("All", "White", "Black", "Hispanic", "Asian"), FALSE,
          'UR by race, 20-24 year olds, \nnot seasonally adjusted', 
          c(0, 35), "plot1_monthly.png")

plot_data(data_monthly, c("All", "White", "Black", "Hispanic", "Asian"), TRUE,
          'Cumulative change in UR by race, \npercentage point difference from Jan 2020, \nnot seasonally adjusted, \n20-24 year olds', 
          c(-10, 30), "plot2_monthly.png")

plot_data(data_quarterly, c("All", "White", "Black", "Hispanic", "Asian"), FALSE,
          'UR by race, 20-24 year olds, \nnot seasonally adjusted', 
          c(0, 35), "plot1_quarterly.png")

plot_data(data_quarterly, c("All", "White", "Black", "Hispanic", "Asian"), TRUE,
          'Cumulative change in UR by race, \npercentage point difference from Jan 2020, \nnot seasonally adjusted, \n20-24 year olds', 
          c(-5, 25), "plot2_quarterly.png")
