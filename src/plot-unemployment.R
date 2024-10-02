# plot-unemployment.R
#
# The purpose of this script is to plot monthly or quarterly unemployment figures 
# by race among young adults age 20-24 using Bureau of Labor Statistics data.
#
# A few notes about this script
# Firstly, it was hastily generated to produce some motivating figures. It's unclear
# how essential these figures will be for the project, so I've avoided overly refining
# the code since it's unclear if it's needed.
# 
# That said, if it is used more or needs more development, I have some ideas to 
# improve the code.
# 
# - Firsly, it's not clear how much the blsAPI package is adding, particularly since
# this script is doing the gritty work of reading the json data and parsing it in 
# a custom way. I should look into if there's a more effective way to use this package
# or if I should instead just define my own custom function that draws on it more 
# straightforwardly. 
# - related to the first point, I'm particularly concenred about metadata. There
# must be a way to query the BLS API to get a basic description of each series. I 
# double checked manually that each series ID matches what I say it is supposed to
# be (which is the race and age of the group being measured), but a more automated,
# safe way of doing this and creating column names and graph legends would be ideal.
# - If I find that I continue to use the cumulative change function a lot, it might
# be beneficial to code up some helper functions related to adding these cumulative
# columns. And if it's really useful, then perhaps this function belongs in the
# duckdata package instead.
# - same goes for the graphing. If I end up making a lot of similar line graphs,
# it might be worthwhile to put those functions in the duckdata package. Another
# thing that I could do to standardize graphing would be to set custom themes.

# ----- Step 0: Source needed packages ----- #

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
create_plot <- function(data, y_columns, cumulative = FALSE, title, y_limits) {
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
  
  return(plot)
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

# Create plots but not save them yet
fig01_monthly <- create_plot(data_monthly, c("All", "White", "Black", "Hispanic", "Asian"), FALSE,
                             'UR by race, 20-24 year olds, \nnot seasonally adjusted', c(0, 35))

fig02_monthly <- create_plot(data_monthly, c("All", "White", "Black", "Hispanic", "Asian"), TRUE,
                             'Cumulative change in UR by race, \npercentage point difference from Jan 2020, \nnot seasonally adjusted, \n20-24 year olds', c(-10, 30))

fig01_quarterly <- create_plot(data_quarterly, c("All", "White", "Black", "Hispanic", "Asian"), FALSE,
                               'UR by race, 20-24 year olds, \nnot seasonally adjusted', c(0, 35))

fig02_quarterly <- create_plot(data_quarterly, c("All", "White", "Black", "Hispanic", "Asian"), TRUE,
                               'Cumulative change in UR by race, \npercentage point difference from Jan 2020, \nnot seasonally adjusted, \n20-24 year olds', c(-5, 25))

# View plots in R
print(fig01_monthly)
print(fig02_monthly)
print(fig01_quarterly)
print(fig02_quarterly)

# Save plots separately after viewing
ggsave("results/fig01-monthly.png", plot = fig01_monthly, width = 6.5, height = 4, units = "in")
ggsave("results/fig02-monthly.png", plot = fig02_monthly, width = 6.5, height = 4, units = "in")
ggsave("results/fig01-quarterly.png", plot = fig01_quarterly, width = 6.5, height = 4, units = "in")
ggsave("results/fig02-quarterly.png", plot = fig02_quarterly, width = 6.5, height = 4, units = "in")


