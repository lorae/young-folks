library(rjson)
library(blsAPI)
library(ggplot2)
library(dplyr)
library(rlang)
library(purrr)

series_ids <- c(
  'LNS14000036Q', 
  'LNU04000039Q', 
  'LNU04000042Q', 
  'LNU04000045Q', 
  'LNU04032248Q'
)

## Pull the data via the API for 2020 to 2024
payload <- list(
  'seriesid'  = series_ids,
  'startyear' = 2020,
  'endyear'   = 2024
)
response <- blsAPI(payload, 2)
json <- fromJSON(response)

## Function to process data object into a dataframe using lapply and create a proper date column
process_data <- function(
    data,
    colname
) {
  year <- unlist(lapply(data, function(x) x$year))
  period <- unlist(lapply(data, function(x) x$period))
  periodName <- unlist(lapply(data, function(x) x$periodName))
  value <- as.numeric(unlist(lapply(data, function(x) x$value)))
  
  # Create a proper date object
  # Assuming the date is the first day of each month
  date <- as.Date(paste0(
    case_when(
      period == "Q01" ~ "01-01",
      period == "Q02" ~ "04-01",
      period == "Q03" ~ "07-01",
      period == "Q04" ~ "10-01"
    ), 
    " ", year), format = "%m-%d %Y")
  
  # Create a dataframe with custom column name for 'value'
  df <- tibble(value = value, date = date) |>
    rename(!!sym(colname) := value) |> 
    arrange(date)
  
  return(df)
}

all <- process_data(
  data = json$Results$series[[1]]$data,
  colname = "All"
)
white <- process_data(
  data = json$Results$series[[2]]$data,
  colname = "White"
)
black <- process_data(
  data = json$Results$series[[3]]$data,
  colname = "Black"
)
hispanic <- process_data(
  data = json$Results$series[[4]]$data,
  colname = "Hispanic"
)
asian <- process_data(
  data = json$Results$series[[5]]$data,
  colname = "Asian"
)

## Merge the data into a single dataframe for plotting
data <- list(all, white, black, hispanic, asian) |>
  reduce(full_join, by = "date") |>
  mutate(
    All_cum_diff = All - first(All, order_by = date),
    White_cum_diff = White - first(White, order_by = date),
    Black_cum_diff = Black - first(Black, order_by = date),
    Hispanic_cum_diff = Hispanic - first(Hispanic, order_by = date),
    Asian_cum_diff = Asian - first(Asian, order_by = date)
  )


## Plot 1: Original unemployment rates
plot1 <- ggplot(data, aes(x = date)) +
  geom_line(aes(y = All, color = "All")) +
  geom_line(aes(y = White, color = "White")) +
  geom_line(aes(y = Black, color = "Black or African American")) +
  geom_line(aes(y = Hispanic, color = "Hispanic or Latino")) +
  geom_line(aes(y = Asian, color = "Asian")) +
  labs(y = 'Unemployment Rate (%)', x = 'Date', 
       title = 'UR by race, 20-24 year olds, \nnot seasonally adjusted',
       color = 'Group') +
  scale_y_continuous(limits = c(0, 35)) +  # Adjust y-axis limits as needed
  theme_minimal() +
  theme(legend.position = "bottom")

## Save Plot 1
ggsave("plot1Q.png", plot = plot1, width = 6.5, height = 4, units = "in")

## Plot 2: Cumulative change in unemployment rates since 2020
plot2 <- ggplot(data, aes(x = date)) +
  geom_line(aes(y = All_cum_diff, color = "All")) +
  geom_line(aes(y = White_cum_diff, color = "White")) +
  geom_line(aes(y = Black_cum_diff, color = "Black or African American")) +
  geom_line(aes(y = Hispanic_cum_diff, color = "Hispanic or Latino")) +
  geom_line(aes(y = Asian_cum_diff, color = "Asian")) +
  labs(y = '', x = 'Date', 
       title = 'Cumulative change in UR by race, \npercentage point difference from Jan 2020, \nnot seasonally adjusted, \n20-24 year olds',
       color = 'Group') +
  scale_y_continuous(limits = c(-5, 25)) +  # Adjust y-axis limits for cumulative changes
  theme_minimal() +
  theme(legend.position = "bottom")

## Save Plot 2
ggsave("plot2Q.png", plot = plot2, width = 6.5, height = 4, units = "in")
