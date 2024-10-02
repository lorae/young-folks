library(rjson)
library(blsAPI)
library(ggplot2)
library(dplyr)


## Updated series IDs for monthly data (removed 'Q' for monthly data)
series_ids <- c('LNS14000036', 'LNU04000039', 'LNU04000042', 'LNU04000045', 'LNU04032248')

## Pull the data via the API for 2020 to 2024
payload <- list(
  'seriesid'  = series_ids,
  'startyear' = 2020,
  'endyear'   = 2024
)
response <- blsAPI(payload, 2)
json <- fromJSON(response)

## Function to process results into a dataframe
apiDF <- function(data) {
  df  <- data.frame(matrix(unlist(data), nrow = length(data), byrow = TRUE))
  colnames(df) <- c("year", "period", "periodName", "value")
  return(df)
}

## Extract data for each series
unemployed.df_20_24 <- apiDF(json$Results$series[[1]]$data)
white.df <- apiDF(json$Results$series[[2]]$data)
black.df <- apiDF(json$Results$series[[3]]$data)
hispanic.df <- apiDF(json$Results$series[[4]]$data)
asian.df <- apiDF(json$Results$series[[5]]$data)

## Change value type from character to numeric
unemployed.df_20_24$value <- as.numeric(unemployed.df_20_24$value)
white.df$value <- as.numeric(white.df$value)
black.df$value <- as.numeric(black.df$value)
hispanic.df$value <- as.numeric(hispanic.df$value)
asian.df$value <- as.numeric(asian.df$value)

## Filter out unreasonable values (sometimes API returns erroneous data)
unemployed.df_20_24 <- subset(unemployed.df_20_24, value < 100)
white.df <- subset(white.df, value < 100)
black.df <- subset(black.df, value < 100)
hispanic.df <- subset(hispanic.df, value < 100)
asian.df <- subset(asian.df, value < 100)

## Merge the data into a single dataframe for plotting
df <- data.frame(
  date = as.POSIXct(strptime(paste0('1', unemployed.df_20_24$periodName, unemployed.df_20_24$year), '%d%B%Y')),
  unemployment_20_24 = unemployed.df_20_24$value,
  white = white.df$value,
  black = black.df$value,
  hispanic = hispanic.df$value,
  asian = asian.df$value
) |>
  arrange(date)

ggplot(df, aes(x = date)) +
  geom_line(aes(y = unemployment_20_24, color = "20-24 Years")) +
  geom_line(aes(y = white, color = "White")) +
  geom_line(aes(y = black, color = "Black or African American")) +
  geom_line(aes(y = hispanic, color = "Hispanic or Latino")) +
  geom_line(aes(y = asian, color = "Asian")) +
  labs(y = 'Unemployment Rate (%)', x = 'Date', 
       title = 'Unemployment Rates by Group (2020-2024)',
       color = 'Group') +
  scale_y_continuous(limits = c(0, 35)) +  # Setting y-axis limits
  theme_minimal() +
  theme(legend.position = "bottom")  # Move the legend to the bottom

