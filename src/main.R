library(rjson)
library(blsAPI)
library(ggplot2)

## Pull the data via the API
payload <- list(
  'seriesid'  = c('LAUCN360610000000004', 'LAUCN360610000000006'),
  'startyear' = 2007,
  'endyear'   = 2009)
response <- blsAPI(payload, 2)
json     <- fromJSON(response)

## Process results
apiDF <- function(data) {
  df  <- data.frame(matrix(unlist(data), nrow = length(data), byrow = TRUE))
  colnames(df) <- c("year", "period", "periodName", "value")
  return(df)
}


unemployed.df  <- apiDF(json$Results$series[[1]]$data)
labor.force.df <- apiDF(json$Results$series[[2]]$data)

## Change value type from character to numeric
unemployed.df[,4]  <- as.numeric(unemployed.df[,4])
labor.force.df[,4] <- as.numeric(labor.force.df[,4])

## Rename value prior to merging
names(unemployed.df)[4]  <- 'unemployed'
names(labor.force.df)[4] <- 'labor.force'

## Merge data frames
df <- merge(unemployed.df, labor.force.df)

## Create date and unemployement rate
df$unemployment.rate <- df$unemployed / df$labor.force
df$date <- as.POSIXct(strptime(paste0('1',df$periodName,df$year), '%d%B%Y'))

## Beginning and end dates for the Great Recession (used in shaded area)
gr.start <- as.POSIXct(strptime('1December2007', '%d%B%Y'))
gr.end   <- as.POSIXct(strptime('1June2009', '%d%B%Y'))

## Plot the data
ggplot(df) + geom_rect(aes(xmin = gr.start, xmax = gr.end, ymin = -Inf, ymax = Inf), alpha = 0.4, fill="#DDDDDD") + geom_line(aes(date, unemployment.rate*100)) + ylab('Percent of labor force')  + xlab('Great Recession shaded in gray') + ggtitle('Unemployment Rate for Manhattan, NY (Jan 2007 to Dec 2010)') + theme_bw()