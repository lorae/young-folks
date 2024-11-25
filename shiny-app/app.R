# app.R

library(shiny)
library(DT)
library(dplyr)
library(scales)
library(ggplot2)
library(plotly)


# Run the application 
shinyApp(ui = ui, server = server)