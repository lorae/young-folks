library(shiny)
library(DT)
library(dplyr)
library(scales)
library(ggplot2)
library(plotly)
library(shinyWidgets)

source("R/data-viz-tools.R") 

# Load necessary data
load("data.rda")
load("race-sex-summary.rda")
load("fast-facts.rda")
load("cohabit_over_time.rda")

# Define some constant variables used in the text description of the data
pct_cohabit_2012 <<- fast_facts$twenties_2012_cohabit$data |>
  filter(cohabit_bin %in% c(
    "Child depends on parent", 
    "Both child and parent are dependent", 
    "Child provides for parent")) |>
  summarize(total = sum(percent)) |>
  pull(total) |>
  round(digits = 1)
pct_cohabit_2022 <<- fast_facts$twenties_2022_cohabit$data |>
  filter(cohabit_bin %in% c(
    "Child depends on parent", 
    "Both child and parent are dependent", 
    "Child provides for parent")) |>
  summarize(total = sum(percent)) |>
  pull(total) |>
  round(digits = 1)


