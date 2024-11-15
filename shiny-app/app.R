# app.R

library(shiny)
library(DT)
library(dplyr)
library(scales)
library(ggplot2)

# Load necessary data
load("data.rda")

# Define some constant variables used in the text description of the data
placeholder_constant_1 <- "1 gazillion"
placeholder_constant_2 <- 123456

# Define UI
ui <- fluidPage(
  
  # Sidebar for potential controls (add inputs here as needed)
  div(class = "scroll-container",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          p(a("Introduction", href = "#intro")),
          p(a("Table 1: Hypothetical Example", href = "#table1")),
          p(a("Table 2: 2005-2022 Changes by Race and Age", href = "#table2")),
          p(a("Table 3: Contributions", href = "#table3"))
        ),
        
        # Main panel for displaying content
        mainPanel(
          titlePanel("Changes in Young Adult Cohabitation Patterns, 2012 - 2022"),
          
          tags$h3("Introduction", id = "intro"),
          p("In 2012,", 
            placeholder_constant_1,
            "percent of young adults aged XX - YY lived with their parents. By 2022,",
            "this figure grew/shrank to",
            placeholder_constant_2, 
            "percent."
          ),
          p(paste("To what extent do these patterns differ by race? By gender? Are",
                  "children relying on parents, or are parents relying on children?",
                  "How does this vary by the homeownership or renter status of the",
                  "household?")),
          
          tags$h3("Table 1: Homeownership in the United States", id = "table1"),
          
          p(paste("Table 1a shows the fraction of individuals in the United States",
                  "in 2022, by their household status. Table 1b shows these values",
                  "in 2012.")),
          
          p(strong("Table 1")),
          DTOutput("table1"),

          )
        )
      )
  )

# Define server
server <- function(input, output, session) {
  
  # Table 1: Render theoretical example table
  output$table1 <- renderDT({
    example_table <- data.frame(
      group = c("White", "Hispanic"),
      perc_2005 = c(80, 20),
      perc_2022 = c(70, 30),
      hhsize_2005 = c(3.5, 5),
      hhsize_2022 = c(3.8, 4)
    ) |>
      mutate(
        cont_2005 = perc_2005 * hhsize_2005 / 100,
        cont_2022 = perc_2022 * hhsize_2022 / 100,
        cont_2022cf = perc_2022 * hhsize_2005 / 100,
        cont_diff = cont_2022 - cont_2022cf
      )
    
    sum_row <- example_table |>
      summarize(
        group = "Sum",
        perc_2005 = sum(perc_2005),
        perc_2022 = sum(perc_2022),
        hhsize_2005 = NA,
        hhsize_2022 = NA,
        cont_2005 = sum(cont_2005),
        cont_2022 = sum(cont_2022),
        cont_2022cf = sum(cont_2022cf),
        cont_diff = sum(cont_diff)
      )
    
    example_table <- bind_rows(example_table, sum_row) |>
      # TODO: divide percentages by 100 upstream in the code
      mutate(
        perc_2005 = perc_2005 /100,
        perc_2022 = perc_2022 / 100
      ) |>
      
      datatable(
        example_table,
        options = list(
          pageLength = 5,
          autoWidth = TRUE,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        colnames = c(
          "Group" = "group",
          "Percent of 2005 Population" = "perc_2005",
          "Percent of 2022 Population" = "perc_2022",
          "Average HH Size (2005)" = "hhsize_2005",
          "Average HH Size (2022)" = "hhsize_2022",
          "Actual Contribution (2005)" = "cont_2005",
          "Actual Contribution (2022)" = "cont_2022",
          "Counterfactual Contribution (2022)" = "cont_2022cf",
          "Difference from Counterfactual (2022)" = "cont_diff"
        )
      ) |>
      formatStyle(
        "Group",
        target = "row",
        fontWeight = styleEqual("Sum", "bold")
      ) |>
      formatRound("Difference from Counterfactual (2022)", digits = 2) |>
      formatPercentage(c("Percent of 2005 Population", "Percent of 2022 Population"), digits = 0)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)