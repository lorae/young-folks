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
    
    own_2012_se <- result$own_2012_se$data |>
      mutate(year = 2012)
    
    own_2022_se <- result$own_2022_se$data |>
      mutate(year = 2022)
    
    example_table <- bind_rows(
      own_2012_se, 
      own_2022_se) 
      
      datatable(
        example_table,
        options = list(
          pageLength = 5,
          autoWidth = TRUE,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE
      ) |>
      formatPercentage(c("percent"), digits = 0)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)