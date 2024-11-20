# app.R

library(shiny)
library(DT)
library(dplyr)
library(scales)
library(ggplot2)
library(plotly)

# Load necessary data
load("data.rda")

# Define a helper function for a stacked bar chart
stacked_bar_plotly <- function(data, title = "Default title") {
  p <- ggplot(data, aes(x = AGE_bucket, y = percent, fill = cohabit_bin, text = paste(
    "Age Group:", AGE_bucket, "<br>",
    "Cohabit Bin:", cohabit_bin, "<br>",
    "Percent:", percent, "%<br>",
    "Count:", count
  ))) +
    geom_bar(stat = "identity", position = "fill") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = title,
      x = "Age Group",
      y = "Percentage",
      fill = "Cohabit Bin"
    ) +
    scale_fill_manual(values = c(
      "Not living with parents" = "white",
      "Child provides for parent" = "#075792",
      "Both child and parent are dependent" = "#1e81b0",
      "Child depends on parent" = "#46b1d5",
      "Living in institution" = "lightcoral"
    )) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  ggplotly(p, tooltip = "text")
}

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
          
          tags$h3("Graph: Cohabitation by Ownership", id = "graph"),
          p("The graph below shows cohabitation patterns for different homeownership statuses."),
          
          # Add a row with radio buttons and graph
          fluidRow(
            column(
              width = 3,
              radioButtons(
                "ownership_status", 
                label = "Select Homeownership Status:",
                choices = c(
                  "Owned free and clear" = "Owned free and clear",
                  "Owned with mortgage or loan" = "Owned with mortgage or loan",
                  "With cash rent" = "With cash rent",
                  "No cash rent" = "No cash rent",
                  "N/A" = "N/A"
                ),
                selected = "Owned free and clear"
              )
            ),
            column(
              width = 9,
              plotlyOutput("ownership_graph", height = "600px")
            )
          )

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
      
  # Render Graph: Cohabitation by Ownership
  output$ownership_graph <- renderPlotly({
    selected_status <- input$ownership_status
    
    # Dynamically filter data based on selected ownership status
    filtered_data <- result$own_age_cohab_2012_se$data |>
      dplyr::filter(OWNERSHPD == selected_status) |>
      # TODO: turn this into factor at data processing stage
      mutate(AGE_bucket = factor(
        AGE_bucket,
        levels = c("Under 16", "16-17", "18-19", "20-21", "22-23", "24-25", "26-27", "28-29", "30+")
      ))
    
    # Generate the graph
    stacked_bar(
      data = filtered_data,
      title = paste("Cohabitation Patterns:", selected_status)
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)