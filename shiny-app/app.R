# app.R

library(shiny)
library(DT)
library(dplyr)
library(scales)
library(ggplot2)
library(plotly)

# Load necessary data
load("data.rda")
load("race-sex-summary.rda")

# Define a helper function for a stacked bar chart
stacked_bar_hhstatus <- function(data, title = "Default title") {
  plot_ly(
    data |> 
      mutate(
        # TODO: turn this into factor at data processing stage
        AGE_bucket = factor(
          AGE_bucket,
          levels = c("Under 16", "16-17", "18-19", "20-21", "22-23", "24-25", "26-27", "28-29", "30+")
        ),
        # TODO: consider refactoring at data processing stage
        cohabit_bin = forcats::fct_rev(cohabit_bin)
      ),
    x = ~AGE_bucket,
    y = ~percent/100,
    type = 'bar',
    color = ~cohabit_bin,
    colors = c(
      "Not living with parents" = "white",
      "Child provides for parent" = "#46b1d5",
      "Both child and parent are dependent" = "#1e81b0",
      "Child depends on parent" = "#075792",
      "Living in institution" = "lightcoral"
    ),
    text = ~paste(
      "Age Group:", AGE_bucket, "<br>",
      "Cohabit Bin:", cohabit_bin, "<br>",
      "Percent:", scales::percent(percent / 100, accuracy = 0.1), "<br>",
      "Count:", count
    ),
    hoverinfo = "text",
    marker = list(line = list(color = 'black', width = 1)),
    showlegend = TRUE
  ) |>
    layout(
      barmode = 'stack',
      title = title,
      xaxis = list(title = "Age Group"),
      yaxis = list(title = "Percentage", tickformat = ".0%"),
      legend = list(title = list(text = " "))
    )
}

stacked_bar_cohabit <- function(data, title = "Default title") {
  plot_ly(
    data |> 
      mutate(
        # TODO: turn this into factor at data processing stage
        AGE_bucket = factor(
          AGE_bucket,
          levels = c("Under 16", "16-17", "18-19", "20-21", "22-23", "24-25", "26-27", "28-29", "30+")
        ),
        # TODO: consider refactoring at data processing stage
        cohabit_bin = forcats::fct_rev(cohabit_bin)
      ),
    x = ~AGE_bucket,
    y = ~percent/100,
    type = 'bar',
    color = ~OWNERSHPD,
    colors = c(
      "N/A" = "white",
      "Owned free and clear" = "#898fbd",
      "Owned with mortgage or loan" = "#cdc9fb",
      "With cash rent" = "#8ba888",
      "No cash rent" = "#9ab163"
    ),
    text = ~paste(
      "Age Group:", AGE_bucket, "<br>",
      "Cohabit Bin:", cohabit_bin, "<br>",
      "Percent:", scales::percent(percent / 100, accuracy = 0.1), "<br>",
      "Count:", count
    ),
    hoverinfo = "text",
    marker = list(line = list(color = 'black', width = 1)),
    showlegend = TRUE
  ) |>
    layout(
      barmode = 'stack',
      title = title,
      xaxis = list(title = "Age Group"),
      yaxis = list(title = "Percentage", tickformat = ".0%"),
      legend = list(title = list(text = " "))
    )
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
            "percent.",
            "Young adults who live with their parents comprise a large and growing",
            "fraction of the U.S. population. But who are the people who choose to",
            "live with their parents, and what factors make a person more likely to",
            "co-habit?"
          ),

          p(paste(
            "On one hand, living with one's parents could be a sign of greater financial",
            "autonomy. Parental cohabitation could be a habit among more privileged young",
            "adults who save money on rent by living on home. Increased cohabitation could",
            "also reflect the growing fraction of Americans in their teens and twenties",
            "who are attending college.",
            "On the other hand, living with parents could be a sign of growing financial",
            "distress among young adults. As housing costs rise more rapidly than wages,",
            "young adults may be priced out of rental markets.",
            "Another explanation for the growth in parental cohabitation is cultural:",
            "There may simply be declining stigma around the practice."
            )),
          
          p(paste(
            "The effects of parental cohabitation on America's youth are also unclear.",
            "Cohabitation could help young adults accumulate savings. If the practice",
            "is more concentrated among children of wealthier parents, then it may",
            "widen existing wealth gaps over time.",
            "On the other hand, if parental cohabitation inhibits geographic mobility,",
            "it might make young adults financially worse off in the long-run.",
            "For a young adult considering a job opportunity",
            "in a different city, the opportunity costs of moving include the cost of renting",
            "instead of living rent-free (or nearly rent-free) in a city where one's",
            "parents already own a home. This may impede wealth growth, particularly",
            "among young people whose parents own homes in parts of the country that",
            "have seen a secular decline in job opportunities, such as within the rust belt."
          )),
          
          p(paste(
            "Surveys have shown a growing prevalence of parental cohabitation among young",
            "adults [Cite Pew or other surveys here], but little is known about the",
            "sociodemographic profile of these individuals or the factors driving this",
            "trend. In this document, we investigate a few key associative questions:"
          )),
          
          p(paste("To what extent do these patterns differ by race? By gender? Are",
                  "children relying on parents, or are parents relying on children?",
                  "How does cohabitation vary by the homeownership or renter status of the",
                  "household? How does it vary based on the education and income level",
                  "of the child?
                  ")),
          
          tags$h3("Table 1: Homeownership in the United States", id = "table1"),
          
          p(paste("Table 1a shows the fraction of individuals in the United States",
                  "in 2022, by their household status. Table 1b shows these values",
                  "in 2012.")),
          
          p(strong("Table 1")),
          DTOutput("table1"),
          
          tags$h3("Cohabitation by Ownership", id = "graph"),
          p(paste("The graph below shows the fraction of the population living with their",
            "parents by age group. Individuals are filtered by the type of household",
            "they live in: owner-occupied, renter-occupied, or not otherwise specified.",
            "Unsurprisingly, a large fraction of young Americans who live in an owner-occupied",
            "household live with their parents. "
            )),
          
          # Add a row with radio buttons and graph
          fluidRow(
            column(
              width = 2,
              radioButtons(
                "ownership_status_a", 
                label = "Homeownership status:",
                choices = c(
                  "Owned free and clear" = "Owned free and clear",
                  "Owned with mortgage or loan" = "Owned with mortgage or loan",
                  "With cash rent" = "With cash rent",
                  "No cash rent" = "No cash rent",
                  "N/A" = "N/A"
                ),
                selected = "Owned free and clear"
              ),
              radioButtons(
                "year_selection_a", 
                label = "Year:",
                choices = c("2012", "2022"),
                selected = "2012"
              )
            ),
            column(
              width = 10,
              plotlyOutput("ownership_graph", height = "600px")
            )
        ),
        
        p(paste("It may be more informative to look at the question a different way:",
                "given a young person's cohabitation status,",
                "what type of home -- owner-occupied or renter-occupied -- do they",
                "typically inhabit? The results below show that a majority of young",
                "adults in their twenties who do not live with their parents occupy rented",
                "housholds. And a large majority of these same Americans in their",
                "twenties who live with and financially depend on a parent live in owner-",
                "occupied households. There is some ambiguity in defining cohabiting",
                "households where the child relies on the parent versus where the parent",
                "relies on the child. [TODO: add hyperlink to the document defining",
                "how we classify these individuals]. Nevertheless, we can feel somewhat optimistic",
                "that our measure of parental reliance is accurate: the profiles",
                "of young adults who support their parents and of young adults who",
                "don't live with their parents both show similar age patterns skewed",
                "toward rentership. This relationship is consistent with a hypothesis that both these groups are",
                "more financially burdened than their peers who rely on their parents for",
                "shelter, and are therefore unlikely to have already purchased a home."
        )),

        # Add a row with radio buttons and graph
        fluidRow(
          column(
            width = 2,
            radioButtons(
              "cohabit_status_b", 
              label = "Cohabit status:",
              choices = c(
                "Not living with parents" = "Not living with parents",
                "Child provides for parent" = "Child provides for parent",
                "Both child and parent are dependent" = "Both child and parent are dependent",
                "Child depends on parent" = "Child depends on parent",
                "Living in institution" = "Living in institution"
              ),
              selected = "Not living with parents"
            ),
            radioButtons(
              "year_selection_b", 
              label = "Year:",
              choices = c("2012", "2022"),
              selected = "2012"
            )
          ),
          column(
            width = 10,
            plotlyOutput("ownership_graph_cohabit", height = "600px")
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
    selected_status <- input$ownership_status_a
    selected_year <- input$year_selection_a
    
    # Filter data dynamically based on selected year
    data <- if (selected_year == "2012") {
      result$own_age_cohab_2012_se$data
    } else {
      result$own_age_cohab_2022_se$data
    }
    # Dynamically filter data based on selected ownership status
    filtered_data <- dplyr::filter(data, OWNERSHPD == selected_status)
    
    # Generate the graph
    stacked_bar_hhstatus(
      data = filtered_data,
      title = paste(selected_year, "Cohabitation by Household Type: \n", selected_status)
    )
  })
  
  # Render Graph: Cohabitation by Ownership
  output$ownership_graph_cohabit <- renderPlotly({
    selected_status <- input$cohabit_status_b
    selected_year <- input$year_selection_b
    
    # Filter data dynamically based on selected year
    data <- if (selected_year == "2012") {
      result$cohab_age_own_2012_se$data
    } else {
      result$cohab_age_own_2022_se$data
    }
    # Dynamically filter data based on selected ownership status
    filtered_data <- dplyr::filter(data, cohabit_bin == selected_status)
    
    # Generate the graph
    stacked_bar_cohabit(
      data = filtered_data,
      title = paste(selected_year, "Cohabitation by Cohabit Status: \n", selected_status)
    )
  })

}

# Run the application 
shinyApp(ui = ui, server = server)