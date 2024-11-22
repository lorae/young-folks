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
load("fast-facts.rda")

# Define a helper function for a stacked bar chart
stacked_bar_hhstatus <- function(data, title = "Default title") {
  # First, transform data as needed for the plot
  data_for_plot <- data |> 
    mutate(
      AGE_bucket = factor(
        AGE_bucket,
        levels = c("Under 16", "16-17", "18-19", "20-21", "22-23", "24-25", "26-27", "28-29", "30+")
      ),
      cohabit_bin = forcats::fct_rev(cohabit_bin)
    ) |> 
    group_by(AGE_bucket) |> 
    mutate(
      total_parent_dependent = sum(
        percent[cohabit_bin %in% c("Child provides for parent", 
                                   "Both child and parent are dependent", 
                                   "Child depends on parent")]
      ),
      total_all_categories = sum(percent) # Total height of all bars for each age group
    ) |> 
    ungroup()
  
  # Create the plot
  plot <- plot_ly(
    data_for_plot,
    x = ~AGE_bucket,
    y = ~percent / 100,
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
      title = list(
        text = title,
        font = list(size = 14),  # Adjust title size
        x = 0.5,                 # Center title
        y = 1.05                 # Add space above plot for title
      ),
      xaxis = list(
        title = "Age Group",
        titlefont = list(size = 12), # Adjust x-axis title size
        tickfont = list(size = 10)   # Adjust x-axis tick label size
      ),
      yaxis = list(
        title = "Percentage",
        titlefont = list(size = 12), # Adjust y-axis title size
        tickformat = ".0%"
      ),
      legend = list(
        title = list(text = " "),
        font = list(size = 10)
      ),
      margin = list(
        t = 80,  # Top margin for title
        b = 100, # Bottom margin for x-axis title and annotation
        l = 50,  # Left margin
        r = 50   # Right margin
      ),
      annotations = list(
        list(
          x = 0.5,
          y = -0.2, # Move further below the x-axis
          text = "Values displayed are percentage living with a parent (sum of blue categories).",
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "top",
          font = list(size = 10, color = "black")
        )
      )
    )
  
  # Add annotations for the sum of percentages
  plot <- plot |> add_annotations(
    data = data_for_plot |> distinct(AGE_bucket, total_parent_dependent, total_all_categories),
    x = ~AGE_bucket,
    y = ~total_all_categories / 100 + 0.02, # Offset slightly above the top of the bar
    text = ~scales::percent(total_parent_dependent / 100, accuracy = 0.1),
    xanchor = 'center',
    yanchor = 'bottom',
    showarrow = FALSE,
    font = list(size = 10, color = "black")
  )
  
  return(plot)
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
pct_cohabit_2012 <- fast_facts$twenties_2012_cohabit$data |>
  filter(cohabit_bin %in% c(
    "Child depends on parent", 
    "Both child and parent are dependent", 
    "Child provides for parent")) |>
  summarize(total = sum(percent)) |>
  pull(total) |>
  round(digits = 1)
pct_cohabit_2022 <- fast_facts$twenties_2022_cohabit$data |>
  filter(cohabit_bin %in% c(
    "Child depends on parent", 
    "Both child and parent are dependent", 
    "Child provides for parent")) |>
  summarize(total = sum(percent)) |>
  pull(total) |>
  round(digits = 1)


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
          p("Young adults who live with their parents comprise a substantial",
            "fraction of the U.S. population. In 2012,", 
            pct_cohabit_2012,
            "percent of young adults aged 18 to 29 lived with their parents. By 2022,",
            "this figure shrank to",
            pct_cohabit_2022, 
            "percent.",

          ),
          
          p(paste(
            "Tables 1a and 1b show the percentage of young people aged 18-29 who lived",
            "with their parents in 2012 and 2022. Households where the child lives with",
            "a parent are further broken down by who is the (presumable) financial provider:",
            "the child, the parent, or some other individual. [TODO: Link to the methodology",
            "document for this categorization.]",
            "The tables show that while the",
            "total percentage of young adults living with their parents has shrunk in the",
            "past 10 years, the minority group of children supporting their parents has grown."
          )),
          
          tags$h4("Table 1a: Cohabitation in 2012 among Americans ages 18-29", id = "table1a"),
          DTOutput("table1a"),
          tags$h4("Table 1b: Cohabitation in 2022 among Americans ages 18-29", id = "table1b"),
          DTOutput("table1b"),

          p(paste(
            "It's unclear what social, economic, or demographic factors underlie",
            "these aggregate trends. A reduction in parental cohabitation could",
            "be indicative of improving financial lives among young adults, who are better",
            "equipped to support themselves - and potentially others - earlier in their",
            "lives. But it also could also indicate an increased burden in an environment",
            "where housing costs are growing rapidly.",
            "Reduced cohabitation could",
            "also reflect the growing fraction of Americans in their teens and twenties",
            "who are attending college [FACT CHECK]. It's also possible that cultural",
            "norms around parental cohabitation have been changing, making young adults",
            "more averse to the practice [note from Lorae: As a young adult, I anecdotally don't think this",
            "is true. I think living with your parents is becoming less stigmatized over time].",
            "It's also possible that the demographics of the population in the age 18-29",
            "cohort have changed such that those groups with a lesser tendency to live",
            "with their parents are growing in size. This is unlikely - as we show in",
            "section XXX, Hispanic Americans - who are a rapidly growing segment of the",
            "young U.S. population - tend to live with their parents more than people",
            "belonging to other races/ethnicities. And White Americans, who comprise a",
            "shrinking fraction of the U.S. population, tend to cohabit at the lowest rates."
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
            "trend. In this document, we investigate a few key preliminary questions:"
          )),
          
          p(paste("To what extent do these patterns differ by race? By gender? Are",
                  "children relying on parents, or are parents relying on children?",
                  "How does cohabitation vary by the homeownership or renter status of the",
                  "household? How does it vary based on the education and income level",
                  "of the child?
                  ")),
          
          p(paste(
            "TODO: add 95% confidence interval for percents to hover element on graphs"
          )),
          
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
              width = 6,
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
              )),
            column(
              width = 6,
              radioButtons(
                "year_selection_a", 
                label = "Year:",
                choices = c("2012", "2022"),
                selected = "2022"
          ))),
          
        plotlyOutput("ownership_graph", height = "400px"),
        
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
            width = 3,
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
              selected = "2022"
            )
          ),
          column(
            width = 9,
            plotlyOutput("ownership_graph_cohabit", height = "400px")
          )
        ),
        
        tags$h3("Cohabitation by Race and Sex", id = "graph"),
        
        p(paste(
          "Cohabitation varies greatly by both race and sex. Men live with their",
          "their parents much more frequently than women do. Hispanics and those",
          "who self-identify their race as \"other\" tend to cohabit at a higher rate,",
          "while Whites tend to live with their parents at the lowest rate." 
        )),
        
        fluidRow(
          column(
            width = 4,
            radioButtons(
              "race_selection_c",
              label = "Race / Ethnicity",
              choices = c(
                "Asian American / Pacific Islander" = "AAPI",
                "American Indian / Alaska Native" = "AIAN",
                "Black" = "Black",
                "Hispanic" = "Hispanic",
                "Multiracial" = "Multiracial",
                "White" = "White",
                "Other" = "Other",
                "All" = "All"
              ),
              selected = "All"
            )),
          column(
            width = 4,
            radioButtons(
              "sex_selection_c", 
              label = "Gender:",
              choices = c(
                "Male" = "Male",
                "Female" = "Female",
                "All" = "All"
              ),
              selected = "All"
          )),
          column(
            width = 4,
            radioButtons(
              "year_selection_c", 
              label = "Year:",
              choices = c("2012", "2022"),
              selected = "2022"
          ))
        ),

        plotlyOutput("race_sex_cohabit", height = "400px"),
        
        tags$h3("Cohabitation over time", id = "graph"),
        
        p(paste(
          "The percentage of Americans supporting their parents has grown over the",
          "past decade, particularly among xxx."
        ))

        )
      )
  )
)

# Define server
server <- function(input, output, session) {
  
  output$table1a <- renderDT({
    # Get the critical value for 95% confidence level
    z_value <- qnorm(0.975)
    
    datatable(
      fast_facts$twenties_2012_cohabit$data |>
        mutate(
          lower_ci = round(percent - z_value * se_percent, 2),  # Calculate lower CI
          upper_ci = round(percent + z_value * se_percent, 2),  # Calculate upper CI
          percent = paste0(round(percent, 2), "%"),  # Add percentage sign
          `95% CI` = paste0("(", lower_ci, "%, ", upper_ci, "%)"),  # Format CI
          `Estimated Population` = scales::comma(weighted_count),  # Format with commas
          `Number Surveyed` = scales::comma(count)  # Format with commas
        ) |>
        select(
          "Cohabitation Type" = cohabit_bin,
          `Estimated Population`,
          `Number Surveyed`,
          "Percentage" = percent,
          `95% CI`
        ),
      options = list(
        pageLength = 5,
        searching = FALSE,
        lengthChange = FALSE,
        paging = FALSE,
        info = FALSE),
      rownames = FALSE
    )
  })
  
  output$table1b <- renderDT({
    # Get the critical value for 95% confidence level
    z_value <- qnorm(0.975)
    
    datatable(
      fast_facts$twenties_2022_cohabit$data |>
        mutate(
          lower_ci = round(percent - z_value * se_percent, 2),  # Calculate lower CI
          upper_ci = round(percent + z_value * se_percent, 2),  # Calculate upper CI
          percent = paste0(round(percent, 2), "%"),  # Add percentage sign
          `95% CI` = paste0("(", lower_ci, "%, ", upper_ci, "%)"),  # Format CI
          `Estimated Population` = scales::comma(weighted_count),  # Format with commas
          `Number Surveyed` = scales::comma(count)  # Format with commas
        ) |>
        select(
          "Cohabitation Type" = cohabit_bin,
          `Estimated Population`,
          `Number Surveyed`,
          "Percentage" = percent,
          `95% CI`
        ),
      options = list(
        pageLength = 5,
        searching = FALSE,
        lengthChange = FALSE,
        paging = FALSE,
        info = FALSE),
      rownames = FALSE
    )
  })
  
  # Table 1: Render theoretical example table
  output$table1 <- renderDT({
    
    own_2012_se <- result$own_2012_se$data |>
      mutate(
        year = 2012,
        percent = percent/100
      )
    
    own_2022_se <- result$own_2022_se$data |>
      mutate(
        year = 2022,
        percent = percent/100
      )
    
    example_table <- bind_rows(
      own_2012_se, 
      own_2022_se
      ) 
      
      datatable(
        example_table,
        options = list(
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
      title = paste(selected_year, "Cohabitation by Age: \n", selected_status, "households")
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
      title = paste(selected_year, "Renter/Homeownership Status by Age: \n", selected_status)
    )
  })
  
  # Cohabitation by Race and SEx
  output$race_sex_cohabit <- renderPlotly({
    selected_sex <- input$sex_selection_c
    selected_year <- input$year_selection_c
    selected_race <- input$race_selection_c
    
    # Dynamically filter data based on radio button selection
    filtered_data <- race_sex_summary |>
      dplyr::filter(SEX == selected_sex & YEAR == selected_year & RACE_ETH_bucket == selected_race)
    
    # Dynamic graph title based on radio button user inputs
    if (selected_sex == "Male") {
      title_custom = paste(selected_race, "Men")
    } else if (selected_sex == "Female") {
      title_custom = paste(selected_race, "Women")
    } else {
      if (selected_race == "All") {
        title_custom = "All Genders, All Races"
      } else {
        title_custom = paste("All Genders,", selected_race)
      }
    }
    # Generate the graph
    stacked_bar_hhstatus(
      data = filtered_data,
      title = paste(selected_year, "Cohabitation Status by Age: \n", title_custom)
    )
  })

}

# Run the application 
shinyApp(ui = ui, server = server)