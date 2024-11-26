# source needed variables using global.R
source("R/global.R")

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
  
  output$line_graph <- renderPlotly({
    selected_sex <- input$sex_selection_d
    selected_race <- input$race_selection_d
    
    filtered_data <- cohabit_over_time |>
      filter(
        cohabit_bin == "Not living with parents" &
        SEX == selected_sex &
        RACE_ETH_bucket == selected_race
      )
    
    line_cohabit(
      data = filtered_data,
      title = "Test title"
    )
  })

}