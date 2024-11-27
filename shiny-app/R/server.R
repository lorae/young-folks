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
  
  # Line names
  line_names <- c(
    "Line 1" = "AAPI",
    "Line 2" = "AIAN",
    "Line 3" = "Black",
    "Line 4" = "Hispanic",
    "Line 5" = "Multiracial",
    "Line 6" = "White",
    "Line 7" = "Other",
    "Line 8" = "All"
  )
  
  output$checkbox_with_dropdowns <- renderUI({
    # Generate a row with a checkbox and a dropdown for each race
    rows <- lapply(names(line_names), function(name) {
      fluidRow(
        column(
          width = 2,
          checkboxInput(
            inputId = paste0("checkbox_", line_names[name]),
            label = name,
            value = FALSE
          )
        ),
        column(
          width = 2,
          selectInput(
            inputId = paste0("dropdown_race_", line_names[name]),
            label = NULL,  # No label for the dropdown
            choices = c(
              "All races" = "All", 
              "Asian American / Pacific Islander" = "AAPI", 
              "American Indian / Alaska Native" = "AIAN",
              "Black or African American" = "Black",
              "Hispanic or Latino" = "Hispanic",
              "Multiracial" = "Multiracial",
              "White" = "White",
              "Other" = "Other"
              ),
            selected = "All"
          )
        ),
        column(
          width = 2,
          selectInput(
            inputId = paste0("dropdown_sex_", line_names[name]),
            label = NULL,
            choices = c(
              "All sexes" = "All",
              "Men" = "Male",
              "Women" = "Female"
            ),
            selected = "All"
          )
        ),
        column(
          width = 4,
          selectInput(
            inputId = paste0("dropdown_cohabit_", line_names[name]),
            label = NULL,
            choices = c(
              "Not living with parents",
              "Child provides for parent",
              "Both child and parent are dependent",
              "Child depends on parent",
              "Living in institution"
            ),
            selected = "Not living with parents"
          )
        )

        )
    })
    do.call(tagList, rows)  # Combine all rows into a single output
  })
  
  output$debug_output <- renderUI({
    # Iterate over line_names to extract selected options
    debug_text <- lapply(names(line_names), function(name) {
      if (input[[paste0("checkbox_", line_names[name])]]) {
        # Get selected values from dropdowns
        selected_race <- input[[paste0("dropdown_race_", line_names[name])]]
        selected_sex <- input[[paste0("dropdown_sex_", line_names[name])]]
        selected_cohabit <- input[[paste0("dropdown_cohabit_", line_names[name])]]
        
        # Create a line of text for the selected line
        paste0(name, ": ", selected_race, ", ", selected_sex, ", ", selected_cohabit)
      }
    })
    
    # Remove NULL entries (unchecked checkboxes)
    debug_text <- debug_text[!sapply(debug_text, is.null)]
    
    # Render the debugging text as a list of paragraphs
    tagList(lapply(debug_text, function(line) {
      p(line)
    }))
  })
  
  filtered_data_for_line <- reactive({
    filtered <- data.frame()
    
    for (name in names(line_names)) {
      if (input[[paste0("checkbox_", line_names[name])]]) {
        selected_race <- input[[paste0("dropdown_race_", line_names[name])]]
        selected_sex <- input[[paste0("dropdown_sex_", line_names[name])]]
        selected_cohabit <- input[[paste0("dropdown_cohabit_", line_names[name])]]
        
        filtered_line <- cohabit_over_time %>%
          filter(
            RACE_ETH_bucket == selected_race,
            SEX == selected_sex,
            cohabit_bin == selected_cohabit
          ) %>%
          mutate(id = name)
        
        filtered <- bind_rows(filtered, filtered_line)
      }
    }
    
    filtered
  })
  
  output$line_graph <- renderPlotly({
    req(filtered_data_for_line())  # Ensure the filtered data is available
    
    line_cohabit(
      data = filtered_data_for_line(),
      title = "Test Title"
    )
  })
  
  output$debug_filtered_table <- renderTable({
    req(filtered_data_for_line())  # Ensure data is available
    filtered_data_for_line()       # Display the filtered data
  })

}