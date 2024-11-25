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