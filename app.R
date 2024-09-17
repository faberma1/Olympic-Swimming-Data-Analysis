library(tidyverse)
library(shiny)
library(shinydashboard)
library(stringr)
library(glue)
library(gt)
library(mgcv)
library(rsconnect)

setwd("/Users/shanefaberman/Downloads/final_sail_present")

swimming <- read_csv("Olympic_Swimming_Results_1912to2020.csv") |> 
  janitor::clean_names()

before_paris <- read_csv("olympic_medals.csv.zip") |> 
  janitor::clean_names() |> 
  rename(country = committee, 
         team = code)

before_paris_duplicates <- before_paris |> 
  count(team) |> 
  filter(n > 1)

before_paris_unique <- before_paris |> 
  distinct(team, .keep_all = TRUE)

swimming <- swimming |>  
  left_join(before_paris_unique, by = "team")

swimming <- swimming |> 
  select(location, year, distance_in_meters, stroke, relay, gender.x, team, athlete, results, rank, country)


ui <- dashboardPage(
  dashboardHeader(title = tags$span(
    "Olympic Swimming Data Analysis", 
    style = "font-size: 14px; white-space: nowrap; overflow: visible; text-overflow: clip;"
  )),
  dashboardSidebar(
    tags$style(HTML("
      .sidebar-menu li a {
        font-size: 18px;
        padding: 12px;
      }
      .selectize-input, .selectize-dropdown {
        font-size: 18px;
        height: 40px;
      }
      .selectize-dropdown-content {
        font-size: 18px;
      }
    ")),
    sidebarMenu(
      menuItem("Country", tabName = "country", icon = icon("globe")),
      menuItem("Heat Map", tabName = "heatmap", icon = icon("globe")),
      menuItem("Individual", tabName = "individual", icon = icon("user")),
      menuItem("Medal Time Projections", tabName = "future", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "country",
              fluidRow(
                column(width = 8, box(
                  plotOutput("plot5", height = 350), 
                  status = "primary", 
                  width = NULL
                )),
                column(width = 4, box(
                  selectInput("country", "Country: ", 
                              choices = c("All", unique(swimming$country)),
                              selected = "All"),
                  selectInput("distance", "Distance:",
                              choices = c("All", unique(swimming$distance_in_meters)),
                              selected = "All"),
                  selectInput("stroke", "Stroke: ",
                              choices = c("All", unique(swimming$stroke)),
                              selected = "All"),
                  selectInput("gender", "Gender: ",
                              choices = c("All", unique(swimming$gender.x)),
                              selected = "All"),
                  status = "primary", 
                  width = NULL
                ))
              ),
              fluidRow(
                column(width = 12, box(
                  plotOutput("plot6", height = 300), 
                  status = "primary",
                  width = NULL
                ))
              )
      ),
      tabItem(tabName = "heatmap",
              fluidRow(
                column(width = 12, box(
                  plotOutput("plot7", height = 500),
                  status = "primary",
                  width = NULL
                ))
              ),
              fluidRow(column(width = 12, box(
                selectInput("country", "Country: ",
                            choices = c("All", unique(swimming$country)),
                            selected = "All"),
                status = "primary",
                width = NULL
              )))
      ),
      tabItem(tabName = "individual",
              fluidRow(
                column(width = 7, box(
                  plotOutput("plot1", height = 250), 
                  status = "primary",
                  width = NULL
                )),
                column(width = 5, box(
                  plotOutput("plot2", height = 60),
                  selectInput("swimmer", "Swimmer: ", 
                              choices = c(unique(swimming$athlete)),
                              selected = "Michael Phelps"),
                  selectInput("distance", "Distance:",
                              choices = c("All", unique(swimming$distance_in_meters)),
                              selected = "All"),
                  selectInput("stroke", "Stroke: ",
                              choices = c("All", unique(swimming$stroke)),
                              selected = ),
                  status = "primary",
                  width = NULL
                ))
              ),
              fluidRow(
                column(width = 6, box(
                  plotOutput("plot3", height = 350), 
                  status = "primary",
                  width = NULL
                )),
                column(width = 6, box(
                  htmlOutput("plot4"), 
                  height = 372, 
                  status = "primary",
                  width = NULL
                ))
              )
      ),
      tabItem(
        tabName = "future",
        fluidRow(
          # Adjust the width to 12 to make the plot box take up full width of the row
          column(width = 12, 
                 box(plotOutput("plot8", height = 300), 
                     status = "primary", 
                     width = 12)  # Full width for the plot
          )
        ),
        fluidRow(
          # Adjust the dropdown inputs section to 4 columns for more space
          column(width = 4, 
                 box(
                   selectInput("distance", "Distance:",
                               choices = c("All", unique(swimming$distance_in_meters)),
                               selected = "All"),
                   selectInput("stroke", "Stroke: ",
                               choices = c("All", unique(swimming$stroke)),
                               selected = "All"),
                   selectInput("gender", "Gender: ",
                               choices = c("All", unique(swimming$gender.x)),
                               selected = "All"),
                   status = "primary",
                   width = 12  # Full width for the dropdowns
                 )
          ),
          # Adjust the table section to 8 columns for more space
          column(width = 8, 
                 box(
                   gt_output("future_predictions_table"), 
                   status = "primary", 
                   width = 12  # Full width for the table
                 )
          )
        )
      )
      
      
    )
  )
)

  
server <- function(input, output) {
  
  reactive_country <- reactive({
    # Filter data based on swimmer input
    swimmer_row <- swimming |>  
      filter(str_detect(athlete, input$swimmer)) |> 
      select(country) |>   # Ensure country column is included
      distinct()
    
    # Extract the country for the selected swimmer
    if (nrow(swimmer_row) > 0) {
      swimmer_row$country[1]  # Assuming 'country' is the column with the country name
    } else {
      "No country available"  # Fallback text if no swimmer is found
    }
  })
  
  output$plot1 <- renderPlot({
    # Check for empty input
    if (input$swimmer == "") {
      return(NULL)  # Avoid plotting if input is empty
    }
    
    # Filter data based on swimmer input
    filtered_data <- swimming |>
      filter(str_detect(athlete, input$swimmer))
    
    # Apply distance filter if not "All"
    if (input$distance != "All") {
      filtered_data <- filtered_data |>
        filter(distance_in_meters == input$distance)
    }
    
    # Process the filtered data
    filtered <- filtered_data |>
      filter(if (input$stroke != "All") stroke == input$stroke else TRUE) |>
      group_by(stroke) |>
      summarize(n = n()) |>
      mutate(prop = n / sum(n)) |>
      arrange(desc(stroke)) |>
      mutate(ypos = cumsum(prop) - prop / 2)
    
    # Check if filtered data is empty
    if (nrow(filtered) == 0) {
      # Display "No data" message
      ggplot() +
        geom_blank() +
        annotate("text", x = 1, y = 1, label = "No data recorded", size = 6, color = "black", fontface = "bold") +
        theme_void()
    } else {
      # Plot with actual data
      ggplot(filtered, aes(x = "", y = prop, fill = stroke)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void() +
        labs(title = glue("{input$swimmer}'s Events by Stroke"),
             subtitle = glue("Distance: {input$distance}")) +
        theme(legend.position = "none",
              plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
              plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5)) +
        geom_text(aes(y = ypos, label = paste0(stroke, "\n", scales::percent(prop))),
                  color = "black", size = 3, fontface = "bold") +
        scale_fill_brewer(palette = "Set2")
    }
  })
  
  output$plot2 <- renderPlot({
    country <- reactive_country()  # Get the reactive country value
    
    ggplot() + 
      geom_blank() + 
      annotate("text", x = 1, y = 1, label = glue("{country}"), size = 6, color = "black", fontface = "bold") + 
      theme_void()
  })
  
  output$plot3 <- renderPlot({
    medal_levels <- c("Gold", "Silver", "Bronze", "No Medal", "No Data", "Disqualified")
    
    # Filter data based on swimmer and conditionally filter for distance
    medal_data <- swimming |>
      filter(str_detect(athlete, input$swimmer)) |>
      filter(if (input$distance != "All") distance_in_meters == input$distance else TRUE) |>
      filter(if (input$stroke != "All") stroke == input$stroke else TRUE) |> 
      mutate(medal = recode(rank,
                            `1` = "Gold",
                            `2` = "Silver",
                            `3` = "Bronze",
                            `4` = "No Medal",
                            `5` = "No Data",
                            `0` = "Disqualified",
                            .default = "Unknown")) |>
      mutate(medal = factor(medal, levels = medal_levels)) |>
      count(medal) |>
      complete(medal = medal_levels, fill = list(n = 0))  # Ensure all medal levels are present
    
    # Check if all counts are zero
    if (sum(medal_data$n) == 0) {
      ggplot() + 
        geom_blank() + 
        annotate("text", x = 1, y = 1, label = "No data recorded", size = 6, color = "black", fontface = "bold") +
        theme_void()
    } else {
      # Plot with bars and labels
      ggplot(medal_data, aes(x = medal, y = n)) +  
        geom_bar(stat = "identity", fill = "#4B9CD3", color = "black", width = 0.7) +  # Adjust bar width
        geom_text(aes(label = n), vjust = -0.5) +  # Labels for counts above bars
        labs(title = glue("Distribution of Race Results for {input$swimmer}"),
             subtitle = glue("Distance: {input$distance}, Stroke: {input$stroke}"),
             x = "Medal", 
             y = "Count") + 
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Prevent negative values
        scale_x_discrete(limits = medal_levels) +  # Explicitly set x-axis limits
        theme_minimal() + 
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
              plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 12),
              axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity
    }
  })
  
  output$plot4 <- render_gt({
    # Convert `results` to numeric `time` values with improved handling of NA cases
    swimming <- swimming |>
      mutate(time = case_when(
        str_count(results, ":") == 2 ~ { # Format HH:MM:SS.SSS
          time_parts <- str_split_fixed(results, ":", 3)
          as.numeric(time_parts[, 1]) * 3600 +  # Convert hours to seconds
            as.numeric(time_parts[, 2]) * 60 +   # Convert minutes to seconds
            as.numeric(time_parts[, 3])          # Seconds (including fractions)
        },
        str_count(results, ":") == 1 ~ { # Format MM:SS.SSS
          time_parts <- str_split_fixed(results, ":", 2)
          as.numeric(time_parts[, 1]) * 60 +   # Convert minutes to seconds
            as.numeric(time_parts[, 2])          # Seconds (including fractions)
        },
        TRUE ~ as.numeric(results) # For already numeric times or unrecognized formats
      ))
    
    
    # Filter and calculate time summary for selected swimmer and distance
    filtered_data <- swimming |>
      filter(str_detect(athlete, input$swimmer)) |>
      filter(if (input$stroke != "All") stroke == input$stroke else TRUE) |>
      filter(if (input$distance != "All") distance_in_meters == input$distance else TRUE)
    
    # Check if filtered data is empty or contains only NA times
    if (nrow(filtered_data) == 0 | all(is.na(filtered_data$time))) {
      return(
        data.frame(Statistic = "No data recorded", Time = NA) |>
          gt() |>
          tab_header(title = "Summary Statistics for Race Times") |>
          cols_label(Statistic = "Statistic", Time = "Time (seconds)")
      )
    }
    
    # Calculate summary statistics
    time_summary <- filtered_data |>
      summarise(
        Maximum = max(time, na.rm = TRUE),
        Minimum = min(time, na.rm = TRUE),
        Mean = mean(time, na.rm = TRUE),
        Median = median(time, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Handle case where summary contains only NAs or invalid data
    if (all(is.na(time_summary))) {
      return(
        data.frame(Statistic = "No data recorded", Time = NA) |>
          gt() |>
          tab_header(title = "Summary Statistics for Race Times") |>
          cols_label(Statistic = "Statistic", Time = "Time (seconds)")
      )
    }
    
    # Format the time summary for display
    time_summary_long <- time_summary |>
      pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Time") |>
      mutate(Time = scales::comma(Time, accuracy = 0.01))
    
    time_summary_long |>
      gt() |>
      tab_header(
        title = glue("Summary Statistics for {input$distance} (Stroke: {input$stroke}) Times")
      ) |>
      cols_label(
        Statistic = "Statistic",
        Time = "Time (seconds)"
      ) |>
      opt_stylize(style = 6, color = 'blue')
  })
  
  
  
  output$plot5 <- renderPlot({
    # Filter individual medals
    individual_medals <- swimming |>
      filter(if (input$country != "All") country == input$country else TRUE) |>
      filter(rank %in% c(1, 2, 3)) |>
      filter(relay == 0) |>
      filter(year >= 2010) |> 
      filter(if (input$distance != "All") distance_in_meters == input$distance else TRUE) |>
      filter(if (input$gender != "All") gender.x == input$gender else TRUE) |>
      filter(if (input$stroke != "All") stroke == input$stroke else TRUE) |> 
      group_by(athlete) |>
      summarise(individual_medals = n(), .groups = 'drop')
    
    # Filter relay medals
    relay_medals <- swimming |>
      filter(if (input$country != "All") country == input$country else TRUE) |>
      filter(rank %in% c(1, 2, 3)) |>
      filter(relay == 1) |> 
      filter(year >= 2010) |> 
      filter(if (input$stroke != "All") stroke == input$stroke else TRUE) |> 
      filter(if (input$distance != "All") distance_in_meters == input$distance else TRUE) |>
      filter(if (input$gender != "All") gender.x == input$gender else TRUE) |>
      separate_rows(athlete, sep = ",") |>
      mutate(athlete = str_trim(athlete)) |>
      group_by(athlete) |>
      summarise(relay_medals = n(), .groups = 'drop')
    
    # Combine data and convert to long format
    total_medals <- full_join(individual_medals, relay_medals, by = "athlete") |>
      mutate(individual_medals = coalesce(individual_medals, 0), 
             relay_medals = coalesce(relay_medals, 0))
    
    medals_long <- total_medals |>
      pivot_longer(cols = c(individual_medals, relay_medals),
                   names_to = "medal_type",
                   values_to = "medal_count")
    
    # Identify top 10 athletes based on total medals
    top_athletes <- total_medals |>
      mutate(total_medals = individual_medals + relay_medals) |>
      arrange(desc(total_medals)) |>
      slice_head(n = 10) |>
      pull(athlete)
    
    medals_long_filtered <- medals_long |>
      filter(athlete %in% top_athletes)
    
    validate(
      need(nrow(medals_long_filtered) > 0, "No data available for the selected country and distance.")
    )
    
    # Plot
    ggplot(medals_long_filtered, aes(x = reorder(athlete, medal_count), y = medal_count, fill = medal_type)) +
      geom_bar(stat = "identity", position = "stack", color = "black") +
      coord_flip() +
      labs(title = glue("Most Swimming Medals Won Since 2010: {input$country}"),
           subtitle = glue("Stroke: {input$stroke}, Distance: {input$distance}, Gender: {input$gender}"),
           x = "Athlete", 
           y = "Number of Medals", 
           fill = "Medal Type") +
      scale_fill_manual(values = c("individual_medals" = "lightgoldenrodyellow", "relay_medals" = "orange"), 
                        labels = c("individual_medals" = "Individual", "relay_medals" = "Relay")) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
            plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 12))
  })
  
  
  output$plot6 <- renderPlot({
    # Filter out rows with missing results
    swimming_prepared <- swimming |>
      filter(!is.na(results)) |>
      mutate(time = case_when(
        str_count(results, ":") == 2 ~ {
          time_parts <- str_split_fixed(results, ":", 3)
          as.numeric(time_parts[, 1]) * 3600 +
            as.numeric(time_parts[, 2]) * 60 +
            as.numeric(time_parts[, 3])
        },
        str_count(results, ":") == 1 ~ {
          time_parts <- str_split_fixed(results, ":", 2)
          as.numeric(time_parts[, 1]) * 60 +
            as.numeric(time_parts[, 2])
        },
        TRUE ~ as.numeric(results)
      )) |>
      filter(if (input$country != "All") country == input$country else TRUE) |> 
      filter(if (input$stroke != "All") stroke == input$stroke else TRUE) |> 
      filter(if (input$gender != "All") gender.x == input$gender else TRUE) |> 
      filter(year >= 2010)
    
    # Check if "All" is selected in the distance input
    if (input$distance == "All") {
      summary_data <- swimming_prepared |>
        group_by(distance_in_meters) |>
        summarise(n = n(), .groups = 'drop')
      
      # Validate that there is data to plot
      validate(
        need(nrow(summary_data) > 0, "No data available for the selected country.")
      )
      
      ggplot(summary_data, aes(x = distance_in_meters, y = n)) +
        geom_bar(stat = "identity", fill = "steelblue", color = "black") +
        geom_text(aes(label = n), vjust = -0.5) +
        labs(title = glue("Number of Total Races by Distance for {input$country} Swimmers Since 2010"),
             subtitle = glue("Stroke: {input$stroke}, Gender: {input$gender}"),
             x = "Distance (meters)",
             y = "Number of Races") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
              plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 12))
      
    } else {
      swimming_prepared <- swimming_prepared |>
        filter(distance_in_meters == input$distance)
      
      validate(
        need(nrow(swimming_prepared) > 0, glue("No data available for {input$distance} meters and {input$country}."))
      )
      
      time_range <- range(swimming_prepared$time)
      data_range <- diff(time_range)
      
      number_of_bins <- max(3, floor(data_range / 10))  # Adjust divisor (10) as needed
      binwidth <- data_range / number_of_bins
      
      ggplot(swimming_prepared, aes(x = time)) +
        geom_histogram(binwidth = binwidth, fill = "steelblue", color = "black") +
        labs(title = glue("Distribution of Times for {input$distance} ({input$country} Swimmers Since 2010)"),
             subtitle = glue("Stroke: {input$stroke}, Gender: {input$gender}"),
             x = "Time (seconds)",
             y = "Count") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
              plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 12))
    }
  })
  
  
  
  output$plot7 <- renderPlot({
    medal_levels <- c("Gold", "Silver", "Bronze", "No Medal", "No Data", "Disqualified")
    
    summarized_data <- swimming |> 
      filter(if (input$country != "All") country == input$country else TRUE)|> 
      filter(year >= 2010) |> 
      mutate(medal = recode(rank,
                            `1` = "Gold",
                            `2` = "Silver",
                            `3` = "Bronze",
                            `4` = "No Medal",
                            `5` = "No Data",
                            `0` = "Disqualified",
                            .default = "Unknown")) |> 
      filter(medal %in% c("Gold", "Silver", "Bronze")) |> 
      group_by(stroke, distance_in_meters) |> 
      summarise(total_medals = sum(medal %in% c("Gold", "Silver", "Bronze")), .groups = 'drop')
    
    # Create a complete grid of stroke and distance combinations
    complete_grid <- expand.grid(stroke = unique(summarized_data$stroke),
                                 distance_in_meters = unique(summarized_data$distance_in_meters))
    
    # Join with the summarized data to ensure all combinations are included
    full_data <- complete_grid |> 
      left_join(summarized_data, by = c("stroke", "distance_in_meters")) |> 
      replace_na(list(total_medals = 0))  # Replace NA values with 0 for missing data
    
    if (sum(full_data$total_medals) == 0) {
      ggplot() + 
        geom_blank() + 
        annotate("text", x = 1, y = 1, label = "No Medals Won", size = 6, color = "black", fontface = "bold") +
        theme_void()
    } else { 
    ggplot(full_data, aes(x = stroke, y = distance_in_meters, fill = total_medals)) +
      geom_tile(color = "black") +  # Add border color to tiles
      scale_fill_gradient2(low = "white", mid = "lightcoral", high = "blue", midpoint = 0, 
                           na.value = "white") +  # Set color for NA values
      labs(title = "Total Number of Medals by Stroke and Distance Since 2010",
           x = "Stroke",
           y = "Distance (meters)",
           fill = "Total Medals") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
  }})
  
  output$plot8 <- renderPlot({
    # Convert results to numeric time
    model_test <- swimming |>  
      mutate(time = case_when(
        str_count(results, ":") == 2 ~ { # Format HH:MM:SS.SSS
          time_parts <- str_split_fixed(results, ":", 3)
          as.numeric(time_parts[, 1]) * 3600 +  # Convert hours to seconds
            as.numeric(time_parts[, 2]) * 60 +   # Convert minutes to seconds
            as.numeric(time_parts[, 3])          # Seconds (including fractions)
        },
        str_count(results, ":") == 1 ~ { # Format MM:SS.SSS
          time_parts <- str_split_fixed(results, ":", 2)
          as.numeric(time_parts[, 1]) * 60 +   # Convert minutes to seconds
            as.numeric(time_parts[, 2])          # Seconds (including fractions)
        },
        TRUE ~ as.numeric(results) # For already numeric times
      ))
    
    if (input$distance == "All" | input$stroke == "All") {
      # Display "N/A" message
      ggplot() +
        geom_blank() +
        annotate("text", x = 1, y = 1, label = "N/A", size = 6, color = "black", fontface = "bold") +
        theme_void()
    } else {
      # Filter data and plot
      model_test |>
        filter(stroke == input$stroke & distance_in_meters == input$distance) |>
        filter(rank %in% c(1, 2, 3)) |>
        filter(gender.x == input$gender) |>
        group_by(year) |>
        summarise(avg_medal_time = mean(time, na.rm = TRUE), .groups = 'drop') |> # Added na.rm = TRUE and .groups = 'drop'
        ggplot(aes(x = year, y = avg_medal_time)) +
        geom_point() +
        geom_smooth(se = FALSE) +
        labs(title = glue("{input$stroke} {input$distance} Medal Times Through the Years ({input$gender})"),
             x = "Year",
             y = "Average Medal Time") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
    }
  })
  
  output$future_predictions_table <- render_gt({
    # Data transformation
    model_test <- swimming |> 
      mutate(time = case_when(
        str_count(results, ":") == 2 ~ { # Format HH:MM:SS.SSS
          time_parts <- str_split_fixed(results, ":", 3)
          as.numeric(time_parts[, 1]) * 3600 +  # Convert hours to seconds
            as.numeric(time_parts[, 2]) * 60 +   # Convert minutes to seconds
            as.numeric(time_parts[, 3])          # Seconds (including fractions)
        },
        str_count(results, ":") == 1 ~ { # Format MM:SS.SSS
          time_parts <- str_split_fixed(results, ":", 2)
          as.numeric(time_parts[, 1]) * 60 +   # Convert minutes to seconds
            as.numeric(time_parts[, 2])          # Seconds (including fractions)
        },
        TRUE ~ as.numeric(results) # For already numeric times
      )) 
    
    # Filter data and calculate average medal time by year
    model_data <- model_test |>
      filter(stroke == input$stroke & distance_in_meters == input$distance) |> 
      filter(rank %in% c(1, 2, 3)) |> 
      filter(gender.x == input$gender) |> 
      group_by(year) |> 
      summarise(avg_medal_time = mean(time))
    
    gam_model <- gam(avg_medal_time ~ s(year), data = model_data)
 
    future_years <- data.frame(year = c(2024, 2028, 2032))
    
    predicted_times <- predict(gam_model, newdata = future_years)
    
    future_predictions <- data.frame(Year = future_years$year, Predicted_Medal_Time = predicted_times)
    
    if (nrow(model_data) < 3) {
      # Return a data frame with a single row for "No data recorded"
      data.frame(Year = "N/A", Predicted_Medal_Time = "N/A") |>
        gt() |>
        tab_header(
          title = glue("Predicted times for {input$gender}'s {input$stroke} {input$distance}")
        ) |>
        cols_label(
          Year = "Year",
          Predicted_Medal_Time = "Predicted Medal Time (seconds)"
        )
    } else {
      future_predictions |> 
        gt() |> 
        tab_header(
          title = glue("Predicted times for {input$gender}'s {input$stroke} {input$distance}")
        ) |>
        fmt_number(
          columns = "Predicted_Medal_Time",
          decimals = 2
        ) |>
        cols_label(
          Year = "Year",
          Predicted_Medal_Time = "Predicted Medal Time (seconds)"
        ) |>
        opt_stylize(style = 6, color = 'blue')
    }
  })
  
  
}

shinyApp(ui, server)