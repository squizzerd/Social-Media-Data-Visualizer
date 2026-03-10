library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggExtra)
library(stringr)
library(purrr)
library(DT)

# UI
ui <- fluidPage(
  titlePanel("Instagram Analytics Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Instagram CSV",
                accept = c(".csv")),
      
      hr(),
      
      uiOutput("impressions_slider"),
      
      hr(),
      
      uiOutput("engagements_slider"),
      
      hr(),
      
      checkboxGroupInput("post_type", 
                         "Post Types:",
                         choices = c("Reel", "Post", "Story"),
                         selected = c("Reel", "Post")),
      
      hr(),
      
      selectInput("color_by", 
                  "Color Points By:",
                  choices = c("Content.Type"),
                  selected = "Content.Type"),
      
      hr(),
      
      selectInput("tag_types", 
                  "Choose Tags:",
                  choices = c("All Tags"),
                  selected = "All Tags"),
      
      hr(),
      
      dateRangeInput("date_range", 
                     "Date Range:",
                     start = NULL,
                     end = NULL),
      
      hr(),
      
      actionButton("reset", "Reset Filters", 
                   class = "btn-warning")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot",
                 plotOutput("scatter_plot", height = "600px"),
                 hr(),
                 verbatimTextOutput("r_squared")),
        
        tabPanel("Summary Statistics",
                 h4("Data Summary"),
                 verbatimTextOutput("summary_stats"),
                 hr(),
                 h4("Filtered Data"),
                 DTOutput("data_table")),
        
        tabPanel("Tag Analysis",
                 h4("Most Common Tags"),
                 plotOutput("tag_plot"),
                 hr(),
                 DTOutput("tag_table")),
        
        tabPanel("Time Series",
                 plotOutput("time_series"),
                 hr(),
                 plotOutput("engagement_rate"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Function to split tags
  split_tags <- function(tag_string) {
    if (is.na(tag_string) || tag_string == "") {
      return(character(0))
    }
    
    temp_string <- gsub("Student Centered, Innovation Driven", 
                        "SCID_placeholder", 
                        tag_string, 
                        fixed = TRUE)
    
    tags <- str_split(temp_string, ",")[[1]] %>%
      str_trim()
    
    tags <- gsub("SCID_placeholder", 
                 "Student Centered, Innovation Driven", 
                 tags, 
                 fixed = TRUE)
    
    return(tags)
  }
  
  # Reactive: Load and process data
  raw_data <- reactive({
    req(input$file)
    
    tryCatch({
      data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      
      # Check if required columns exist
      required_cols <- c("Date", "Post.Type", "Content.Type", "Impressions", 
                         "Reach", "Engagements", "Likes", "Comments", 
                         "Shares", "Saves", "Video.Views", "Tags")
      
      missing_cols <- setdiff(required_cols, names(data))
      if (length(missing_cols) > 0) {
        showNotification(
          paste("Missing columns:", paste(missing_cols, collapse = ", ")),
          type = "error",
          duration = NULL
        )
        return(NULL)
      }
      
      data <- data %>%
        mutate(Date = mdy_hm(Date)) %>%
        select(Date, Post.Type, Content.Type, Impressions, Reach, Engagements,
               Likes, Comments, Shares, Saves, Video.Views, Tags) %>%
        mutate(across(c(Impressions, Reach, Engagements, Likes, Comments, 
                        Shares, Saves, Video.Views), 
                      ~as.numeric(gsub(",", "", as.character(.))))) %>%
        mutate(Date = as.Date(Date)) %>%
        arrange(Date)
      
      showNotification("Data loaded successfully!", type = "message", duration = 3)
      return(data)
      
    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message),
        type = "error",
        duration = NULL
      )
      return(NULL)
    })
  })
  
  # Update date range input when data is loaded
  observe({
    req(raw_data())
    data <- raw_data()
    
    updateDateRangeInput(session, "date_range",
                         start = min(data$Date, na.rm = TRUE),
                         end = max(data$Date, na.rm = TRUE))
  })
  
  # Dynamic impressions slider
  output$impressions_slider <- renderUI({
    if (is.null(input$file)) {
      sliderInput("impressions_max", 
                  "Max Impressions Filter:",
                  min = 0,
                  max = 100000,
                  value = 80000)
    } else {
      req(raw_data())
      
      max_val <- mean(raw_data()$Impressions, na.rm = TRUE) + 
        (2.5 * sd(raw_data()$Impressions, na.rm = TRUE))
      
      sliderInput("impressions_max", 
                  "Max Impressions Filter:",
                  min = 0,
                  max = ceiling(max_val),
                  value = ceiling(max_val * 0.8))
    }
  })
  
  # Dynamic engagements slider
  output$engagements_slider <- renderUI({
    if (is.null(input$file)) {
      sliderInput("engagements_max",
                  "Max Engagements Filter:",
                  min = 0,
                  max = 5000,
                  value = 4000)
    } else {
      req(raw_data())
      
      max_val <- mean(raw_data()$Engagements, na.rm = TRUE) + 
        (2.5 * sd(raw_data()$Engagements, na.rm = TRUE))
      
      sliderInput("engagements_max",
                  "Max Engagements Filter:",
                  min = 0,
                  max = ceiling(max_val),
                  value = ceiling(max_val * 0.8))
    }
  })
  
  # Reactive: Filtered data (SINGLE DEFINITION - FIXED)
  filtered_data <- reactive({
    req(raw_data(), input$impressions_max, input$engagements_max)
    
    data <- raw_data() %>%
      filter(Impressions <= input$impressions_max,
             Engagements <= input$engagements_max,
             Post.Type %in% input$post_type)
    
    # Apply date filter if set
    if (!is.null(input$date_range)) {
      data <- data %>%
        filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    }
    
    # Apply tag filter if not "All Tags"
    if (!is.null(input$tag_types) && input$tag_types != "All Tags") {
      data <- data %>%
        filter(grepl(input$tag_types, Tags, fixed = TRUE))
    }
    
    data
  })
  
  # Reactive: Data with split tags
  data_with_tags <- reactive({
    req(filtered_data())
    
    tryCatch({
      data <- filtered_data()
      
      # Handle cases where Tags column might be all NA or empty
      if (all(is.na(data$Tags) | data$Tags == "")) {
        showNotification("No tags found in data", type = "warning", duration = 3)
        return(data)
      }
      
      data %>%
        mutate(
          tag_list = map(Tags, split_tags),
          max_tags = max(map_int(tag_list, length), na.rm = TRUE)
        ) %>%
        mutate(
          tag_columns = map(tag_list, ~{
            max_t <- first(max_tags)
            if (length(.x) == 0) {
              rep(NA_character_, max_t)
            } else {
              c(.x, rep(NA_character_, max(0, max_t - length(.x))))
            }
          })
        ) %>%
        unnest_wider(tag_columns, names_sep = "_") %>%
        rename_with(~str_replace(.x, "tag_columns_", "Tag_"), 
                    starts_with("tag_columns_")) %>%
        select(-tag_list, -max_tags)
      
    }, error = function(e) {
      showNotification(
        paste("Error processing tags:", e$message),
        type = "warning",
        duration = 5
      )
      return(filtered_data())
    })
  })
  
  # Extract unique tags for dropdown
  available_tags <- reactive({
    req(data_with_tags())
    
    tryCatch({
      tag_cols <- grep("^Tag_", names(data_with_tags()), value = TRUE)
      
      if (length(tag_cols) == 0) {
        return(character(0))
      }
      
      unique_tags <- data_with_tags() %>%
        select(all_of(tag_cols)) %>%
        pivot_longer(everything(), values_to = "tag") %>%
        filter(!is.na(tag), tag != "") %>%
        pull(tag) %>%
        unique() %>%
        sort()
      
      return(unique_tags)
      
    }, error = function(e) {
      return(character(0))
    })
  })
  
  # Update tag dropdown when data changes (but preserve user selection)
  observe({
    req(available_tags())
    
    current_selection <- isolate(input$tag_types)
    new_choices <- c("All Tags", available_tags())
    
    # Only update selection if current selection is no longer valid
    if (is.null(current_selection) || !current_selection %in% new_choices) {
      updateSelectInput(session, "tag_types",
                        choices = new_choices,
                        selected = "All Tags")
    } else {
      updateSelectInput(session, "tag_types",
                        choices = new_choices,
                        selected = current_selection)
    }
  })
  
  # Scatter plot
  output$scatter_plot <- renderPlot({
    req(filtered_data())
    
    color_var <- sym(input$color_by)
    
    p <- ggplot(filtered_data(), 
                aes(Impressions, Engagements, color = !!color_var)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE) +
      theme_minimal() +
      labs(title = "Impressions vs Engagements",
           x = "Impressions",
           y = "Engagements") +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 16, face = "bold"))
    
    ggMarginal(p, type = "density", groupColour = TRUE, groupFill = TRUE)
  })
  
  # R-squared value
  output$r_squared <- renderText({
    req(filtered_data())
    
    if (nrow(filtered_data()) < 3) {
      return("Insufficient data for regression")
    }
    
    model <- lm(Engagements ~ Impressions, data = filtered_data())
    r_sqrd <- signif(summary(model)$r.squared, digits = 3)
    
    paste0("R-squared: ", r_sqrd, "\n",
           "Number of observations: ", nrow(filtered_data()))
  })
  
  # Summary statistics
  output$summary_stats <- renderPrint({
    req(filtered_data())
    
    filtered_data() %>%
      select(Impressions, Reach, Engagements, Likes, Comments, 
             Shares, Saves, Video.Views) %>%
      summary()
  })
  
  # Data table
  output$data_table <- renderDT({
    req(filtered_data())
    
    datatable(filtered_data(), 
              options = list(pageLength = 10, scrollX = TRUE),
              filter = "top")
  })
  
  # Tag analysis plot
  output$tag_plot <- renderPlot({
    req(data_with_tags())
    
    tryCatch({
      tag_cols <- grep("^Tag_", names(data_with_tags()), value = TRUE)
      
      if (length(tag_cols) == 0) {
        plot.new()
        text(0.5, 0.5, "No tags available in filtered data", cex = 1.5)
        return()
      }
      
      tag_counts <- data_with_tags() %>%
        select(all_of(tag_cols)) %>%
        pivot_longer(everything(), values_to = "tag") %>%
        filter(!is.na(tag), tag != "") %>%
        count(tag, sort = TRUE) %>%
        head(15)
      
      if (nrow(tag_counts) == 0) {
        plot.new()
        text(0.5, 0.5, "No tags found in data", cex = 1.5)
        return()
      }
      
      ggplot(tag_counts, aes(x = reorder(tag, n), y = n)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Top 15 Most Common Tags",
             x = "Tag",
             y = "Count") +
        theme(plot.title = element_text(size = 14, face = "bold"))
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating tag plot:", e$message), cex = 1)
    })
  })
  
  # Tag table
  output$tag_table <- renderDT({
    req(data_with_tags())
    
    tryCatch({
      tag_cols <- grep("^Tag_", names(data_with_tags()), value = TRUE)
      
      if (length(tag_cols) == 0) {
        return(datatable(data.frame(Message = "No tags available")))
      }
      
      tag_data <- data_with_tags() %>%
        select(all_of(tag_cols)) %>%
        pivot_longer(everything(), values_to = "tag") %>%
        filter(!is.na(tag), tag != "") %>%
        count(tag, sort = TRUE)
      
      if (nrow(tag_data) == 0) {
        return(datatable(data.frame(Message = "No tags found in data")))
      }
      
      datatable(tag_data, options = list(pageLength = 20))
      
    }, error = function(e) {
      datatable(data.frame(Error = paste("Error processing tags:", e$message)))
    })
  })
  
  # Time series plot
  output$time_series <- renderPlot({
    req(filtered_data())
    
    tryCatch({
      if (nrow(filtered_data()) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available for selected filters", cex = 1.5)
        return()
      }
      
      filtered_data() %>%
        select(Date, Impressions, Engagements, Reach) %>%
        pivot_longer(-Date, names_to = "Metric", values_to = "Value") %>%
        ggplot(aes(Date, Value, color = Metric)) +
        geom_line(linewidth = 1) +
        geom_point() +
        theme_minimal() +
        labs(title = "Metrics Over Time",
             x = "Date",
             y = "Count") +
        theme(legend.position = "bottom",
              plot.title = element_text(size = 14, face = "bold"))
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating time series:", e$message), cex = 1)
    })
  })
  
  # Engagement rate over time
  output$engagement_rate <- renderPlot({
    req(filtered_data())
    
    tryCatch({
      if (nrow(filtered_data()) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available for selected filters", cex = 1.5)
        return()
      }
      
      filtered_data() %>%
        mutate(Engagement_Rate = (Engagements / Impressions) * 100) %>%
        ggplot(aes(Date, Engagement_Rate, color = Post.Type)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        theme_minimal() +
        labs(title = "Engagement Rate Over Time",
             x = "Date",
             y = "Engagement Rate (%)") +
        theme(legend.position = "bottom",
              plot.title = element_text(size = 14, face = "bold"))
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error creating engagement rate plot:", e$message), cex = 1)
    })
  })
  
  # Reset filters
  observeEvent(input$reset, {
    req(raw_data())
    
    data <- raw_data()
    updateDateRangeInput(session, "date_range",
                         start = min(data$Date, na.rm = TRUE),
                         end = max(data$Date, na.rm = TRUE))
################################################################################    
    updateSelectInput(session, "tag_types",
                      selected = "All Tags")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
