require(pacman)
p_load(tidyverse, googlesheets4, shiny, lubridate, bslib, ggbeeswarm, shinycssloaders, shinyWidgets, ggsankey)

# OAuth ----

# The following only needs to be done once
# # Set authentication token to be stored in a folder called `.secrets`
# options(gargle_oauth_cache = ".secrets")
# 
# # Authenticate manually
# gs4_auth()
# 
# # If successful, the previous step stores a token file.
# # Check that a file has been created with:
# list.files(".secrets/")
# 
# # Check that the non-interactive authentication works by first deauthorizing:
# gs4_deauth()

# Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "Your Email")

# Setup ----

# Disable use of scientific notation throughout app
options(scipen=10000)

df <- read_sheet("Your Google Sheet URL") |> 
  drop_na(`Job Title`) |> 
  mutate(normalized_salary_low = case_when(`Pay Unit` == "Hourly" ~ `Pay Lower`*1890,
                                           `Pay Unit` == "Biweekly" ~ `Pay Lower`*27,
                                           `Pay Unit` == "Salary" ~ `Pay Lower`,
                                       NA ~ `Pay Lower`)) |> 
  mutate(normalized_salary_high = case_when(`Pay Unit` == "Hourly" ~ `Pay Upper`*1890,
                                            `Pay Unit` == "Biweekly" ~ `Pay Upper`*27,
                                            `Pay Unit` == "Salary" ~ `Pay Upper`,
                                           NA ~ `Pay Upper`)) |> 
  mutate(average_salary = (normalized_salary_low + normalized_salary_high) / 2)

# Compute minimum and maximum values for initial slider positions 
min_salary <- min(df$normalized_salary_low, na.rm = TRUE)
max_salary <- max(df$normalized_salary_high, na.rm = TRUE)
min_date <- min(as_date(df$`Date Applied`))
max_date <- max(as_date(df$`Date Applied`))



# GLOBAL OPTIONS ----

options(shiny.sanitize.errors = TRUE,
        shiny.trace = FALSE,
        shiny.maxRequestSize = 60*1024^2)


# UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "Cole's Job Applications Dashboard",

  sidebar = sidebar(
    # Width of sidebar
    width = 300,
    
    # Application Result selector
    selectInput(inputId = "application_result", 
                label = "Select Plot Type for Application Result", 
                choices = c("Sankey Plot", "Bar Plot"), 
                selected = "Sankey Plot",
                multiple = FALSE,
                width = "auto"),
    
    # Date range slider
    sliderInput(
      inputId = "date_range",               
      label = "Select Date Range",          
      min = min(min_date), 
      max = max(max_date), 
      value = c(min(min_date), max(max_date)), 
      timeFormat = "%Y-%m-%d"              
    ),
    
    # Filter by salary?
    checkboxInput(
      inputId = "filter_salary",
      label = "Check to filter by salary",
      value = FALSE
    ),
    p("Checking this box will remove jobs with no salary listed."),
    
    # Salary slider if the above box is ticked
    uiOutput("salary_filter"),
    
    # Filter by job type
    selectInput("job_type", 
                label = "Select Job Type", 
                choices = unique(df$`Job Type`), 
                selected = unique(df$`Job Type`),
                multiple = TRUE,
                width = "auto"),
    
    # Filter by work location
    selectInput("location", 
                label = "Select Work Location", 
                choices = unique(df$`Work Location`), 
                selected = unique(df$`Work Location`),
                multiple = TRUE,
                width = "auto"),
  ),
  
  # Box UI - note there is a bug when toggling the salary filter causing a brief error
  # to appear. This is because the reactive elements do not yet have the user selection
  
  uiOutput("boxes"),
  
# Plot cards: added a spinner to prevent errors while loading from occurring 
# Application Result Card
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Application Result"),
      plotOutput("app_result") |> withSpinner() |> (\(x) {
        x[[4]] <- x[[4]] |> bslib::as_fill_carrier() 
        x})()
    ),
    
    # Compensation Card
    card(
      full_screen = TRUE,
      card_header("Compensation"),
      plotOutput("compensation") |> withSpinner() |> (\(x) {
        x[[4]] <- x[[4]] |> bslib::as_fill_carrier() 
        x})()
    )
  ),

# OVer time card
  card(
    full_screen = TRUE,
    card_header("Number of Applications Over Time"),
    plotOutput("date_applied") |> withSpinner() |> (\(x) {
      x[[4]] <- x[[4]] |> bslib::as_fill_carrier() 
      x})()
  )
)



# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  
  # Salary filter UI if filter_salary is selected 
  output$salary_filter <- renderUI({
   
    if(input$filter_salary == TRUE) {
    sliderInput(
      inputId = "salary_range",               
      label = "Select Salary Range",          
      min = min(df$normalized_salary_low, na.rm = TRUE), 
      max = max(df$normalized_salary_high, na.rm = TRUE), 
      value = c(min(df$normalized_salary_low, na.rm = TRUE), max(df$normalized_salary_high, na.rm = TRUE))
      
    
    )
    }
    
  })
  


  ## Total jobs ----

  total_jobs <- reactive({
    df_filtered <- df |> 
      filter(`Job Type` %in% input$job_type) |>
      filter(`Work Location` %in% input$location) |>
      filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2])
    
    # Conditionally apply the salary filter
    if (input$filter_salary == TRUE) {
      df_filtered <- df_filtered |>
        filter(normalized_salary_low >= input$salary_range[1] &
                 normalized_salary_high <= input$salary_range[2])
    }
    
    # Return the number of rows after filtering
    nrow(df_filtered)
  })
  
  

  ## Cover letters ----

  total_cover_letters <- reactive({
    df_filtered <- df |>
      filter(`Cover Letter` == TRUE) |>
      filter(`Job Type` %in% input$job_type) |>
      filter(`Work Location` %in% input$location) |>
      filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2])
    
    # Conditionally apply the salary filter
    if (input$filter_salary == TRUE) {
      df_filtered <- df_filtered |>
        filter(normalized_salary_low >= input$salary_range[1] &
                 normalized_salary_high <= input$salary_range[2])
    }
    
    nrow(df_filtered)
  })
  
## Exams ----
  total_exams <- reactive({
    df_filtered <- df |>
      filter(`Exam` == TRUE) |>
      filter(`Job Type` %in% input$job_type) |>
      filter(`Work Location` %in% input$location) |>
      filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2])
    
    # Conditionally apply the salary filter
    if (input$filter_salary == TRUE) {
      df_filtered <- df_filtered |>
        filter(normalized_salary_low >= input$salary_range[1] &
                 normalized_salary_high <= input$salary_range[2])
    }

    nrow(df_filtered)
  })
  
## Interviews ----
  total_interviews <- reactive({
    df_filtered <- df |>
      filter(`Interview` == TRUE) |>
      filter(`Job Type` %in% input$job_type) |>
      filter(`Work Location` %in% input$location) |>
      filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2])
    
    # Conditionally apply the salary filter
    if (input$filter_salary == TRUE) {
      df_filtered <- df_filtered |>
        filter(normalized_salary_low >= input$salary_range[1] &
                 normalized_salary_high <= input$salary_range[2])
    }

    nrow(df_filtered)
  })
  
## Time Spent ----
  time_spent <- reactive({
    df_filtered <- df |>
      filter(`Job Type` %in% input$job_type) |>
      filter(`Work Location` %in% input$location) |>
      filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2])
    
    # Conditionally apply the salary filter
    if (input$filter_salary == TRUE) {
      df_filtered <- df_filtered |>
        filter(normalized_salary_low >= input$salary_range[1] &
                 normalized_salary_high <= input$salary_range[2]) 
    }
    
    df_filtered |> 
      summarise(total_time = sum(`Time Spent (Hours)`, na.rm = TRUE)) |>
      pull(total_time)
  })
  
  # Boxes ----
  
  # Layout for value boxes
  output$boxes <- renderUI({
    try(
    layout_columns(

      height = "120px",
      fill = FALSE,
      value_box(
        title = NULL,  # Remove the default title
        value = tags$div(
          style = 'display: flex; flex-direction: column; align-items: flex-start;',  # Column layout for title and number
          tags$div(
            style = 'display: flex; align-items: center;',  # Flexbox for icon and title alignment
            bsicons::bs_icon("briefcase", style = "font-size: 28px; margin-right: 8px;"),  # Larger icon with smaller margin
            tags$span("Total Jobs Applied", style = "font-size: 18px; font-weight: bold;")  # Slightly larger title
          ),
          tags$strong(total_jobs(), style = "font-size: 32px; margin-top: 8px;")  # Adjusted size for the number
        ),
        style = 'background-color: #f5f5f5!important; padding: 8px;',  # Adjusted height, keeping width default
        showcase = NULL  # No showcase needed since the icon is handled inside
      

      ),
      value_box(
        title = NULL,
        value = tags$div(
          style = 'display: flex; flex-direction: column; align-items: flex-start; color: white;',  # Set text color to white
          tags$div(
            style = 'display: flex; align-items: center;',
            bsicons::bs_icon("file-earmark-fill", style = "font-size: 28px; margin-right: 8px;"),  # Adjust icon size and margin
            tags$span("Cover Letters Submitted", style = "font-size: 18px; font-weight: bold; color: white;")  # Title styling
          ),
          tags$strong(total_cover_letters(), style = "font-size: 32px; margin-top: 8px; color: white;")  # Number styling
        ),
        style = 'background-color: #4a4a4a!important; padding: 8px;',  # Keep padding
        showcase = NULL  # No showcase needed since the icon is handled inside
      ),
      
      value_box(
        title = NULL,
        value = tags$div(
          style = 'display: flex; flex-direction: column; align-items: flex-start; color: white;',  # Set text color to white
          tags$div(
            style = 'display: flex; align-items: center;',
            bsicons::bs_icon("file-earmark-richtext", style = "font-size: 28px; margin-right: 8px;"),
            tags$span("Exams Written", style = "font-size: 18px; font-weight: bold; color: white;")  # Title styling
          ),
          tags$strong(total_exams(), style = "font-size: 32px; margin-top: 8px; color: white;")  # Number styling
        ),
        style = 'background-color: #56799a!important; padding: 8px;',
        showcase = NULL
      ),
      
      value_box(
        title = NULL,
        value = tags$div(
          style = 'display: flex; flex-direction: column; align-items: flex-start; color: white;',  # Set text color to white
          tags$div(
            style = 'display: flex; align-items: center;',
            bsicons::bs_icon("people-fill", style = "font-size: 28px; margin-right: 8px;"),
            tags$span("Interviews", style = "font-size: 18px; font-weight: bold; color: white;")  # Title styling
          ),
          tags$strong(total_interviews(), style = "font-size: 32px; margin-top: 8px; color: white;")  # Number styling
        ),
        style = 'background-color: #2c3e50!important; padding: 8px;',
        showcase = NULL
      ),
      
      value_box(
        title = NULL,
        value = tags$div(
          style = 'display: flex; flex-direction: column; align-items: flex-start; color: white;',  # Set text color to white
          tags$div(
            style = 'display: flex; align-items: center;',
            bsicons::bs_icon("clock-history", style = "font-size: 28px; margin-right: 8px;"),
            tags$span("Time Spent", style = "font-size: 18px; font-weight: bold; color: white;")  # Title styling
          ),
          tags$strong(paste0(time_spent(), " hours"), style = "font-size: 32px; margin-top: 8px; color: white;")  # Number styling
        ),
        style = 'background-color: #c25b56!important; padding: 8px;',
        showcase = NULL
      )
    ),
    silent = TRUE
    )
  })

  
  # Plots ----

  # Application result plots; either sankey or bar based on user input
  output$app_result <- renderPlot({
    
    if(input$application_result == "Bar Plot"){
      
      # Application result bar plot
    df_filtered <- df |>
      filter(`Job Type` %in% input$job_type) |>
      filter(`Work Location` %in% input$location) |>
      filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) 
        
        # Conditionally apply the salary filter
        if (input$filter_salary == TRUE) {

          df_filtered <- df_filtered |>
            filter(normalized_salary_low >= input$salary_range[1] &
                     normalized_salary_high <= input$salary_range[2])
        }
    
    df_filtered |> 
     
    ggplot() +
      geom_bar(aes(x = `Sector`, fill = `Result`, colour = `Result`), alpha = 0.5) +
      theme_bw(base_size = 16) +
      scale_fill_viridis_d(end = 0.9) +
      scale_colour_viridis_d(end = 0.9) +
      scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::label_number(accuracy = 1)) +
      theme(axis.text.x = element_text(size = 10)) +
      labs(x = "Job Sector",
           y = "Number of Applications")|> 
      suppressWarnings()
      
    } else if(input$application_result == "Sankey Plot") {

      # Application result sankey plot
      df_filtered <- df |>
        filter(`Job Type` %in% input$job_type) |>
        filter(`Work Location` %in% input$location) |>
        filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) 
      

      # Conditionally apply the salary filter
      if (input$filter_salary == TRUE) {

        
        df_filtered <- df_filtered |>
          filter(normalized_salary_low >= input$salary_range[1] &
                   normalized_salary_high <= input$salary_range[2])
      }
      
      df_sankey <- df_filtered |> 
      make_long(Sector, Exam, Interview, Result)

    

    
    ggplot(df_sankey, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
      geom_sankey(flow.alpha = 0.5, node.color = "gray30") +  # Sankey plot
      geom_sankey_label(size = 3, color = "black", fill = "white") +  # Node labels
      theme_sankey(base_size = 16) +  # Customize theme
      labs(x = "Competition Stage") +
      scale_fill_viridis_d(end = 0.9, option = "inferno") +
      theme(legend.position = "none", 
            axis.title.y = element_text("none")) |> 
      suppressWarnings()
    

    
    }
    
    
  })
  

    
    output$compensation <- renderPlot({
      
      
      num_na <- sum(is.na(df$average_salary))

      df_filtered <- df |>
        filter(`Job Type` %in% input$job_type) |>
        filter(`Work Location` %in% input$location) |>
        filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) 
      
      # Conditionally apply the salary filter
      if (input$filter_salary == TRUE) {
        df_filtered <- df_filtered |>
          filter(normalized_salary_low >= input$salary_range[1] &
                   normalized_salary_high <= input$salary_range[2])
        num_na <- sum(is.na(df_filtered$average_salary))
      }
      
      df_filtered |> 
      ggplot(aes(x = Sector, y = average_salary, fill = `Sector`, colour = `Sector`)) +
        geom_violin(trim = FALSE, drop = FALSE, alpha = 0.6) +
        geom_beeswarm(size = 5, alpha = 0.8, pch=21) +
        annotate("text", x = Inf, y = Inf, label = paste("NA count:", num_na), 
                 hjust = 1.1, vjust = 2, size = 5, color = "red", fontface = "bold") +
        theme_bw(base_size = 16) +
        theme(axis.text.x = element_text(size = 10)) + 
        scale_fill_viridis_d(end = 0.9) +
        scale_colour_viridis_d(end = 0.9) +
        scale_y_continuous(labels = scales::comma) +
        
        labs(x = "Job Sector",
             y = "Mean Salary (CAD)") |> 
        suppressWarnings()
      
     
    
  })
    
    output$date_applied <- renderPlot({
      # Create a new column for week-year
      df_filtered <- df |>
        filter(`Job Type` %in% input$job_type) |>
        filter(`Work Location` %in% input$location) |>
        filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) 
      
      # Conditionally apply the salary filter
      if (input$filter_salary == TRUE) {
        df_filtered <- df_filtered |>
          filter(normalized_salary_low >= input$salary_range[1] &
                   normalized_salary_high <= input$salary_range[2])
      }
      
      application_counts <- df_filtered |> 
        
        # Add week_year and count the number of jobs
        mutate(week_year = paste0(format(`Date Applied`, "%Y"), "-W", 
                                  format(`Date Applied`, "%U"), " (", 
                                  format(`Date Applied`, "%b"), ")")) |> 
        mutate(number_of_jobs = 1) |> 
        
        # Summarise by week_year
        summarise(number_of_jobs = sum(number_of_jobs),
                  .by = week_year)
      
  
      
      application_counts |> 
        ggplot(aes(x = week_year, y = number_of_jobs, size = number_of_jobs)) + 
        geom_smooth(aes(group = 1), method = "loess", se = FALSE, color = "cadetblue", size = 2) +  
        geom_point(alpha = 0.7, stroke = 1, shape = 21, fill = "black", colour = "black") + 
        theme_bw(base_size = 16) +
        labs(x = "Date Applied (Year-Week)", y = "No. of Apps") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +  
        expand_limits(y = max(application_counts$number_of_jobs) + 0.5) +
        scale_y_continuous(breaks = seq(0, max(application_counts$number_of_jobs, na.rm = TRUE), by = 2)) +  
        scale_size_continuous("Number of \nApplications", breaks = seq(1, max(application_counts$number_of_jobs, na.rm = TRUE), by = 2)) |> 
        suppressWarnings()

      
    })
    
  
}


# Shiny App ---------------------------------------------------------------

shinyApp(ui, server)
