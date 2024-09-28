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
gs4_auth(cache = ".secrets", email = "colewilliambaril95@gmail.com")

# Setup ----

options(scipen=10000)

df <- read_sheet("https://docs.google.com/spreadsheets/d/1mxJ46ZMN1EMKeJje2Ol0_XCcp6lf4PmEK5S8KZjR_mU/edit?gid=0#gid=0") |> 
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



min_salary <- min(df$normalized_salary_low, na.rm = TRUE)

max_salary <- max(df$normalized_salary_high, na.rm = TRUE)

min_date <- min(as_date(df$`Date Applied`))

max_date <- max(as_date(df$`Date Applied`))

# application_counts <- df %>%
#   group_by(`Date Applied`, Sector, `Work Location`, `Job Type`) %>%
#   summarise(number_of_jobs = n(), .groups = "drop")



total_jobs <- nrow(df)

total_cover_letters <- df |> 
  filter(`Cover Letter` == TRUE) |> 
  nrow()

total_exams <- df |> 
  filter(`Exam` == TRUE) |> 
  nrow()

total_interviews <- df |> 
  filter(`Interview` == TRUE) |> 
  nrow()

time_spent <- sum(df$`Time Spent (Hours)`)



# GLOBAL OPTIONS ----

options(shiny.sanitize.errors = TRUE,
        shiny.trace = FALSE,
        shiny.maxRequestSize = 60*1024^2)


# UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "Cole's Job Applications Dashboard",
  


  sidebar = sidebar(
    width = 300,
    
    selectInput(inputId = "application_result", 
                label = "Select Plot Type for Application Result", 
                choices = c("Sankey Plot", "Bar Plot"), 
                selected = "Sankey Plot",
                multiple = FALSE,
                width = "auto"),
    
    sliderInput(
      inputId = "date_range",               
      label = "Select Date Range",          
      min = min(min_date), 
      max = max(max_date), 
      value = c(min(min_date), max(max_date)), 
      timeFormat = "%Y-%m-%d"              
    ),
    
    sliderInput(
      inputId = "salary",               
      label = "Select Salary Range",          
      min = min(min_salary), 
      max = max(max_salary), 
      value = c(min(min_salary), max(max_salary))
    ),
    helpText("Slide to adjust salary range. NAs will be included if no selection is made."),
    
    selectInput("job_type", 
                label = "Select Job Type", 
                choices = unique(df$`Job Type`), 
                selected = unique(df$`Job Type`),
                multiple = TRUE,
                width = "auto"),
    
    selectInput("location", 
                label = "Select Work Location", 
                choices = unique(df$`Work Location`), 
                selected = unique(df$`Work Location`),
                multiple = TRUE,
                width = "auto"),
    
    # pickerInput("job_type", 
    #             label = "Select Job Type", 
    #             choices = unique(df$`Job Type`), 
    #             selected = unique(df$`Job Type`),
    #             multiple = TRUE,
    #             width = "auto",
    #             options = list(`actions-box` = TRUE)),
    # p(strong("Personal Links:"),
    #   br(),
    #   tags$a(href="https://github.com/colebaril/Jobs", "GitHub Repository"),
    #   br(),
    #   tags$a(href="https://github.com/colebaril", "GitHub Page"),
    #   br(),
    #   tags$a(href="https://colebaril.ca/", "Personal Website"),
    #   br(),
    #   tags$a(href="https://colebaril.ca/files/Cole%20Baril%20CV%20Sept%202024.pdf", "My Resume"),
    # ),
    # p(strong("Looking for a star employee?"),
    # tags$strong(tags$a(href="mailto:colebarilca@gmail.com", "Email me!"))
    # ),  # Replace with your email address
  ),
  uiOutput("boxes"),
  
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Application Result"),
      plotOutput("app_result")
    ),
    card(
      full_screen = TRUE,
      card_header("Compensation"),
      plotOutput("compensation")
    )
  ),
  card(
    full_screen = TRUE,
    card_header("Number of Applications Over Time"),
    plotOutput("date_applied")
  )
)



# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # Reactive calculation of total jobs
  total_jobs <- reactive({
    df |>
      filter(`Job Type` %in% input$job_type) |>
      filter(`Work Location` %in% input$location) |>
      filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) |>
      filter(
        if (input$salary[1] == min_salary & input$salary[2] == max_salary) {
          TRUE  # Keep all rows, including NAs, when slider is at default
        } else {
          normalized_salary_low >= input$salary[1] & 
            normalized_salary_high <= input$salary[2] & 
            !is.na(normalized_salary_low) & 
            !is.na(normalized_salary_high)  # Exclude NAs if slider is moved
        }
      ) |>
      nrow()
  })
  
  # Reactive calculation of total cover letters
  total_cover_letters <- reactive({
    df |>
      filter(`Cover Letter` == TRUE) |>
      filter(`Job Type` %in% input$job_type) |>
      filter(`Work Location` %in% input$location) |>
      filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) |>
      filter(
        if (input$salary[1] == min_salary & input$salary[2] == max_salary) {
          TRUE
        } else {
          normalized_salary_low >= input$salary[1] & 
            normalized_salary_high <= input$salary[2] & 
            !is.na(normalized_salary_low) & 
            !is.na(normalized_salary_high)
        }
      ) |>
      nrow()
  })
  
  # Reactive calculation of total exams
  total_exams <- reactive({
    df |>
      filter(`Exam` == TRUE) |>
      filter(`Job Type` %in% input$job_type) |>
      filter(`Work Location` %in% input$location) |>
      filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) |>
      filter(
        if (input$salary[1] == min_salary & input$salary[2] == max_salary) {
          TRUE
        } else {
          normalized_salary_low >= input$salary[1] & 
            normalized_salary_high <= input$salary[2] & 
            !is.na(normalized_salary_low) & 
            !is.na(normalized_salary_high)
        }
      ) |>
      nrow()
  })
  
  # Reactive calculation of total interviews
  total_interviews <- reactive({
    df |>
      filter(`Interview` == TRUE) |>
      filter(`Job Type` %in% input$job_type) |>
      filter(`Work Location` %in% input$location) |>
      filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) |>
      filter(
        if (input$salary[1] == min_salary & input$salary[2] == max_salary) {
          TRUE
        } else {
          normalized_salary_low >= input$salary[1] & 
            normalized_salary_high <= input$salary[2] & 
            !is.na(normalized_salary_low) & 
            !is.na(normalized_salary_high)
        }
      ) |>
      nrow()
  })
  
  # Reactive calculation of time spent
  time_spent <- reactive({
    df |>
      filter(`Job Type` %in% input$job_type) |>
      filter(`Work Location` %in% input$location) |>
      filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) |>
      filter(
        if (input$salary[1] == min_salary & input$salary[2] == max_salary) {
          TRUE
        } else {
          normalized_salary_low >= input$salary[1] & 
            normalized_salary_high <= input$salary[2] & 
            !is.na(normalized_salary_low) & 
            !is.na(normalized_salary_high)
        }
      ) |>
      summarise(total_time = sum(`Time Spent (Hours)`, na.rm = TRUE)) |>
      pull(total_time)
  })
  
  # Boxes ----
  
  # Layout for value boxes
  output$boxes <- renderUI({
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
        style = 'background-color: #f5f5f5!important; padding: 8px; height: 120px;',  # Adjusted height, keeping width default
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
    )
  })

  
  # Plots ----

  
  output$app_result <- renderPlot({
    
    if(input$application_result == "Bar Plot"){
    
    df |>
      filter(`Job Type` %in% input$job_type) |>
      filter(`Work Location` %in% input$location) |>
      filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) |>
      # Conditional salary filter
      filter(
        if (input$salary[1] == min_salary & input$salary[2] == max_salary) {
          TRUE  # Keep all rows including NAs when slider is at default
        } else {
          normalized_salary_low >= input$salary[1] &
            normalized_salary_high <= input$salary[2] &
            !is.na(normalized_salary_low) &
            !is.na(normalized_salary_high)  # Exclude NAs if slider is moved
        }
      ) |>
    ggplot() +
      geom_bar(aes(x = `Sector`, fill = `Result`, colour = `Result`), alpha = 0.5) +
      theme_bw(base_size = 16) +
      scale_fill_viridis_d(end = 0.9) +
      scale_colour_viridis_d(end = 0.9) +
      scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::label_number(accuracy = 1)) +
      theme(axis.text.x = element_text(size = 10)) +
      labs(x = "Job Sector",
           y = "Number of Applications")
      
    } else if(input$application_result == "Sankey Plot") {
    
    
    df_sankey <- df |> 
      filter(`Job Type` %in% input$job_type) |>
      filter(`Work Location` %in% input$location) |>
      filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) |> 
      # Conditional salary filter
      filter(
        if (input$salary[1] == min_salary & input$salary[2] == max_salary) {
          TRUE  # Keep all rows including NAs when slider is at default
        } else {
          normalized_salary_low >= input$salary[1] & 
            normalized_salary_high <= input$salary[2] & 
            !is.na(normalized_salary_low) & 
            !is.na(normalized_salary_high)  # Exclude NAs if slider is moved
        }
      ) |>
      make_long(Sector, Exam, Interview, Result)

    
    # png("sankey_plot.png", width = 800, height = 600)
    
    ggplot(df_sankey, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
      geom_sankey(flow.alpha = 0.5, node.color = "gray30") +  # Sankey plot
      geom_sankey_label(size = 3, color = "black", fill = "white") +  # Node labels
      theme_sankey(base_size = 16) +  # Customize theme
      labs(x = "Competition Stage") +
      scale_fill_viridis_d(end = 0.9, option = "inferno") +
      theme(legend.position = "none", 
            axis.title.y = element_text("none"))
    
    # dev.off()  # Close the file device after plotting
    
    }
    
    
  })
    
    output$compensation <- renderPlot({
      
      num_na_pre <-  df |> 
        filter(`Job Type` %in% input$job_type) |>
        filter(`Work Location` %in% input$location) |>
        filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) |>
        # Conditional salary filter
        filter(
          if (input$salary[1] == min_salary & input$salary[2] == max_salary) {
            TRUE  # Keep all rows including NAs when slider is at default
          } else {
            normalized_salary_low >= input$salary[1] & 
              normalized_salary_high <= input$salary[2] &  
              !is.na(normalized_salary_low) & 
              !is.na(normalized_salary_high)  # Exclude NAs if slider is moved
          }
        )
      
      
      num_na <- sum(is.na(num_na_pre$average_salary))

      df |> 
        filter(`Job Type` %in% input$job_type) |>
        filter(`Work Location` %in% input$location) |>
        filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) |>
        # Conditional salary filter
        filter(
          if (input$salary[1] == min_salary & input$salary[2] == max_salary) {
            TRUE  # Keep all rows including NAs when slider is at default
          } else {
            normalized_salary_low >= input$salary[1] & 
              normalized_salary_high <= input$salary[2] &  
              !is.na(normalized_salary_low) & 
              !is.na(normalized_salary_high)  # Exclude NAs if slider is moved
          }
        ) |>
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
             y = "Mean Salary (CAD)")
      
     
    
  })
    
    output$date_applied <- renderPlot({
      # Create a new column for week-year
      application_counts <- df %>%
        # Filter by Date Applied range
        filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) |>
        
        # Conditional salary filter
        filter(
          if (input$salary[1] == min_salary & input$salary[2] == max_salary) {
            TRUE  # Keep all rows including NAs when slider is at default
          } else {
            normalized_salary_low >= input$salary[1] & 
              normalized_salary_high <= input$salary[2] & 
              !is.na(normalized_salary_low) & 
              !is.na(normalized_salary_high)  # Exclude NAs if slider is moved
          }
        ) |>
        
        # Filter by Job Type and Location
        filter(`Job Type` %in% input$job_type) |>
        filter(`Work Location` %in% input$location) |>
        
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
        scale_size_continuous("Number of \nApplications", breaks = seq(1, max(application_counts$number_of_jobs, na.rm = TRUE), by = 2))  

      
    })
    
  
}


# Shiny App ---------------------------------------------------------------

shinyApp(ui, server)
