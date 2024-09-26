require(pacman)
p_load(tidyverse, googlesheets4, shiny, lubridate, bslib, ggbeeswarm)

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

min_date <- min(as_date(df$`Date Applied`))

max_date <- max(as_date(df$`Date Applied`))

application_counts <- df %>%
  group_by(`Date Applied`, Type) %>%
  summarise(number_of_jobs = n(), .groups = "drop")

num_na <- sum(is.na(df$average_salary))

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



# GLOBAL OPTIONS ----

options(shiny.sanitize.errors = TRUE,
        shiny.trace = FALSE,
        shiny.maxRequestSize = 60*1024^2)


# UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "Job Applications Dashboard",

  sidebar = sidebar(
    sliderInput(
      inputId = "date_range",               # Input ID for the date slider
      label = "Select Date Range",          # Label for the date slider
      min = min(min_date), # Min date (assumed from penguins dataset)
      max = max(max_date), # Max date (assumed from penguins dataset)
      value = c(min(min_date), max(max_date)), # Default to min and max
      timeFormat = "%Y-%m-%d"               # Format the dates displayed
    )
  ),
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Total Jobs Applied",
      value = total_jobs,
      showcase = bsicons::bs_icon("briefcase")
    ),
    value_box(
      title = "Cover Letters Drafted",
      value = total_cover_letters,
      showcase = bsicons::bs_icon("file-earmark-fill"),
      theme_color = "dark"
    ),
    
    value_box(
      title = "Exams Written",
      value = total_exams,
      showcase = bsicons::bs_icon("file-earmark-richtext"),
      theme_color = "primary"
    ),
    
    value_box(
      title = "Interviews",
      value = total_interviews,
      showcase = bsicons::bs_icon("people-fill"),
      theme_color = "secondary"
    )
  ),
  
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

  
  output$app_result <- renderPlot({
    df |> 
      filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) |> 
    ggplot() +
      geom_bar(aes(x = `Type`, fill = `Result`), alpha = 0.5) +
      theme_bw(base_size = 16) +
      scale_fill_viridis_d(end = 0.9) +
      theme(axis.text.x = element_text(size = 10)) + 
      labs(x = "Job Sector",
           y = "Number of Applications")
  })
    
    output$compensation <- renderPlot({
      df |> 
        filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) |> 
      ggplot(aes(x = Type, y = average_salary, fill = Type, colour = Type)) +
        geom_violin(trim = FALSE, drop = FALSE, alpha = 0.6) +
        geom_beeswarm(width = 0.2, height = 0, size = 5, alpha = 0.8, pch=21) +
        annotate("text", x = Inf, y = Inf, label = paste("NA count:", num_na), 
                 hjust = 1.1, vjust = 2, size = 5, color = "red", fontface = "bold") +
        theme_bw(base_size = 16) +
        theme(axis.text.x = element_text(size = 10)) + 
        scale_fill_viridis_d(end = 0.9) +
        scale_colour_viridis_d(end = 0.9) +
        scale_y_continuous(labels = scales::comma) +
        
        labs(x = "Job Sector",
             y = "Mean Compensation (CAD)")
    
  })
    
    output$date_applied <- renderPlot({
      # Create a new column for week-year
      application_counts <- application_counts %>%
        filter(`Date Applied` >= input$date_range[1] & `Date Applied` <= input$date_range[2]) |> 
        mutate(week_year = paste0(format(`Date Applied`, "%Y"), "-W", 
                                  format(`Date Applied`, "%U"), " (", 
                                  format(`Date Applied`, "%b"), ")"))
      
      application_counts |> 
        ggplot(aes(x = week_year, y = number_of_jobs, size = number_of_jobs)) + 
        geom_smooth(aes(group = 1), method = "loess", se = FALSE, color = "cadetblue", size = 2) +  
        geom_point(alpha = 0.7, stroke = 1, shape = 21, fill = "black", colour = "black") + 
        theme_bw(base_size = 16) +
        labs(x = "Date Applied (Year-Week)", y = "Number of Applications") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +  
        expand_limits(y = max(application_counts$number_of_jobs) + 0.5) +
        scale_y_continuous(breaks = seq(0, max(application_counts$number_of_jobs, na.rm = TRUE), by = 1)) +  
        scale_size_continuous("Number of \nApplications", breaks = seq(1, max(application_counts$number_of_jobs, na.rm = TRUE), by = 1))  

      
    })
    
  
}


# Shiny App ---------------------------------------------------------------

shinyApp(ui, server)
