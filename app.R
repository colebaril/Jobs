require(pacman)
p_load(tidyverse, googlesheets4, shiny, lubridate, bslib, ggbeeswarm, shinycssloaders, shinyWidgets, ggsankey, ggiraph)

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
gs4_auth(cache = ".secrets", email = "your_email")

# Login ----



# Setup ----

# Disable use of scientific notation throughout app
options(scipen=10000)

df <- read_sheet("google_sheet_url") |> 
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
    open = TRUE,
    # Width of sidebar
    width = 250,
    
    
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
    

    p(strong("Personal Links:"),
      br(),
      tags$a(href="https://github.com/colebaril/Jobs", "GitHub Repository"),
      br(),
      tags$a(href="https://github.com/colebaril", "GitHub Page"),
      br(),
      tags$a(href="https://colebaril.ca/", "Personal Website"),
      br(),
      tags$a(href="https://colebaril.ca/files/Cole%20Baril%20CV%20Sept%202024.pdf", "My Resume"),
      br(),

    # p(strong("Looking for a star employee?"),
    tags$strong(tags$a(href="mailto:colebarilca@gmail.com", "Email me!"))
    ),  
  ),
  
  # Box UI - note there is a bug when toggling the salary filter causing a brief error
  # to appear. This is because the reactive elements do not yet have the user selection
  
  uiOutput("boxes"),
  
  # Add JavaScript to capture window size and DPI with debouncing
  tags$body(tags$div(id="ppitest", style="width:1in;visible:hidden;padding:0px")),
  tags$script('
  var debounceResize;

  $(document).on("shiny:connected", function(e) {
    var w = window.innerWidth;
    var h = window.innerHeight;
    var d = document.getElementById("ppitest").offsetWidth;
    var obj = {width: w, height: h, dpi: d};
    Shiny.onInputChange("pltChange", obj);
  });

  $(window).resize(function(e) {
    clearTimeout(debounceResize);
    debounceResize = setTimeout(function() {
      var w = $(window).width();
      var h = $(window).height();
      var d = document.getElementById("ppitest").offsetWidth;
      var obj = {width: w, height: h, dpi: d};
      Shiny.onInputChange("pltChange", obj);
    }, 250);  // Debounce time set to 250ms (can be adjusted)
  });
'),
  
# Plot cards: added a spinner to prevent errors while loading from occurring 
# Application Result Card
layout_column_wrap(
  width = 0.5,
  
  card(
    height = "50%", full_screen = TRUE,
    card_header("Application Result - Sankey Plot"),
    plotOutput("sankey_plot", height = "100%") |> withSpinner() |> (\(x) {
      x[[4]] <- x[[4]] |> bslib::as_fill_carrier() 
      x})()
  ),
  
  card(
    height = "50%", full_screen = TRUE,
    card_header("Compensation"),
    girafeOutput("compensation", width = "100%", height = "100%") |> withSpinner()
  ),
  
  card(
    height = "50%", full_screen = TRUE,
    card_header("Application Result - Bar Plot"),
    girafeOutput("interactive_bar_plot", width = "100%", height = "100%") |> withSpinner()
  ),
  
  card(
    height = "50%", full_screen = TRUE,
    card_header("Number of Applications Over Time"),
    plotOutput("date_applied", height = "100%") |> withSpinner() |> (\(x) {
      x[[4]] <- x[[4]] |> bslib::as_fill_carrier() 
      x})()
  )
)
)



# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  showNotification("Please Note: This app was not designed for mobile. Please view on a desktop computer.",
                   type = "error")
  
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
        height = "12vh",  # Consistent box height
        fill = TRUE,  # Fill based on column space
        value_box(
          title = NULL,  # Remove the default title
          value = tags$div(
            style = 'display: flex; flex-direction: column; align-items: flex-start;',  # Column layout for title and number
            tags$div(
              style = 'display: flex; align-items: center;',  # Flexbox for icon and title alignment
              bsicons::bs_icon("briefcase", style = "font-size: 4.5vw; margin-right: 0.5vw;"),  # Large icon with reduced margin
              tags$span("Total Jobs Applied", style = "font-size: 1vw; font-weight: bold;")  # Small title
            ),
            tags$strong(total_jobs(), style = "font-size: 2vw; margin-top: 0.2vw;")  # Adjust number size and margin
          ),
          style = 'background-color: #f5f5f5!important; padding: 0.2vw;',  # Reduced padding
          showcase = NULL  # No showcase needed since the icon is handled inside
        ),
        
        value_box(
          title = NULL,
          value = tags$div(
            style = 'display: flex; flex-direction: column; align-items: flex-start; color: white;',  # Set text color to white
            tags$div(
              style = 'display: flex; align-items: center;',
              bsicons::bs_icon("file-earmark-fill", style = "font-size: 4.5vw; margin-right: 0.5vw;"),  # Adjust icon size and margin
              tags$span("Cover Letters Submitted", style = "font-size: 1vw; font-weight: bold; color: white;")  # Title styling
            ),
            tags$strong(total_cover_letters(), style = "font-size: 2vw; margin-top: 0.2vw; color: white;")  # Number styling
          ),
          style = 'background-color: #4a4a4a!important; padding: 0.2vw;',  # Consistent padding
          showcase = NULL
        ),
        
        value_box(
          title = NULL,
          value = tags$div(
            style = 'display: flex; flex-direction: column; align-items: flex-start; color: white;',  # Set text color to white
            tags$div(
              style = 'display: flex; align-items: center;',
              bsicons::bs_icon("file-earmark-richtext", style = "font-size: 4.5vw; margin-right: 0.5vw;"),  # Consistent icon size
              tags$span("Exams Written", style = "font-size: 1vw; font-weight: bold; color: white;")  # Title styling
            ),
            tags$strong(total_exams(), style = "font-size: 2vw; margin-top: 0.2vw; color: white;")  # Number styling
          ),
          style = 'background-color: #56799a!important; padding: 0.2vw;',  # Consistent padding
          showcase = NULL
        ),
        
        value_box(
          title = NULL,
          value = tags$div(
            style = 'display: flex; flex-direction: column; align-items: flex-start; color: white;',  # Set text color to white
            tags$div(
              style = 'display: flex; align-items: center;',
              bsicons::bs_icon("people-fill", style = "font-size: 4.5vw; margin-right: 0.5vw;"),  # Consistent icon size
              tags$span("Interviews", style = "font-size: 1vw; font-weight: bold; color: white;")  # Title styling
            ),
            tags$strong(total_interviews(), style = "font-size: 2vw; margin-top: 0.2vw; color: white;")  # Number styling
          ),
          style = 'background-color: #2c3e50!important; padding: 0.2vw;',  # Consistent padding
          showcase = NULL
        ),
        
        value_box(
          title = NULL,
          value = tags$div(
            style = 'display: flex; flex-direction: column; align-items: flex-start; color: white;',  # Set text color to white
            tags$div(
              style = 'display: flex; align-items: center; padding-top: 0.5vw;',  # Added padding-top to create space above the icon
              bsicons::bs_icon("clock-history", style = "font-size: 4.5vw; margin-right: 0.5vw;"),  # Consistent icon size with margin
              tags$span("Time Spent", style = "font-size: 1vw; font-weight: bold; color: white;")  # Title styling
            ),
            HTML(
              paste0("<strong style='font-size: 2vw;'>", time_spent(), "<span style='font-size: 1.5vw;'> hours</span></strong>")
            )
          ),
          style = 'background-color: #c25b56!important; padding: 0.2vw;',  # Consistent padding for the box
          showcase = NULL
        )
      ),
      silent = TRUE
    )
  })
  

  
  # Plots ----

  # Server-side: Render either ggplot or ggiraph dynamically

  # Bar plot ----
  
  output$interactive_bar_plot <- renderGirafe({
    req(input$pltChange)  # Wait until JavaScript has passed the window size
    
    # Filter data based on user input
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
    
    # Dynamically adjust text size based on plot width
    base_text_size <- max(25, (0.06 * input$pltChange$width / input$pltChange$dpi))
    
    # Create the ggplot object
    p <- df_filtered |> 
      ggplot(aes(x = Sector, fill = `Result`, colour = `Result`)) +
      geom_bar_interactive(aes(tooltip = paste0(# "Job Title: ", `Job Title`, "\n",
                                                "Job Type: ", `Job Type`, "\n",
                                                # "Company: ", `Company`, "\n",
                                                "Lists Salary: ", `Pay`, "\n",
                                                "Days Since Applied: ", `Days Since Applied`, "\n",
                                                "Hours Spent on Process:  ", `Time Spent (Hours)`, "\n",
                                                "Status: ", `Result`)),
                           alpha = 0.5, position = "stack") +
      theme_bw(base_size = base_text_size) +  # Dynamically adjust text size
      theme(
        axis.text.x = element_text(size = base_text_size * 0.8),
        axis.text.y = element_text(size = base_text_size * 0.8)
      ) +
      scale_fill_viridis_d(end = 0.9) +
      scale_colour_viridis_d(end = 0.9) +
      scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::label_number(accuracy = 1)) +
      labs(x = "Job Sector", y = "Number of Applications")
    
    # Set reactive plot size based on window dimensions
    width_svg <- (0.8 * input$pltChange$width / input$pltChange$dpi)
    height_svg <- (0.5 * input$pltChange$height / input$pltChange$dpi)
    
    # Render the plot with girafe with dynamic sizing and hover options
    girafe(ggobj = p,
           width_svg = width_svg,   # Dynamically adjust width
           height_svg = height_svg,  # Dynamically adjust height
           options = list(
             opts_hover(css = "fill:red;stroke:gray;stroke-width:2px;"),
             opts_tooltip(css = "background-color:rgba(50, 50, 50, 0.7); color:white; border-radius:5px; padding:8px;"),
             opts_sizing(rescale = TRUE, width = 1),  # Enable responsive resizing
             opts_toolbar(saveaspng = TRUE)
           ))
  })
  
  
  # Sankey plot ----
  output$sankey_plot <- renderPlot({
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
    
    df_sankey <- df_filtered |> make_long(Sector, Exam, Interview, Result)
    
    ggplot(df_sankey, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
      geom_sankey(flow.alpha = 0.5, node.color = "gray30") +
      geom_sankey_label(size = 3, color = "black", fill = "white") +
      theme_sankey(base_size = 16) +
      labs(x = "Competition Stage") +
      scale_fill_viridis_d(end = 0.9, option = "inferno") +
      theme(legend.position = "none", axis.title.y = element_text("none"))
  })
  
  
    ### Compensation Plot ----
    
  output$compensation <- renderGirafe({
    req(input$pltChange)  # Wait until JavaScript has passed the window size
    
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
    
    # Calculate dynamic plot size based on window size and DPI
    width_svg <- (0.8 * input$pltChange$width / input$pltChange$dpi)
    height_svg <- (0.5 * input$pltChange$height / input$pltChange$dpi)
    
    # Calculate a base size for text that scales with plot dimensions
    base_text_size <- max(25, (0.06 * input$pltChange$width / input$pltChange$dpi))
    
    # Create the compensation plot
    p <- df_filtered |>
      ggplot(aes(x = Sector, y = average_salary, fill = `Sector`)) +
      
      # geom_violin for salary distribution
      geom_violin(trim = FALSE, alpha = 0.6) +
      
      # Interactive points for beeswarm layout
      geom_point_interactive(aes(
        tooltip = paste0(
                         # "Job Title: ", `Job Title`, "\n",
                         "Job Type: ", `Job Type`, "\n",
                         # "Company: ", `Company`, "\n",
                         "Min Salary: ", "$", scales::comma(normalized_salary_low), " CAD", "\n",
                         "Max Salary: ", "$", scales::comma(normalized_salary_high), " CAD", "\n",
                         "Days Since Applied: ", `Days Since Applied`, "\n",
                         "Hours Spent on Process:  ", `Time Spent (Hours)`, "\n",
                         "Status: ", `Result`
                         )),
        size = 5, alpha = 0.8, pch = 21, colour = "black") +
      
      # Annotate NA count
      annotate("text", x = Inf, y = Inf, label = paste("NA count:", num_na),
               hjust = 1.1, vjust = 2, size = base_text_size * 0.4, color = "red", fontface = "bold") +
      
      # Styling
      theme_bw(base_size = base_text_size) +  # Use dynamic text size

      # Remove extra margins
      theme(
        axis.text.x = element_text(size = base_text_size * 0.8),
        axis.text.y = element_text(size = base_text_size * 0.8),
        plot.margin = unit(c(0, 0, 0, 0.5), "cm")  # No margin to avoid white space
      ) +
      scale_fill_viridis_d(end = 0.9) +
      scale_colour_viridis_d(end = 0.9) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Job Sector", y = "Mean Salary (CAD)")
    
    # Render the plot interactively using girafe
    girafe(ggobj = p,
           width_svg = width_svg,  # Dynamically adjust width
           height_svg = height_svg,  # Dynamically adjust height
           options = list(
             opts_hover(css = "fill:red;stroke:gray;stroke-width:2px;"),
             opts_tooltip(css = "background-color:#525252; color:white; border-radius:5px; padding:8px;"),  # Dark grey background with transparency
             opts_sizing(rescale = TRUE, width = 1)
           ))
  })
  
  # Date plot ----

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
