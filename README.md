# Job Application Tracker

This Shiny app allows you to track your progress as you apply for jobs. It pulls data directly from Google Sheets using the `googlesheets4` package, processes the data, and presents key metrics and visualizations of your job application journey. 

The app is available [here](https://colewb.shinyapps.io/Job_Applications/). 

## Features

- **Data Import**: The app reads job application data from a Google Sheet. It uses the `googlesheets4` package to access the data, making it easy to update and manage the dataset from Google Sheets.
  
- **Dynamic Date Range Selection**: Users can filter the data by selecting a date range using a slider to analyze job application patterns over time.

- **Key Metrics**: The app displays important statistics such as:
  - Total jobs applied for
  - Number of cover letters written
  - Number of exams completed
  - Number of interviews attended

- **Data Visualizations**:
  - **Job Application Results**: Visualize the distribution of job applications across different sectors and their results.
  - **Compensation Insights**: Compare compensation across different job sectors using violin and beeswarm plots.
  - **Applications Over Time**: Track the number of job applications submitted over time with a smooth trend line.

## Authentication

The app uses OAuth to authenticate with Google Sheets. Authentication is handled using a stored token in the `.secrets` folder for secure access.

### Initial Authentication Setup:

```r
# Set the authentication token to be stored in a folder called `.secrets`
options(gargle_oauth_cache = ".secrets")

# Authenticate manually
gs4_auth()

# Verify the token is saved
list.files(".secrets/")
```

### Non-Interactive Authentication:
```r
gs4_auth(cache = ".secrets", email = "your-email@gmail.com")
```

## Packages Used

The app is built using the following R packages:

- `tidyverse` for data manipulation
- `googlesheets4` for accessing Google Sheets
- `shiny` for the interactive web app framework
- `lubridate` for date manipulation
- `bslib` for theming the UI
- `ggbeeswarm` for creating beeswarm plots
- `bsicons` for adding custom icons in the UI

## How It Works

The app retrieves the Google Sheets data and processes it by normalizing salary ranges based on hourly, biweekly, and annual pay units.
Various metrics are calculated, such as total applications, cover letters, exams, and interviews.
The UI is rendered dynamically, allowing users to filter data by date and view the results in various charts.

## Visual Components

**Value Boxes**: Quickly see key application stats at a glance.
**Bar Plot**: Shows the number of applications for each job sector and the outcome.
**Violin Plot with Beeswarm**: Displays salary distribution across job sectors with additional NA count annotation.
**Smooth Line Plot**: Shows job application trends over time, broken down by week.

## Future Plans

**Improved UI**: Enhance the user interface to allow more customization and filtering.
**More Detailed Analytics**: Provide deeper insights into application outcomes and salary trends.

## How to Run the App

To run the app locally, follow these steps:

Install the necessary packages:

1. Clone this repository and open the R project.

2. Authenticate with Google Sheets by running the OAuth setup in your console.

3. Run the app:

```r
shiny::runApp()
```

## References 

Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J,
Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43),
1686. doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.

Bryan J (2023). _googlesheets4: Access Google Sheets using the Sheets API V4_. R package version 1.1.1, <https://CRAN.R-project.org/package=googlesheets4>.

Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2023). _shiny: Web Application Framework for R_. R package
version 1.8.0, <https://CRAN.R-project.org/package=shiny>.

Grolemund G, Wickham H (2011). “Dates and Times Made Easy with lubridate.” _Journal of Statistical Software_, *40*(3), 1-25. <https://www.jstatsoft.org/v40/i03/>.

Sievert C, Cheng J, Aden-Buie G (2023). _bslib: Custom 'Bootstrap' 'Sass' Themes for 'shiny' and 'rmarkdown'_. R package version 0.6.1,
<https://CRAN.R-project.org/package=bslib>.

Clarke E, Sherrill-Mix S, Dawson C (2023). _ggbeeswarm: Categorical Scatter (Violin Point) Plots_. R package version 0.7.2,
<https://CRAN.R-project.org/package=ggbeeswarm>.

R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria.
<https://www.R-project.org/>.
