


# R/app_ui.R - UI Configuration

#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import shinyWidgets
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Main dashboard page
    dashboardPage(
      skin = "green",
      
      # Header
      dashboardHeader(
        title = tags$div(
          tags$img(src = "www/rtv_logo.png", height = "40px", style = "margin-right: 10px;"),
          tags$span("RTV Data Science Hub", style = "font-weight: bold;")
        ),
        
        # Header dropdown menus
        dropdownMenu(
          type = "notifications",
          icon = icon("bullhorn"),
          badgeStatus = "success",
          notificationItem(
            text = "301,273 beneficiaries reached in 2023",
            icon = icon("users"),
            status = "success"
          ),
          notificationItem(
            text = "267% income increase achieved",
            icon = icon("chart-line"),
            status = "info"
          )
        ),
        
        dropdownMenu(
          type = "tasks",
          badgeStatus = "danger",
          taskItem(value = 90, color = "green", "Data Collection"),
          taskItem(value = 75, color = "yellow", "Model Training"),
          taskItem(value = 60, color = "red", "Field Validation")
        )
      ),
      
      # Sidebar
      dashboardSidebar(
        sidebarMenu(
          id = "sidebar",
          
          menuItem("Executive Overview", 
                   tabName = "overview", 
                   icon = icon("dashboard"),
                   selected = TRUE),
          
          menuItem("Vulnerability Scoring", 
                   tabName = "vulnerability", 
                   icon = icon("heartbeat")),
          
          menuItem("Climate Risk Engine", 
                   tabName = "climate", 
                   icon = icon("cloud-sun-rain")),
          
          menuItem("Crop Diagnostics", 
                   tabName = "crops", 
                   icon = icon("seedling")),
          
          menuItem("Impact Metrics", 
                   tabName = "impact", 
                   icon = icon("chart-line")),
          
          menuItem("Field Activities", 
                   tabName = "field", 
                   icon = icon("map-marked-alt")),
          
          menuItem("Data Sources",
                   icon = icon("database"),
                   menuSubItem("World Bank Indicators", 
                               tabName = "worldbank"),
                   menuSubItem("Upload New Data", 
                               tabName = "upload")),
          
          # Add a search box
          tags$div(
            style = "padding: 20px;",
            searchInput(
              inputId = "search_global",
              label = "Search dashboard",
              placeholder = "Search...",
              btnSearch = icon("search"),
              btnReset = icon("remove"),
              width = "100%"
            )
          ),
          
          # Info box at bottom
          tags$div(
            class = "sidebar-footer",
            style = "position: absolute; bottom: 0; width: 100%; padding: 10px; background-color: #1a1a1a;",
            tags$small(
              tags$p("Powered by Project Venn", style = "margin: 0; color: #999;"),
              tags$p("Last updated: ", Sys.Date(), style = "margin: 0; color: #999;")
            )
          )
        )
      ),
      
      # Body
      dashboardBody(
        # Custom CSS
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"),
          tags$script(src = "www/custom.js"),
          
          # Include Plotly for advanced visualizations
          tags$script(src = "https://cdn.plot.ly/plotly-latest.min.js"),
          
          # Custom fonts
          tags$link(
            href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap",
            rel = "stylesheet"
          )
        ),
        
        # Tab items
        tabItems(
          # Overview tab
          tabItem(
            tabName = "overview",
            mod_overview_ui("overview_1")
          ),
          
          # Vulnerability scoring tab
          tabItem(
            tabName = "vulnerability",
            mod_vulnerability_scoring_ui("vulnerability_1")
          ),
          
          # Climate risk tab
          tabItem(
            tabName = "climate",
            mod_climate_risk_ui("climate_1")
          ),
          
          # Crop diagnostics tab
          tabItem(
            tabName = "crops",
            mod_crop_diagnostics_ui("crops_1")
          ),
          
          # Impact metrics tab
          tabItem(
            tabName = "impact",
            mod_impact_metrics_ui("impact_1")
          ),
          
          # Field activities tab
          tabItem(
            tabName = "field",
            mod_field_activities_ui("field_1")
          ),
          
          # World Bank data tab
          tabItem(
            tabName = "worldbank",
            h2("World Bank Agriculture Indicators"),
            fluidRow(
              box(
                width = 12,
                title = "Uganda Agriculture and Rural Development Data",
                status = "primary",
                solidHeader = TRUE,
                DT::dataTableOutput("worldbank_table")
              )
            )
          ),
          
          # Upload data tab
          tabItem(
            tabName = "upload",
            h2("Upload New Dataset"),
            fluidRow(
              box(
                width = 6,
                title = "File Upload",
                status = "warning",
                solidHeader = TRUE,
                fileInput("file_upload", "Choose CSV File",
                          accept = c(".csv", ".xlsx"),
                          multiple = FALSE),
                
                radioButtons("file_type", "File Type:",
                             choices = list("CSV" = "csv", "Excel" = "xlsx"),
                             selected = "csv"),
                
                actionButton("upload_btn", "Process File", 
                             icon = icon("upload"),
                             class = "btn-success")
              ),
              
              box(
                width = 6,
                title = "Upload Status",
                status = "info",
                solidHeader = TRUE,
                verbatimTextOutput("upload_status")
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' @import shiny
#' @noRd
golem_add_external_resources <- function() {
  
  # Check if www directory exists
  if (dir.exists("www")) {
    addResourcePath('www', 'www')
  }
  
  tags$head(
    # Add custom CSS and JS
    if (file.exists("www/custom.css")) {
      tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
    },
    if (file.exists("www/custom.js")) {
      tags$script(src = "www/custom.js")
    },
    
    # Add custom meta tags
    tags$meta(name = "description", content = "Raising The Village Data Science Dashboard"),
    tags$meta(name = "author", content = "RTV Data Team"),
    
    # Add favicon if exists
    if (file.exists("www/favicon.ico")) {
      tags$link(rel = "icon", type = "image/x-icon", href = "www/favicon.ico")
    }
  )
}

