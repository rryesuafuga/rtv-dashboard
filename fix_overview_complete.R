


# fix_overview_complete.R - Complete fix for the overview module value boxes

cat("Applying comprehensive fix to overview module...\n\n")

# Create a completely new overview module with proper reactive handling
overview_content <- '# R/mod_overview.R - Executive Overview Module

#\' overview UI Function
#\'
#\' @description Executive overview module for the dashboard
#\' @param id,input,output,session Internal parameters for {shiny}.
#\' @noRd 
#\' @importFrom shiny NS tagList 
mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Header with KPIs
    fluidRow(
      valueBoxOutput(ns("total_beneficiaries")),
      valueBoxOutput(ns("avg_income_increase")),
      valueBoxOutput(ns("roi_percentage")),
      valueBoxOutput(ns("water_access_rate"))
    ),
    
    # Main visualizations
    fluidRow(
      # Geographic distribution
      box(
        width = 8,
        title = "Geographic Distribution of Beneficiaries",
        status = "primary",
        solidHeader = TRUE,
        height = 500,
        leafletOutput(ns("map_distribution"), height = 450)
      ),
      
      # Program phase breakdown
      box(
        width = 4,
        title = "Program Phase Distribution",
        status = "success",
        solidHeader = TRUE,
        height = 500,
        plotlyOutput(ns("phase_pie"), height = 450)
      )
    ),
    
    # Time series and demographics
    fluidRow(
      # Income progression
      box(
        width = 6,
        title = "Income Progression Over Time",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotlyOutput(ns("income_timeline"))
      ),
      
      # Vulnerability distribution
      box(
        width = 6,
        title = "Household Vulnerability Distribution",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotlyOutput(ns("vulnerability_dist"))
      )
    ),
    
    # Quick stats table
    fluidRow(
      box(
        width = 12,
        title = "District-wise Summary Statistics",
        status = "primary",
        solidHeader = TRUE,
        div(style = "overflow-x: auto;",
            DT::dataTableOutput(ns("district_summary"))
        )
      )
    )
  )
}

#\' overview Server Functions
#\' @noRd 
mod_overview_server <- function(id, datasets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create reactive expressions for data
    household_data <- reactive({
      if (is.null(datasets$household)) {
        # Return empty data frame with expected structure
        data.frame(
          household_size = numeric(),
          baseline_income_usd_daily = numeric(),
          current_income_usd_daily = numeric(),
          has_safe_water_access = numeric(),
          district = character(),
          vulnerability_score = numeric(),
          program_phase = character(),
          stringsAsFactors = FALSE
        )
      } else {
        datasets$household
      }
    })
    
    impact_data <- reactive({
      if (is.null(datasets$impact)) {
        data.frame(
          date = as.Date(character()),
          roi_percentage = numeric(),
          avg_income_increase_pct = numeric(),
          stringsAsFactors = FALSE
        )
      } else {
        datasets$impact
      }
    })
    
    # Calculate KPIs with proper error handling
    output$total_beneficiaries <- renderValueBox({
      data <- household_data()
      
      if (nrow(data) == 0) {
        valueBox(
          value = "0",
          subtitle = "Total Beneficiaries",
          icon = icon("users"),
          color = "green"
        )
      } else {
        total <- nrow(data) * mean(data$household_size, na.rm = TRUE)
        valueBox(
          value = format(round(total), big.mark = ","),
          subtitle = "Total Beneficiaries",
          icon = icon("users"),
          color = "green"
        )
      }
    })
    
    output$avg_income_increase <- renderValueBox({
      data <- household_data()
      
      if (nrow(data) == 0 || 
          !("baseline_income_usd_daily" %in% names(data)) || 
          !("current_income_usd_daily" %in% names(data))) {
        valueBox(
          value = "0%",
          subtitle = "Average Income Increase",
          icon = icon("dollar-sign"),
          color = "yellow"
        )
      } else {
        baseline <- mean(data$baseline_income_usd_daily, na.rm = TRUE)
        current <- mean(data$current_income_usd_daily, na.rm = TRUE)
        
        if (is.na(baseline) || is.na(current) || baseline == 0) {
          increase_pct <- 0
        } else {
          increase_pct <- ((current - baseline) / baseline) * 100
        }
        
        valueBox(
          value = paste0("+", round(increase_pct), "%"),
          subtitle = "Average Income Increase",
          icon = icon("dollar-sign"),
          color = "yellow"
        )
      }
    })
    
    output$roi_percentage <- renderValueBox({
      data <- impact_data()
      
      if (nrow(data) == 0 || !("roi_percentage" %in% names(data))) {
        valueBox(
          value = "0%",
          subtitle = "Return on Investment",
          icon = icon("chart-line"),
          color = "aqua"
        )
      } else {
        latest_roi <- data %>%
          filter(date == max(date, na.rm = TRUE)) %>%
          summarise(avg_roi = mean(roi_percentage, na.rm = TRUE)) %>%
          pull(avg_roi)
        
        if (length(latest_roi) == 0 || is.na(latest_roi)) {
          latest_roi <- 0
        }
        
        valueBox(
          value = paste0(round(latest_roi), "%"),
          subtitle = "Return on Investment",
          icon = icon("chart-line"),
          color = "aqua"
        )
      }
    })
    
    output$water_access_rate <- renderValueBox({
      data <- household_data()
      
      if (nrow(data) == 0 || !("has_safe_water_access" %in% names(data))) {
        valueBox(
          value = "0%",
          subtitle = "Safe Water Access",
          icon = icon("tint"),
          color = "blue"
        )
      } else {
        water_rate <- mean(data$has_safe_water_access, na.rm = TRUE) * 100
        
        if (is.na(water_rate)) {
          water_rate <- 0
        }
        
        valueBox(
          value = paste0(round(water_rate), "%"),
          subtitle = "Safe Water Access",
          icon = icon("tint"),
          color = "blue"
        )
      }
    })
    
    # Geographic distribution map
    output$map_distribution <- renderLeaflet({
      data <- household_data()
      
      if (nrow(data) == 0) {
        # Return empty map
        leaflet() %>%
          addTiles() %>%
          setView(lng = 30.75, lat = -0.3, zoom = 8)
      } else {
        # Aggregate by district
        district_data <- data %>%
          group_by(district) %>%
          summarise(
            households = n(),
            beneficiaries = sum(household_size, na.rm = TRUE),
            avg_vulnerability = mean(vulnerability_score, na.rm = TRUE),
            .groups = "drop"
          )
        
        # Create a simple map (using approximate district centers)
        district_coords <- data.frame(
          district = c("Mbarara City", "Rwampara", "Ibanda", "Sheema", 
                      "Bushenyi", "Kiruhura", "Kazo", "Isingiro"),
          lat = c(-0.6061, -0.7000, -0.1167, -0.6167, 
                 -0.5833, -0.1833, -0.0333, -0.8833),
          lon = c(30.6545, 30.8333, 30.4833, 30.4667, 
                 30.2000, 30.9667, 30.9000, 30.8167)
        )
        
        map_data <- district_data %>%
          left_join(district_coords, by = "district")
        
        leaflet(map_data) %>%
          addTiles() %>%
          addCircleMarkers(
            ~lon, ~lat,
            radius = ~sqrt(beneficiaries/100),
            color = ~colorNumeric("YlOrRd", avg_vulnerability)(avg_vulnerability),
            fillOpacity = 0.7,
            popup = ~paste0(
              "<strong>", district, "</strong><br>",
              "Households: ", format(households, big.mark = ","), "<br>",
              "Beneficiaries: ", format(beneficiaries, big.mark = ","), "<br>",
              "Avg Vulnerability: ", round(avg_vulnerability, 1)
            )
          ) %>%
          addLegend(
            position = "bottomright",
            pal = colorNumeric("YlOrRd", map_data$avg_vulnerability),
            values = ~avg_vulnerability,
            title = "Vulnerability Score",
            opacity = 0.8
          )
      }
    })
    
    # Program phase pie chart
    output$phase_pie <- renderPlotly({
      data <- household_data()
      
      if (nrow(data) == 0 || !("program_phase" %in% names(data))) {
        # Empty plot
        plot_ly(type = "pie") %>%
          layout(
            title = "No data available",
            showlegend = FALSE
          )
      } else {
        phase_data <- data %>%
          count(program_phase) %>%
          mutate(percentage = n / sum(n) * 100)
        
        plot_ly(
          phase_data,
          labels = ~program_phase,
          values = ~n,
          type = "pie",
          textposition = "inside",
          textinfo = "label+percent",
          marker = list(colors = c("#2E7D32", "#FFA726", "#1E88E5")),
          hovertemplate = "%{label}<br>%{value} households<br>%{percent}<extra></extra>"
        ) %>%
          layout(
            showlegend = TRUE,
            font = list(family = "Inter, sans-serif")
          )
      }
    })
    
    # Income timeline
    output$income_timeline <- renderPlotly({
      data <- impact_data()
      
      if (nrow(data) == 0 || !("avg_income_increase_pct" %in% names(data))) {
        plot_ly(type = "scatter", mode = "lines") %>%
          layout(title = "No data available")
      } else {
        timeline_data <- data %>%
          group_by(date) %>%
          summarise(
            avg_increase = mean(avg_income_increase_pct, na.rm = TRUE),
            .groups = "drop"
          )
        
        plot_ly(
          timeline_data,
          x = ~date,
          y = ~avg_increase,
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "#4CAF50", width = 3),
          marker = list(size = 6),
          hovertemplate = "%{x|%b %Y}<br>Income Increase: %{y:.1f}%<extra></extra>"
        ) %>%
          layout(
            xaxis = list(title = "Date", gridcolor = "#E0E0E0"),
            yaxis = list(title = "Average Income Increase (%)", gridcolor = "#E0E0E0"),
            hovermode = "x unified",
            font = list(family = "Inter, sans-serif"),
            plot_bgcolor = "#FAFAFA"
          )
      }
    })
    
    # Vulnerability distribution
    output$vulnerability_dist <- renderPlotly({
      data <- household_data()
      
      if (nrow(data) == 0 || !("vulnerability_score" %in% names(data))) {
        plot_ly(type = "histogram") %>%
          layout(title = "No data available")
      } else {
        plot_ly(
          data,
          x = ~vulnerability_score,
          type = "histogram",
          nbinsx = 30,
          marker = list(
            color = ~vulnerability_score,
            colorscale = list(
              c(0, "#4CAF50"),
              c(0.5, "#FFA726"),
              c(1, "#F44336")
            ),
            line = list(color = "white", width = 1)
          ),
          hovertemplate = "Score: %{x}<br>Count: %{y}<extra></extra>"
        ) %>%
          layout(
            xaxis = list(title = "Vulnerability Score", gridcolor = "#E0E0E0"),
            yaxis = list(title = "Number of Households", gridcolor = "#E0E0E0"),
            bargap = 0.1,
            font = list(family = "Inter, sans-serif"),
            plot_bgcolor = "#FAFAFA"
          )
      }
    })
    
    # District summary table
    output$district_summary <- DT::renderDataTable({
      data <- household_data()
      
      if (nrow(data) == 0) {
        DT::datatable(
          data.frame(Message = "No data available"),
          options = list(dom = "t"),
          rownames = FALSE
        )
      } else {
        summary_data <- data %>%
          group_by(district) %>%
          summarise(
            Households = n(),
            Beneficiaries = sum(household_size, na.rm = TRUE),
            `Avg Income (USD/day)` = round(mean(current_income_usd_daily, na.rm = TRUE), 2),
            `Income Increase (%)` = round(
              (mean(current_income_usd_daily, na.rm = TRUE) - 
               mean(baseline_income_usd_daily, na.rm = TRUE)) / 
               mean(baseline_income_usd_daily, na.rm = TRUE) * 100, 1
            ),
            `Water Access (%)` = round(mean(has_safe_water_access, na.rm = TRUE) * 100, 1),
            `VSLA Members (%)` = round(mean(has_vsla_membership, na.rm = TRUE) * 100, 1),
            `Avg Vulnerability` = round(mean(vulnerability_score, na.rm = TRUE), 1),
            .groups = "drop"
          ) %>%
          arrange(desc(Beneficiaries))
        
        DT::datatable(
          summary_data,
          options = list(
            pageLength = 10,
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel"),
            columnDefs = list(
              list(className = "dt-center", targets = "_all")
            )
          ),
          class = "cell-border stripe hover compact",
          extensions = "Buttons",
          rownames = FALSE
        ) %>%
          DT::formatStyle(
            "Avg Vulnerability",
            backgroundColor = DT::styleInterval(
              c(40, 60), 
              c("#C8E6C9", "#FFF9C4", "#FFCDD2")
            )
          ) %>%
          DT::formatStyle(
            "Income Increase (%)",
            color = DT::styleInterval(c(100, 200), c("black", "darkgreen", "green")),
            fontWeight = "bold"
          )
      }
    })
  })
}
'

# Write the new content
writeLines(overview_content, "R/mod_overview.R")
cat("✓ Completely rewrote mod_overview.R with proper error handling\n")

# Also ensure the app loads data correctly
cat("\nChecking data loading in app_server.R...\n")

if (file.exists("R/app_server.R")) {
  server_content <- readLines("R/app_server.R", warn = FALSE)
  
  # Find where datasets are loaded
  load_line <- grep("datasets\\$household <- read_csv", server_content)
  
  if (length(load_line) > 0) {
    # Make sure error handling is in place
    if (!any(grepl("error = function", server_content))) {
      cat("Adding error handling to data loading...\n")
      
      # Find the observe block
      observe_start <- max(grep("observe\\(\\{|observeEvent\\(TRUE", server_content)[
        grep("observe\\(\\{|observeEvent\\(TRUE", server_content) < load_line[1]
      ])
      
      # Add tryCatch around data loading
      for (i in load_line) {
        server_content[i] <- paste0("      tryCatch({
        ", server_content[i], "
      }, error = function(e) {
        showNotification(paste(\"Error loading household data:\", e$message), type = \"error\")
        NULL
      })")
      }
    }
    
    writeLines(server_content, "R/app_server.R")
    cat("✓ Added error handling to data loading\n")
  }
}

cat("\n✓ Complete fix applied!\n")
cat("\nPlease restart your R session and run: shiny::runApp()\n")
cat("\nThe value boxes should now display properly with:\n")
cat("- Total Beneficiaries (green)\n")
cat("- Average Income Increase (yellow)\n")
cat("- Return on Investment (aqua)\n")
cat("- Safe Water Access (blue)\n")