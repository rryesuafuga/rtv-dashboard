


# fix_value_boxes.R - Fix the value boxes display issue

cat("Fixing value boxes in overview module...\n\n")

# Read the current overview module
if (file.exists("R/mod_overview.R")) {
  content <- readLines("R/mod_overview.R", warn = FALSE)
  
  # Find the value box outputs
  start_line <- grep("# Calculate KPIs", content)
  
  if (length(start_line) > 0) {
    cat("Updating value box rendering...\n")
    
    # Replace the value box sections with improved versions
    new_value_boxes <- '    # Calculate KPIs
    output$total_beneficiaries <- renderValueBox({
      if (is.null(datasets$household)) {
        valueBox(
          value = "Loading...",
          subtitle = "Total Beneficiaries",
          icon = icon("users"),
          color = "gray"
        )
      } else {
        total <- nrow(datasets$household) * mean(datasets$household$household_size)
        valueBox(
          value = format(round(total), big.mark = ","),
          subtitle = "Total Beneficiaries",
          icon = icon("users"),
          color = "green"
        )
      }
    })
    
    output$avg_income_increase <- renderValueBox({
      if (is.null(datasets$household)) {
        valueBox(
          value = "Loading...",
          subtitle = "Average Income Increase",
          icon = icon("dollar-sign"),
          color = "gray"
        )
      } else {
        baseline <- mean(datasets$household$baseline_income_usd_daily, na.rm = TRUE)
        current <- mean(datasets$household$current_income_usd_daily, na.rm = TRUE)
        increase_pct <- ((current - baseline) / baseline) * 100
        
        valueBox(
          value = paste0("+", round(increase_pct), "%"),
          subtitle = "Average Income Increase",
          icon = icon("dollar-sign"),
          color = "yellow"
        )
      }
    })
    
    output$roi_percentage <- renderValueBox({
      if (is.null(datasets$impact)) {
        valueBox(
          value = "Loading...",
          subtitle = "Return on Investment",
          icon = icon("chart-line"),
          color = "gray"
        )
      } else {
        latest_roi <- datasets$impact %>%
          filter(date == max(date)) %>%
          summarise(avg_roi = mean(roi_percentage, na.rm = TRUE)) %>%
          pull(avg_roi)
        
        valueBox(
          value = paste0(round(latest_roi), "%"),
          subtitle = "Return on Investment",
          icon = icon("chart-line"),
          color = "aqua"
        )
      }
    })
    
    output$water_access_rate <- renderValueBox({
      if (is.null(datasets$household)) {
        valueBox(
          value = "Loading...",
          subtitle = "Safe Water Access",
          icon = icon("tint"),
          color = "gray"
        )
      } else {
        water_rate <- mean(datasets$household$has_safe_water_access, na.rm = TRUE) * 100
        
        valueBox(
          value = paste0(round(water_rate), "%"),
          subtitle = "Safe Water Access",
          icon = icon("tint"),
          color = "blue"
        )
      }
    })'
    
    # Find where the value boxes end (look for next output or function)
    end_line <- start_line[1]
    for (i in (start_line[1]+1):length(content)) {
      if (grepl("^\\s*#\\s*Geographic distribution map|^\\s*output\\$map_distribution", content[i])) {
        end_line <- i - 1
        break
      }
    }
    
    # Replace the section
    new_content <- c(
      content[1:(start_line[1]-1)],
      strsplit(new_value_boxes, "\n")[[1]],
      content[(end_line+1):length(content)]
    )
    
    writeLines(new_content, "R/mod_overview.R")
    cat("✓ Fixed value box rendering\n")
  }
}

# Also ensure the app_server.R loads data immediately
if (file.exists("R/app_server.R")) {
  cat("\nUpdating app_server.R for immediate data loading...\n")
  
  content <- readLines("R/app_server.R", warn = FALSE)
  
  # Find the observe block that loads data
  observe_line <- grep("observe\\(\\{", content)
  if (length(observe_line) > 0) {
    # Change observe to observe with immediate execution
    for (i in observe_line) {
      if (grepl("withProgress.*Loading datasets", content[i+1])) {
        content[i] <- "  observeEvent(TRUE, {"
        break
      }
    }
    
    writeLines(content, "R/app_server.R")
    cat("✓ Updated data loading to be immediate\n")
  }
}

cat("\n✓ Value boxes should now display properly!\n")
cat("\nRestart the app to see the changes: shiny::runApp()\n")