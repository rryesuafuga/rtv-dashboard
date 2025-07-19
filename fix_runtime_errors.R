

# fix_runtime_errors.R - Fix runtime errors in the dashboard

cat("Fixing runtime errors in RTV Dashboard...\n\n")

# Fix 1: Update vulnerability heatmap in mod_vulnerability_scoring.R
if (file.exists("R/mod_vulnerability_scoring.R")) {
  cat("Fixing duplicate row names error in vulnerability heatmap...\n")
  
  content <- readLines("R/mod_vulnerability_scoring.R")
  
  # Find the heatmap section
  start_line <- grep("# Vulnerability heatmap", content)
  if (length(start_line) > 0) {
    # Find the end of the renderPlotly function
    end_line <- start_line[1]
    brace_count <- 0
    in_function <- FALSE
    
    for (i in (start_line[1]+1):length(content)) {
      if (grepl("renderPlotly\\(\\{", content[i])) {
        in_function <- TRUE
      }
      if (in_function) {
        brace_count <- brace_count + str_count(content[i], "\\{") - str_count(content[i], "\\}")
        if (brace_count == 0 && grepl("\\}\\)", content[i])) {
          end_line <- i
          break
        }
      }
    }
    
    # Replace with fixed version
    new_heatmap <- '    # Vulnerability heatmap
    output$vulnerability_heatmap <- renderPlotly({
      req(filtered_data())
      
      # Create matrix data for heatmap
      heatmap_data <- filtered_data() %>%
        mutate(
          income_category = cut(current_income_usd_daily,
                               breaks = c(0, 0.5, 1, 1.5, 2, Inf),
                               labels = c("<$0.50", "$0.50-1", "$1-1.50", 
                                        "$1.50-2", ">$2"))
        ) %>%
        group_by(district, income_category) %>%
        summarise(
          avg_vulnerability = mean(vulnerability_score, na.rm = TRUE),
          count = n(),
          .groups = "drop"
        ) %>%
        complete(district, income_category, fill = list(avg_vulnerability = NA, count = 0))
      
      # Create matrix - handle duplicate districts by aggregating
      matrix_prep <- heatmap_data %>%
        group_by(district, income_category) %>%
        summarise(avg_vulnerability = mean(avg_vulnerability, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = income_category, 
                   values_from = avg_vulnerability,
                   values_fill = 0)
      
      # Extract row names and matrix separately
      row_names <- matrix_prep$district
      matrix_data <- as.matrix(matrix_prep[, -1])
      
      plot_ly(
        z = matrix_data,
        x = colnames(matrix_data),
        y = row_names,
        type = "heatmap",
        colorscale = list(
          c(0, "#4CAF50"),
          c(0.5, "#FFEB3B"),
          c(1, "#F44336")
        ),
        hovertemplate = "District: %{y}<br>Income: %{x}<br>Avg Score: %{z:.1f}<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "Daily Income Category (USD)"),
          yaxis = list(title = "District"),
          font = list(family = "Inter, sans-serif")
        )
    })'
    
    # Replace in content
    new_content <- c(
      content[1:(start_line[1]-1)],
      strsplit(new_heatmap, "\n")[[1]],
      if (end_line < length(content)) content[(end_line+1):length(content)] else character(0)
    )
    
    writeLines(new_content, "R/mod_vulnerability_scoring.R")
    cat("✓ Fixed vulnerability heatmap\n")
  }
}

# Fix 2: Update climate time series loess in mod_climate_risk.R
if (file.exists("R/mod_climate_risk.R")) {
  cat("\nFixing loess span warnings in climate module...\n")
  
  content <- readLines("R/mod_climate_risk.R")
  
  # Fix the loess span issue
  loess_line <- grep("predict\\(loess.*span = 0\\.3", content)
  if (length(loess_line) > 0) {
    # Replace with conditional loess
    content[loess_line[1]] <- '        add_trace(y = ~{'
    content[loess_line[1]+1] <- '                    if(nrow(monthly_data) > 10) {'
    content[loess_line[1]+2] <- '                      predict(loess(avg_value ~ as.numeric(date), data = monthly_data, span = 0.5))'
    content[loess_line[1]+3] <- '                    } else {'
    content[loess_line[1]+4] <- '                      avg_value'
    content[loess_line[1]+5] <- '                    }'
    content[loess_line[1]+6] <- '                  },'
    
    writeLines(content, "R/mod_climate_risk.R")
    cat("✓ Fixed loess span issue\n")
  }
  
  # Fix the marker mode warning
  content <- readLines("R/mod_climate_risk.R")
  add_lines <- grep("add_lines.*marker.*size = 8", content)
  if (length(add_lines) > 0) {
    for (line_num in add_lines) {
      # Add mode = 'lines+markers' if it's missing
      if (!grepl("mode\\s*=", content[line_num])) {
        content[line_num] <- gsub(
          "line = list\\(color = \\'#FF5722\\', width = 3\\),",
          "line = list(color = \\'#FF5722\\', width = 3),\n                  mode = \\'lines+markers\\',",
          content[line_num]
        )
      }
    }
    writeLines(content, "R/mod_climate_risk.R")
    cat("✓ Fixed plotly marker warnings\n")
  }
}

# Fix 3: Add str_count function if missing
if (!exists("str_count")) {
  str_count <- function(string, pattern) {
    lengths(regmatches(string, gregexpr(pattern, string)))
  }
}

cat("\n✓ All runtime errors fixed!\n")
cat("\nRestart your R session and run: shiny::runApp()\n")
