
# generate_pseudo_data.R
# Script to generate pseudo datasets for Raising The Village Dashboard

library(tidyverse)
library(lubridate)
library(sf)

set.seed(42)

# 1. Household Vulnerability Scoring Dataset
generate_household_data <- function(n = 5000) {
  
  # Generate districts in Mbarara region
  districts <- c("Mbarara City", "Rwampara", "Ibanda", "Sheema", "Bushenyi", 
                 "Kiruhura", "Kazo", "Isingiro")
  
  # Generate villages (20-30 per district)
  villages <- expand.grid(
    district = districts,
    village_num = 1:25
  ) %>%
    mutate(village_name = paste0(district, "_Village_", village_num))
  
  household_data <- tibble(
    household_id = sprintf("HH%06d", 1:n),
    district = sample(districts, n, replace = TRUE, 
                      prob = c(0.25, 0.15, 0.12, 0.12, 0.11, 0.10, 0.08, 0.07)),
    village = NA,
    enrollment_date = sample(seq(as.Date('2020-01-01'), 
                                 as.Date('2024-12-31'), by="day"), n, replace = TRUE),
    
    # Demographics
    household_size = rpois(n, lambda = 5) + 1,
    num_children_under_5 = rpois(n, lambda = 1.2),
    num_school_age_children = rpois(n, lambda = 2),
    female_headed = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
    
    # Economic indicators
    baseline_income_usd_daily = abs(rnorm(n, mean = 0.85, sd = 0.3)),
    current_income_usd_daily = NA,
    
    # Agricultural indicators
    land_size_acres = abs(rnorm(n, mean = 2.5, sd = 1.2)),
    crops_grown = sample(c("Banana", "Coffee", "Maize", "Beans", "Banana,Coffee", 
                           "Maize,Beans", "Coffee,Maize", "Banana,Beans,Maize"), 
                         n, replace = TRUE),
    uses_improved_seeds = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
    has_irrigation = sample(c(0, 1), n, replace = TRUE, prob = c(0.85, 0.15)),
    
    # Health & Sanitation
    has_safe_water_access = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4)),
    water_collection_time_mins = ifelse(has_safe_water_access == 1, 
                                        rpois(n, lambda = 15), 
                                        rpois(n, lambda = 45) + 20),
    has_latrine = sample(c(0, 1), n, replace = TRUE, prob = c(0.5, 0.5)),
    
    # Financial inclusion
    has_vsla_membership = sample(c(0, 1), n, replace = TRUE, prob = c(0.3, 0.7)),
    savings_amount_usd = ifelse(has_vsla_membership == 1, 
                                abs(rnorm(n, mean = 25, sd = 15)), 0),
    
    # Program participation
    program_phase = sample(c("Baseline", "Active", "Graduated"), n, 
                           replace = TRUE, prob = c(0.2, 0.5, 0.3)),
    months_in_program = NA
  )
  
  # Assign villages based on district
  for (i in 1:nrow(household_data)) {
    district_villages <- villages %>% 
      filter(district == household_data$district[i])
    household_data$village[i] <- sample(district_villages$village_name, 1)
  }
  
  # Calculate program duration based on phase
  household_data <- household_data %>%
    mutate(
      months_in_program = case_when(
        program_phase == "Baseline" ~ rpois(n(), lambda = 3),
        program_phase == "Active" ~ rpois(n(), lambda = 12) + 3,
        program_phase == "Graduated" ~ rpois(n(), lambda = 6) + 24
      ),
      # Income improves with program participation
      current_income_usd_daily = case_when(
        program_phase == "Baseline" ~ baseline_income_usd_daily * rnorm(n(), mean = 1.05, sd = 0.1),
        program_phase == "Active" ~ baseline_income_usd_daily * rnorm(n(), mean = 1.8, sd = 0.3),
        program_phase == "Graduated" ~ baseline_income_usd_daily * rnorm(n(), mean = 2.67, sd = 0.4)
      )
    )
  
  # Calculate vulnerability score (0-100, higher = more vulnerable)
  household_data <- household_data %>%
    mutate(
      vulnerability_score = round(
        (100 - current_income_usd_daily * 20) * 0.3 +
          (100 - has_safe_water_access * 50) * 0.2 +
          (100 - has_latrine * 50) * 0.15 +
          (female_headed * 20) * 0.1 +
          (100 - uses_improved_seeds * 50) * 0.15 +
          (100 - has_vsla_membership * 40) * 0.1
      )
    )
  
  return(household_data)
}

# 2. Climate Risk Data
generate_climate_data <- function() {
  
  # Create grid for Mbarara region (approximately -0.6 to 0.0 lat, 30.5 to 31.0 lon)
  grid <- expand.grid(
    lat = seq(-0.6, 0.0, by = 0.05),
    lon = seq(30.5, 31.0, by = 0.05)
  )
  
  # Generate monthly climate data for 2020-2024
  dates <- seq(as.Date("2020-01-01"), as.Date("2024-12-31"), by = "month")
  
  climate_data <- expand.grid(
    lat = grid$lat,
    lon = grid$lon,
    date = dates
  ) %>%
    mutate(
      month = month(date),
      year = year(date),
      
      # Temperature (seasonal variation)
      avg_temp_c = 22 + 2 * sin((month - 1) * pi / 6) + rnorm(n(), 0, 0.5),
      max_temp_c = avg_temp_c + rnorm(n(), mean = 8, sd = 1),
      min_temp_c = avg_temp_c - rnorm(n(), mean = 7, sd = 1),
      
      # Rainfall (bimodal pattern typical for Uganda)
      rainfall_mm = case_when(
        month %in% c(3, 4, 5) ~ abs(rnorm(n(), mean = 150, sd = 40)),  # First rainy season
        month %in% c(9, 10, 11) ~ abs(rnorm(n(), mean = 180, sd = 50)), # Second rainy season
        TRUE ~ abs(rnorm(n(), mean = 40, sd = 20))  # Dry seasons
      ),
      
      # Drought risk index (0-1, higher = more risk)
      drought_risk = case_when(
        rainfall_mm < 30 ~ runif(n(), 0.7, 1.0),
        rainfall_mm < 60 ~ runif(n(), 0.4, 0.7),
        rainfall_mm < 100 ~ runif(n(), 0.2, 0.4),
        TRUE ~ runif(n(), 0.0, 0.2)
      ),
      
      # Climate anomaly flag
      is_anomaly = rainfall_mm < quantile(rainfall_mm, 0.1) | 
        rainfall_mm > quantile(rainfall_mm, 0.9) |
        avg_temp_c < quantile(avg_temp_c, 0.1) |
        avg_temp_c > quantile(avg_temp_c, 0.9)
    )
  
  return(climate_data)
}

# 3. Crop Diagnostics Data
generate_crop_diagnostics <- function(n = 10000) {
  
  household_data <- generate_household_data(1000)
  
  # Generate multiple observations per household
  crop_data <- household_data %>%
    sample_n(n, replace = TRUE) %>%
    mutate(
      observation_id = sprintf("OBS%06d", 1:n),
      observation_date = enrollment_date + days(sample(0:730, n, replace = TRUE)),
      
      # Crop type from household
      crop_type = map_chr(str_split(crops_grown, ","), ~ sample(.x, 1)),
      
      # Growth stage
      growth_stage = sample(c("Seedling", "Vegetative", "Flowering", "Fruiting", "Harvest"), 
                            n, replace = TRUE),
      
      # Plant health metrics
      leaf_color_score = runif(n, 1, 5),  # 1=yellow, 5=dark green
      plant_height_cm = case_when(
        growth_stage == "Seedling" ~ rnorm(n, 15, 5),
        growth_stage == "Vegetative" ~ rnorm(n, 50, 15),
        growth_stage == "Flowering" ~ rnorm(n, 120, 30),
        growth_stage == "Fruiting" ~ rnorm(n, 150, 40),
        growth_stage == "Harvest" ~ rnorm(n, 180, 45)
      ),
      
      # Disease/pest indicators
      has_pest_damage = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
      pest_type = ifelse(has_pest_damage == 1, 
                         sample(c("Aphids", "Caterpillars", "Beetles", "Mites"), n, replace = TRUE),
                         NA),
      disease_present = sample(c(0, 1), n, replace = TRUE, prob = c(0.75, 0.25)),
      disease_type = ifelse(disease_present == 1,
                            sample(c("Bacterial wilt", "Leaf spot", "Root rot", "Mosaic virus"), 
                                   n, replace = TRUE),
                            NA),
      
      # Soil metrics
      soil_ph = runif(n, 5.5, 7.5),
      soil_moisture_pct = runif(n, 10, 60),
      
      # Yield prediction (kg per plant/tree)
      predicted_yield_kg = case_when(
        crop_type == "Banana" ~ pmax(0, rnorm(n, 30, 10) * leaf_color_score/3 * (1 - has_pest_damage * 0.3)),
        crop_type == "Coffee" ~ pmax(0, rnorm(n, 2, 0.5) * leaf_color_score/3 * (1 - disease_present * 0.4)),
        crop_type == "Maize" ~ pmax(0, rnorm(n, 0.3, 0.1) * leaf_color_score/3 * (1 - has_pest_damage * 0.25)),
        crop_type == "Beans" ~ pmax(0, rnorm(n, 0.2, 0.05) * leaf_color_score/3 * (1 - disease_present * 0.35))
      ),
      
      # GPS coordinates (near household location with some variation)
      lat = runif(n, -0.6, 0.0),
      lon = runif(n, 30.5, 31.0),
      
      # Field officer who made observation
      field_officer_id = sample(sprintf("FO%03d", 1:20), n, replace = TRUE)
    ) %>%
    select(-crops_grown)  # Remove the concatenated field
  
  return(crop_data)
}

# 4. Program Impact Metrics (Time Series)
generate_impact_metrics <- function() {
  
  dates <- seq(as.Date("2020-01-01"), as.Date("2024-12-31"), by = "month")
  districts <- c("Mbarara City", "Rwampara", "Ibanda", "Sheema", "Bushenyi", 
                 "Kiruhura", "Kazo", "Isingiro")
  
  impact_data <- expand.grid(
    date = dates,
    district = districts
  ) %>%
    mutate(
      year = year(date),
      month = month(date),
      quarter = quarter(date),
      
      # Cumulative beneficiaries (growing over time)
      cumulative_beneficiaries = round(500 * (1 + (as.numeric(date - min(date))/365) * 0.8) + 
                                         rnorm(n(), 0, 50)),
      
      # Active households
      active_households = round(cumulative_beneficiaries * runif(n(), 0.6, 0.8)),
      
      # Average income increase %
      avg_income_increase_pct = pmin(300, 20 * (as.numeric(date - min(date))/365) + 
                                       rnorm(n(), 0, 10)),
      
      # VSLA metrics
      active_vsla_groups = round(active_households / 25),
      total_savings_usd = active_vsla_groups * runif(n(), 800, 1500),
      loans_disbursed_usd = total_savings_usd * runif(n(), 0.6, 0.85),
      
      # Water access improvement
      households_with_water_access = round(active_households * 
                                             (0.4 + 0.4 * (as.numeric(date - min(date))/365)/5)),
      
      # Agricultural productivity
      avg_crop_yield_increase_pct = pmin(200, 15 * (as.numeric(date - min(date))/365) + 
                                           rnorm(n(), 0, 8)),
      
      # ROI calculation (increasing over time)
      program_cost_usd = active_households * 29.19,
      program_value_generated_usd = active_households * 
        (50 + 450 * (as.numeric(date - min(date))/365)/5),
      roi_percentage = (program_value_generated_usd - program_cost_usd) / program_cost_usd * 100
    )
  
  return(impact_data)
}

# 5. Field Officer Activity Data
generate_field_activity <- function(n = 15000) {
  
  field_officers <- tibble(
    officer_id = sprintf("FO%03d", 1:20),
    officer_name = paste("Officer", LETTERS[1:20]),
    district_assigned = sample(c("Mbarara City", "Rwampara", "Ibanda", "Sheema", 
                                 "Bushenyi", "Kiruhura", "Kazo", "Isingiro"), 
                               20, replace = TRUE)
  )
  
  activity_data <- tibble(
    activity_id = sprintf("ACT%06d", 1:n),
    officer_id = sample(field_officers$officer_id, n, replace = TRUE),
    activity_date = sample(seq(as.Date('2023-01-01'), 
                               as.Date('2024-12-31'), by="day"), n, replace = TRUE),
    activity_type = sample(c("Household Visit", "Training Session", "VSLA Meeting", 
                             "Crop Assessment", "Water Point Inspection", "Data Collection"),
                           n, replace = TRUE, 
                           prob = c(0.3, 0.2, 0.15, 0.15, 0.1, 0.1)),
    duration_hours = case_when(
      activity_type == "Training Session" ~ runif(n, 2, 4),
      activity_type == "VSLA Meeting" ~ runif(n, 1.5, 3),
      TRUE ~ runif(n, 0.5, 2)
    ),
    households_reached = case_when(
      activity_type == "Training Session" ~ rpois(n, lambda = 25),
      activity_type == "VSLA Meeting" ~ rpois(n, lambda = 20),
      activity_type == "Household Visit" ~ rpois(n, lambda = 3),
      TRUE ~ rpois(n, lambda = 5)
    ),
    lat = runif(n, -0.6, 0.0),
    lon = runif(n, 30.5, 31.0),
    notes = sample(c("Successful engagement", "Follow-up needed", "Good progress observed",
                     "Challenges identified", "Training well received", NA),
                   n, replace = TRUE)
  ) %>%
    left_join(field_officers, by = "officer_id")
  
  return(activity_data)
}

# Generate all datasets
message("Generating household vulnerability data...")
household_data <- generate_household_data(5000)

message("Generating climate risk data...")
climate_data <- generate_climate_data()

message("Generating crop diagnostics data...")
crop_diagnostics <- generate_crop_diagnostics(10000)

message("Generating program impact metrics...")
impact_metrics <- generate_impact_metrics()

message("Generating field officer activity data...")
field_activity <- generate_field_activity(15000)

# Save datasets
dir.create("data", showWarnings = FALSE)

write_csv(household_data, "data/household_vulnerability_data.csv")
write_csv(climate_data, "data/climate_risk_data.csv")
write_csv(crop_diagnostics, "data/crop_diagnostics_data.csv")
write_csv(impact_metrics, "data/program_impact_metrics.csv")
write_csv(field_activity, "data/field_officer_activity.csv")

message("All pseudo datasets generated and saved to data/ directory!")

# Display summaries
cat("\nDataset Summaries:\n")
cat("- Household Data:", nrow(household_data), "households\n")
cat("- Climate Data:", nrow(climate_data), "observations\n")
cat("- Crop Diagnostics:", nrow(crop_diagnostics), "assessments\n")
cat("- Impact Metrics:", nrow(impact_metrics), "monthly records\n")
cat("- Field Activities:", nrow(field_activity), "activities\n")
