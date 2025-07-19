
# tests/testthat/test-fct_helpers.R - Unit tests for helper functions

library(testthat)
library(dplyr)

test_that("calculate_vulnerability_score works correctly", {
  # Test data
  test_household <- data.frame(
    has_safe_water_access = c(1, 0, 1, 0),
    has_latrine = c(1, 1, 0, 0),
    current_income_usd_daily = c(2, 1, 0.5, 0.3),
    uses_improved_seeds = c(1, 0, 1, 0),
    has_vsla_membership = c(1, 1, 0, 0),
    female_headed = c(0, 1, 0, 1)
  )
  
  scores <- calculate_vulnerability_score(test_household)
  
  # Test that scores are within valid range
  expect_true(all(scores >= 0 & scores <= 100))
  
  # Test that lower vulnerability factors result in higher scores
  expect_true(scores[4] > scores[1])  # Household 4 should be more vulnerable than 1
  
  # Test edge cases
  # Perfect household (all positive factors)
  perfect <- data.frame(
    has_safe_water_access = 1,
    has_latrine = 1,
    current_income_usd_daily = 3,
    uses_improved_seeds = 1,
    has_vsla_membership = 1,
    female_headed = 0
  )
  expect_lt(calculate_vulnerability_score(perfect), 20)
  
  # Worst case household
  worst <- data.frame(
    has_safe_water_access = 0,
    has_latrine = 0,
    current_income_usd_daily = 0,
    uses_improved_seeds = 0,
    has_vsla_membership = 0,
    female_headed = 1
  )
  expect_gt(calculate_vulnerability_score(worst), 80)
})

test_that("format_currency handles various inputs", {
  expect_equal(format_currency(1000), "$1,000.00")
  expect_equal(format_currency(0.50), "$0.50")
  expect_equal(format_currency(1234567.89), "$1,234,567.89")
  expect_equal(format_currency(100, "£"), "£100.00")
  
  # Test negative values
  expect_equal(format_currency(-500), "$-500.00")
})

test_that("calculate_roi returns correct percentages", {
  expect_equal(calculate_roi(100, 200), 100)  # 100% ROI
  expect_equal(calculate_roi(50, 100), 100)   # 100% ROI
  expect_equal(calculate_roi(100, 150), 50)   # 50% ROI
  
  # Test edge cases
  expect_equal(calculate_roi(100, 100), 0)    # Break even
  expect_lt(calculate_roi(100, 50), 0)        # Negative ROI
})

test_that("format_number handles large numbers correctly", {
  expect_equal(format_number(1000), "1K")
  expect_equal(format_number(1500), "2K")
  expect_equal(format_number(1000000), "1M")
  expect_equal(format_number(2500000, 1), "2.5M")
  expect_equal(format_number(999), "999")
  expect_equal(format_number(-5000), "-5K")
})

test_that("percent_change calculates correctly", {
  expect_equal(percent_change(100, 150), 50)
  expect_equal(percent_change(200, 100), -50)
  expect_equal(percent_change(50, 100), 100)
  
  # Test edge case
  expect_true(is.na(percent_change(0, 100)))
})

test_that("safe_divide prevents division by zero", {
  expect_equal(safe_divide(10, 2), 5)
  expect_equal(safe_divide(100, 0), NA)
  expect_equal(safe_divide(100, 0, 0), 0)
  expect_equal(safe_divide(0, 10), 0)
})

test_that("get_date_range returns correct periods", {
  # Test last week
  range_week <- get_date_range("last_week")
  expect_equal(as.numeric(range_week$end - range_week$start), 7)
  
  # Test last month
  range_month <- get_date_range("last_month")
  expect_equal(as.numeric(range_month$end - range_month$start), 30)
  
  # Test YTD
  range_ytd <- get_date_range("ytd")
  expect_equal(month(range_ytd$start), 1)
  expect_equal(day(range_ytd$start), 1)
  expect_equal(year(range_ytd$start), year(Sys.Date()))
})

test_that("validate_upload checks files correctly", {
  # Test NULL file
  expect_equal(validate_upload(NULL), "No file selected")
  
  # Test valid file
  valid_file <- list(
    name = "test.csv",
    size = 1024 * 1024  # 1 MB
  )
  expect_true(validate_upload(valid_file))
  
  # Test large file
  large_file <- list(
    name = "test.csv",
    size = 50 * 1024 * 1024  # 50 MB
  )
  expect_match(validate_upload(large_file, max_size_mb = 30), "too large")
  
  # Test invalid extension
  invalid_file <- list(
    name = "test.pdf",
    size = 1024 * 1024
  )
  expect_match(validate_upload(invalid_file), "Invalid file type")
})

test_that("generate_summary_stats aggregates correctly", {
  # Create test data
  test_data <- data.frame(
    district = rep(c("A", "B"), each = 5),
    vulnerability_score = runif(10, 20, 80),
    current_income_usd_daily = runif(10, 0.5, 2),
    has_safe_water_access = sample(c(0, 1), 10, replace = TRUE),
    has_vsla_membership = sample(c(0, 1), 10, replace = TRUE)
  )
  
  # Test without grouping
  summary_all <- generate_summary_stats(test_data)
  expect_equal(nrow(summary_all), 1)
  expect_equal(summary_all$n, 10)
  
  # Test with grouping
  summary_grouped <- generate_summary_stats(test_data, "district")
  expect_equal(nrow(summary_grouped), 2)
  expect_true(all(summary_grouped$n == 5))
  
  # Test that percentages are in valid range
  expect_true(all(summary_grouped$water_access_rate >= 0 & 
                    summary_grouped$water_access_rate <= 100))
})

# tests/testthat/test-mod_overview.R - Module tests

test_that("overview module UI creates correct elements", {
  ui <- mod_overview_ui("test")
  
  # Check that UI is a tagList
  expect_s3_class(ui, "shiny.tag.list")
  
  # Check for required outputs
  ui_html <- as.character(ui)
  expect_match(ui_html, "test-total_beneficiaries")
  expect_match(ui_html, "test-map_distribution")
  expect_match(ui_html, "test-income_timeline")
  expect_match(ui_html, "test-district_summary")
})

# Test for edge cases in data processing
test_that("data processing handles edge cases", {
  # Empty dataset
  empty_data <- data.frame()
  expect_silent(generate_summary_stats(empty_data))
  
  # Dataset with all NA values
  na_data <- data.frame(
    vulnerability_score = rep(NA, 5),
    current_income_usd_daily = rep(NA, 5),
    has_safe_water_access = rep(NA, 5),
    has_vsla_membership = rep(NA, 5)
  )
  summary_na <- generate_summary_stats(na_data)
  expect_true(is.na(summary_na$avg_vulnerability))
  
  # Dataset with extreme values
  extreme_data <- data.frame(
    vulnerability_score = c(0, 100, 50),
    current_income_usd_daily = c(0, 1000, 1),
    has_safe_water_access = c(0, 1, 1),
    has_vsla_membership = c(0, 1, 0)
  )
  summary_extreme <- generate_summary_stats(extreme_data)
  expect_equal(summary_extreme$avg_vulnerability, 50)
})

# Performance test
test_that("functions perform efficiently", {
  # Generate large dataset
  large_data <- data.frame(
    has_safe_water_access = sample(c(0, 1), 10000, replace = TRUE),
    has_latrine = sample(c(0, 1), 10000, replace = TRUE),
    current_income_usd_daily = runif(10000, 0, 3),
    uses_improved_seeds = sample(c(0, 1), 10000, replace = TRUE),
    has_vsla_membership = sample(c(0, 1), 10000, replace = TRUE),
    female_headed = sample(c(0, 1), 10000, replace = TRUE)
  )
  
  # Test that vulnerability calculation is fast
  system.time({
    scores <- calculate_vulnerability_score(large_data)
  }) -> time_taken
  
  expect_lt(time_taken["elapsed"], 0.1)  # Should take less than 0.1 seconds
})