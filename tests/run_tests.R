# Test Runner Script for WMCA EAG Data Pack
# =============================================================================
# This script checks for required packages, installs them if needed,
# and runs the testthat test suite.
#
# Usage: Rscript tests/run_tests.R
# Or in R: source("tests/run_tests.R")
# =============================================================================

# Clear existing objects
rm(list = ls())

# Ensure all dependencies are installed
source("requirements.R")

# Main execution
main <- function() {
  message("=== WMCA EAG Data Pack Test Suite ===\n")

  # Load testthat
  library(testthat)

  # Set project root for tests
  Sys.setenv(TESTTHAT_PROJECT_ROOT = getwd())

  # Create fixtures if they don't exist
  fixture_dir <- "tests/testthat/fixtures"
  if (!file.exists(file.path(fixture_dir, "test_cwt.csv"))) {
    message("\nCreating test fixtures...")
    source("tests/testthat/fixtures/create_fixtures.R")
    create_test_fixtures()
  }

  # Run tests
  message("\n=== Running Tests ===\n")
  test_results <- test_dir(
    "tests/testthat",
    reporter = "progress",
    stop_on_failure = FALSE
  )

  # Summary
  # message("\n=== Test Summary ===")
  # print(test_results)

  # Return results for programmatic use
  invisible(test_results)
}

# Run if executed directly
if (sys.nframe() == 0) {
  main()
}
