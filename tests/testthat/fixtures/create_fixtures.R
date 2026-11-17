# Fixture Generation Script
# =============================================================================
# Run this script once to create test fixtures.
# Uses only base R for CSV files.
# Excel fixtures require openxlsx package (optional).
#
# Usage: source("tests/testthat/fixtures/create_fixtures.R")
# =============================================================================

create_test_fixtures <- function() {
  # Determine fixture directory
  fixture_dir <- tryCatch({
    dirname(sys.frame(1)$ofile)
  }, error = function(e) NULL)

  if (is.null(fixture_dir) || fixture_dir == "" || fixture_dir == ".") {
    fixture_dir <- "tests/testthat/fixtures"
  }

  if (!dir.exists(fixture_dir)) {
    dir.create(fixture_dir, recursive = TRUE)
  }

  message("Creating test fixtures in: ", fixture_dir)

  # -------------------------------------------------------------------------
  # CSV Fixtures (base R - always work)
  # -------------------------------------------------------------------------

  # Fixture 1: CWT data
  csv_data <- data.frame(
    ORG.CODE = c("RRK", "RL4", "RNA", "RKB"),
    PERIOD = c("2024-04", "2024-04", "2024-04", "2024-04"),
    WITHIN.STANDARD = c(80, 75, 85, 90),
    TOTAL.TREATED = c(100, 100, 100, 100),
    stringsAsFactors = FALSE
  )
  write.csv(csv_data, file.path(fixture_dir, "test_cwt.csv"), row.names = FALSE)
  message("Created: test_cwt.csv")

  # Fixture 2: Second month CSV
  csv_data_may <- data.frame(
    ORG.CODE = c("RRK", "RL4", "RNA", "RKB"),
    PERIOD = c("2024-05", "2024-05", "2024-05", "2024-05"),
    WITHIN.STANDARD = c(82, 78, 88, 92),
    TOTAL.TREATED = c(100, 100, 100, 100),
    stringsAsFactors = FALSE
  )
  write.csv(csv_data_may, file.path(fixture_dir, "test_cwt_may.csv"), row.names = FALSE)
  message("Created: test_cwt_may.csv")

  # Fixture 3: Empty CSV (for error testing)
  write.csv(data.frame(), file.path(fixture_dir, "test_empty.csv"), row.names = FALSE)
  message("Created: test_empty.csv")

  # -------------------------------------------------------------------------
  # Excel Fixtures (optional - requires openxlsx)
  # -------------------------------------------------------------------------

  excel_created <- FALSE

  if (requireNamespace("openxlsx", quietly = TRUE)) {
    tryCatch({
      library(openxlsx)

      # Multi-sheet workbook
      wb1 <- createWorkbook()

      # Sheet 1: FDS data
      fds_data <- data.frame(
        Provider.Code = c("RRK", "RL4", "RNA"),
        Cancer.Type = c("Lung", "Lung", "Lung"),
        Period = c("2024-04", "2024-04", "2024-04"),
        Within.Standard = c(80, 75, 85),
        Total.Patients = c(100, 100, 100),
        stringsAsFactors = FALSE
      )
      addWorksheet(wb1, "28-DAY FDS")
      writeData(wb1, "28-DAY FDS", fds_data)

      # Sheet 2: 31-day data
      day31_data <- data.frame(
        Provider.Code = c("RRK", "RL4", "RNA"),
        Cancer.Type = c("Lung", "Lung", "Lung"),
        Period = c("2024-04", "2024-04", "2024-04"),
        Within.Standard = c(95, 92, 98),
        Total.Patients = c(100, 100, 100),
        stringsAsFactors = FALSE
      )
      addWorksheet(wb1, "31-DAY")
      writeData(wb1, "31-DAY", day31_data)

      # Sheet 3: 62-day data
      day62_data <- data.frame(
        Provider.Code = c("RRK", "RL4", "RNA"),
        Cancer.Type = c("Lung", "Lung", "Lung"),
        Period = c("2024-04", "2024-04", "2024-04"),
        Within.Standard = c(70.5, 68, 75.5),
        Total.Patients = c(100, 100, 100),
        stringsAsFactors = FALSE
      )
      addWorksheet(wb1, "62-DAY")
      writeData(wb1, "62-DAY", day62_data)

      saveWorkbook(wb1, file.path(fixture_dir, "test_cwt_data.xlsx"), overwrite = TRUE)
      message("Created: test_cwt_data.xlsx (3 sheets)")

      # Second month file
      wb2 <- createWorkbook()
      fds_data_may <- data.frame(
        Provider.Code = c("RRK", "RL4", "RNA"),
        Cancer.Type = c("Lung", "Lung", "Lung"),
        Period = c("2024-05", "2024-05", "2024-05"),
        Within.Standard = c(82, 78, 88),
        Total.Patients = c(100, 100, 100),
        stringsAsFactors = FALSE
      )
      addWorksheet(wb2, "28-DAY FDS")
      writeData(wb2, "28-DAY FDS", fds_data_may)
      saveWorkbook(wb2, file.path(fixture_dir, "test_cwt_data_may.xlsx"), overwrite = TRUE)
      message("Created: test_cwt_data_may.xlsx (1 sheet)")

      excel_created <- TRUE
    }, error = function(e) {
      message("Note: Could not create Excel fixtures: ", e$message)
    })
  } else {
    message("Note: openxlsx package not installed - Excel fixtures not created")
    message("      Install with: install.packages('openxlsx')")
  }

  message("\nFixture creation complete!")
  message("CSV fixtures: 3 created")
  message("Excel fixtures: ", ifelse(excel_created, "2 created", "0 (openxlsx not available)"))

  invisible(TRUE)
}

# Run if sourced directly
if (sys.nframe() == 0) {
  create_test_fixtures()
}
