# Tests for read_in_utils.R and validation_utils.R functions
# =============================================================================

# -----------------------------------------------------------------------------
# read_sheets_from_files() tests - Error handling (no fixtures required)
# -----------------------------------------------------------------------------

test_that("read_sheets_from_files() errors on empty file list", {
  expect_error(
    read_sheets_from_files(character(0), "Sheet1"),
    regexp = "No files provided to read_sheets_from_files"
  )
})

test_that("read_sheets_from_files() errors on non-existent file (require_all=TRUE)", {
  expect_error(
    read_sheets_from_files("nonexistent_file.xlsx", "Sheet1", require_all_sheets = TRUE),
    regexp = "File does not exist"
  )
})

test_that("read_sheets_from_files() warns and errors on non-existent file (require_all=FALSE)", {
  # When all files are missing, even with require_all=FALSE, the function errors
  # because no data can be read at all. This is correct behaviour.
  expect_error(
    expect_warning(
      read_sheets_from_files("nonexistent_file.xlsx", "Sheet1", require_all_sheets = FALSE),
      regexp = "File does not exist"
    ),
    regexp = "No data was successfully read"
  )
})

# -----------------------------------------------------------------------------
# read_sheets_from_files() tests - With fixtures
# -----------------------------------------------------------------------------

test_that("read_sheets_from_files() reads single sheet from file", {
  fixture_file <- get_fixture_path("test_cwt_data.xlsx")
  skip_if_not(file.exists(fixture_file), "Fixture file not found. Run create_fixtures.R first.")

  result <- read_sheets_from_files(fixture_file, "28-DAY FDS")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("Provider.Code" %in% names(result))
})

test_that("read_sheets_from_files() reads multiple sheets from file", {
  fixture_file <- get_fixture_path("test_cwt_data.xlsx")
  skip_if_not(file.exists(fixture_file), "Fixture file not found. Run create_fixtures.R first.")

  result <- read_sheets_from_files(fixture_file, c("28-DAY FDS", "31-DAY"))

  expect_s3_class(result, "data.frame")
  # Should have combined rows from both sheets
  expect_true(nrow(result) > 3)
})

test_that("read_sheets_from_files() reads from multiple files", {
  fixture1 <- get_fixture_path("test_cwt_data.xlsx")
  fixture2 <- get_fixture_path("test_cwt_data_may.xlsx")
  skip_if_not(
    file.exists(fixture1) && file.exists(fixture2),
    "Fixture files not found. Run create_fixtures.R first."
  )

  result <- read_sheets_from_files(c(fixture1, fixture2), "28-DAY FDS")

  expect_s3_class(result, "data.frame")
  # Should have rows from both files (April and May)
  expect_true(nrow(result) >= 6)
})

test_that("read_sheets_from_files() errors on missing sheet (require_all=TRUE)", {
  fixture_file <- get_fixture_path("test_cwt_data.xlsx")
  skip_if_not(file.exists(fixture_file), "Fixture file not found. Run create_fixtures.R first.")

  expect_error(
    read_sheets_from_files(fixture_file, "NONEXISTENT SHEET", require_all_sheets = TRUE),
    regexp = "not found"
  )
})

test_that("read_sheets_from_files() warns on missing sheet (require_all=FALSE)", {
  fixture_file <- get_fixture_path("test_cwt_data.xlsx")
  skip_if_not(file.exists(fixture_file), "Fixture file not found. Run create_fixtures.R first.")

  expect_warning(
    result <- read_sheets_from_files(
      fixture_file,
      c("28-DAY FDS", "NONEXISTENT SHEET"),
      require_all_sheets = FALSE
    ),
    regexp = "not found"
  )

  # Should still return data from the valid sheet
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("read_sheets_from_files() stops when the file can't be read as Excel at all (require_all=TRUE)", {
  dir <- withr::local_tempdir()
  corrupt <- file.path(dir, "corrupt.xlsx")
  writeLines("this is not a real xlsx file", corrupt)

  # get_available_sheets() issues its own warning first ("Could not read
  # sheets from file...") before returning NULL; suppress it here since this
  # test is about read_sheets_from_files()'s own stop(), not that warning.
  expect_error(
    suppressWarnings(read_sheets_from_files(corrupt, "28-DAY FDS", require_all_sheets = TRUE)),
    regexp = "Could not read sheet list from: corrupt\\.xlsx"
  )
})

test_that("read_sheets_from_files() warns and skips an unreadable file (require_all=FALSE), keeping good files", {
  dir <- withr::local_tempdir()
  corrupt <- file.path(dir, "corrupt.xlsx")
  writeLines("this is not a real xlsx file", corrupt)
  good_fixture <- get_fixture_path("test_cwt_data.xlsx")
  skip_if_not(file.exists(good_fixture), "Fixture file not found. Run create_fixtures.R first.")

  warnings_seen <- testthat::capture_warnings(
    result <- read_sheets_from_files(c(corrupt, good_fixture), "28-DAY FDS", require_all_sheets = FALSE)
  )
  all_warnings <- paste(warnings_seen, collapse = "\n")

  expect_match(all_warnings, "Could not read sheet list from: corrupt\\.xlsx")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("read_sheets_from_files() stops with the underlying read error when require_all=TRUE", {
  # Stub get_available_sheets() so the "sheet exists" check passes for a
  # sheet the file genuinely doesn't have, isolating this function's own
  # error-catch branch (around the real read_excel() call) from the
  # is.null(available_sheets) branch covered above.
  fixture_file <- get_fixture_path("test_cwt_data.xlsx")
  skip_if_not(file.exists(fixture_file), "Fixture file not found. Run create_fixtures.R first.")

  original_fn <- get_available_sheets
  target_env <- environment(get_available_sheets)
  assign("get_available_sheets", function(file_path) c("28-DAY FDS", "Phantom Sheet"), envir = target_env)
  on.exit(assign("get_available_sheets", original_fn, envir = target_env))

  expect_error(
    read_sheets_from_files(fixture_file, "Phantom Sheet", require_all_sheets = TRUE),
    regexp = "Error reading sheet 'Phantom Sheet' from"
  )
})

test_that("read_sheets_from_files() warns on the underlying read error when require_all=FALSE, keeping good sheets", {
  fixture_file <- get_fixture_path("test_cwt_data.xlsx")
  skip_if_not(file.exists(fixture_file), "Fixture file not found. Run create_fixtures.R first.")

  original_fn <- get_available_sheets
  target_env <- environment(get_available_sheets)
  assign("get_available_sheets", function(file_path) c("28-DAY FDS", "Phantom Sheet"), envir = target_env)
  on.exit(assign("get_available_sheets", original_fn, envir = target_env))

  warnings_seen <- testthat::capture_warnings(
    result <- read_sheets_from_files(
      fixture_file, c("28-DAY FDS", "Phantom Sheet"), require_all_sheets = FALSE
    )
  )
  all_warnings <- paste(warnings_seen, collapse = "\n")

  expect_match(all_warnings, "Error reading sheet 'Phantom Sheet' from")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

# -----------------------------------------------------------------------------
# get_alliance_files() tests
# -----------------------------------------------------------------------------

test_that("get_alliance_files() returns empty with warning for no matches", {
  # Using a temp directory with no matching files
  temp_dir <- tempdir()

  expect_warning(
    result <- get_alliance_files(temp_dir, "Nonexistent Alliance"),
    regexp = "No files found for alliance"
  )

  expect_equal(length(result), 0)
})

# -----------------------------------------------------------------------------
# read_csv_files() tests
# -----------------------------------------------------------------------------

test_that("read_csv_files() errors on non-existent directory", {
  expect_error(
    read_csv_files("/nonexistent/path"),
    regexp = "Directory does not exist"
  )
})

test_that("read_csv_files() errors on directory with no CSV files", {
  temp_dir <- tempdir()
  # Create a subdirectory with no CSV files
  empty_dir <- file.path(temp_dir, "empty_test_dir")
  dir.create(empty_dir, showWarnings = FALSE)

  expect_error(
    read_csv_files(empty_dir),
    regexp = "No CSV files matching pattern"
  )
})

test_that("read_csv_files() reads CSV file correctly", {
  fixture_dir <- file.path(project_root, "tests", "testthat", "fixtures")
  csv_file <- file.path(fixture_dir, "test_cwt.csv")
  skip_if_not(file.exists(csv_file), "Fixture CSV not found. Run create_fixtures.R first.")

  result <- read_csv_files(fixture_dir, pattern = "test_cwt\\.csv$")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("ORG.CODE" %in% names(result))
})

test_that("read_csv_files() applies column renames correctly", {
  fixture_dir <- file.path(project_root, "tests", "testthat", "fixtures")
  csv_file <- file.path(fixture_dir, "test_cwt.csv")
  skip_if_not(file.exists(csv_file), "Fixture CSV not found. Run create_fixtures.R first.")

  result <- read_csv_files(
    fixture_dir,
    pattern = "test_cwt\\.csv$",
    col_renames = c("TOTAL.TREATED" = "TOTAL_NEW_NAME")
  )

  expect_true("TOTAL_NEW_NAME" %in% names(result))
  expect_false("TOTAL.TREATED" %in% names(result))
})

test_that("read_csv_files() stops on a header-only (0-row) file when require_all=TRUE", {
  dir <- withr::local_tempdir()
  writeLines("ORG.CODE,VALUE", file.path(dir, "empty.csv"))

  expect_error(
    read_csv_files(dir, require_all = TRUE),
    regexp = "File is empty"
  )
})

test_that("read_csv_files() warns and skips a header-only file when require_all=FALSE, keeping good files", {
  dir <- withr::local_tempdir()
  writeLines("ORG.CODE,VALUE", file.path(dir, "empty.csv"))
  write.csv(data.frame(ORG.CODE = "RRK", VALUE = 1), file.path(dir, "good.csv"), row.names = FALSE)

  warnings_seen <- testthat::capture_warnings(
    result <- read_csv_files(dir, require_all = FALSE)
  )
  all_warnings <- paste(warnings_seen, collapse = "\n")

  expect_match(all_warnings, "File is empty")
  expect_match(all_warnings, "CSV read completed with 1 failures out of 2 files")
  expect_equal(nrow(result), 1)
  expect_equal(result$ORG.CODE, "RRK")
})

test_that("read_csv_files() stops with the underlying read error when require_all=TRUE", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "zero_byte.csv"))  # 0 bytes - read.csv() itself errors

  expect_error(
    read_csv_files(dir, require_all = TRUE),
    regexp = "Error reading 'zero_byte\\.csv'"
  )
})

test_that("read_csv_files() warns on an unreadable file when require_all=FALSE and lists it in the failure summary", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "zero_byte.csv"))
  write.csv(data.frame(ORG.CODE = "RRK", VALUE = 1), file.path(dir, "good.csv"), row.names = FALSE)

  warnings_seen <- testthat::capture_warnings(
    result <- read_csv_files(dir, require_all = FALSE)
  )
  all_warnings <- paste(warnings_seen, collapse = "\n")

  expect_match(all_warnings, "Error reading 'zero_byte\\.csv'")
  expect_match(all_warnings, "CSV read completed with 1 failures out of 2 files")
  expect_equal(nrow(result), 1)
})

test_that("read_csv_files() stops listing every failure when nothing could be read at all", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "zero_byte.csv"))

  err <- tryCatch(
    suppressWarnings(read_csv_files(dir, require_all = FALSE)),
    error = function(e) e
  )

  expect_match(conditionMessage(err), "No data was successfully read from any CSV file", fixed = TRUE)
  expect_match(conditionMessage(err), "Failures:\n  - zero_byte.csv", fixed = TRUE)
})

# -----------------------------------------------------------------------------
# read_excel_files() tests
# -----------------------------------------------------------------------------
# Unlike read_sheets_from_files() (tested above against a pre-built file
# list), read_excel_files() discovers files in a directory itself and
# supports a header-row `skip` - mirroring the real read-in scripts
# (e.g. read_in_cwt_public_provider.R's `skip = 14`). These build throwaway
# workbooks with openxlsx rather than reusing the shared fixtures, since
# several scenarios here (an empty-but-headered sheet, a sheet that doesn't
# exist) have no existing fixture to reuse.

#' Write a minimal one-sheet .xlsx fixture for read_excel_files() tests
write_xlsx_fixture <- function(path, sheet_name, df = data.frame(ORG.CODE = "RRK", VALUE = 1)) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, df)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

test_that("read_excel_files() errors on non-existent directory", {
  expect_error(
    read_excel_files("/nonexistent/path", sheet = "Sheet1"),
    regexp = "Directory does not exist"
  )
})

test_that("read_excel_files() errors on directory with no matching Excel files", {
  dir <- withr::local_tempdir()

  expect_error(
    read_excel_files(dir, sheet = "Sheet1"),
    regexp = "No Excel files matching pattern"
  )
})

test_that("read_excel_files() reads and combines matching files, adding SOURCE.FILE", {
  dir <- withr::local_tempdir()
  write_xlsx_fixture(file.path(dir, "a.xlsx"), "Data", data.frame(ORG.CODE = "RRK", VALUE = 1))
  write_xlsx_fixture(file.path(dir, "b.xlsx"), "Data", data.frame(ORG.CODE = "RKB", VALUE = 2))

  result <- read_excel_files(dir, sheet = "Data")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_setequal(result$SOURCE.FILE, c("a.xlsx", "b.xlsx"))
})

test_that("read_excel_files() applies skip to the header row", {
  dir <- withr::local_tempdir()
  # Two metadata rows above the real header, like the real CWT workbooks
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Data")
  openxlsx::writeData(wb, "Data", data.frame(junk = "metadata"), startRow = 1, colNames = FALSE)
  openxlsx::writeData(wb, "Data", data.frame(ORG.CODE = "RRK", VALUE = 1), startRow = 3)
  openxlsx::saveWorkbook(wb, file.path(dir, "a.xlsx"), overwrite = TRUE)

  result <- read_excel_files(dir, sheet = "Data", skip = 2)

  expect_true("ORG.CODE" %in% names(result))
  expect_equal(result$ORG.CODE, "RRK")
})

test_that("read_excel_files() stops when the sheet doesn't exist and require_all=TRUE", {
  dir <- withr::local_tempdir()
  write_xlsx_fixture(file.path(dir, "a.xlsx"), "Data")

  expect_error(
    read_excel_files(dir, sheet = "Not A Real Sheet", require_all = TRUE),
    regexp = "Sheet 'Not A Real Sheet' not found in 'a\\.xlsx'"
  )
})

test_that("read_excel_files() warns and skips a file with no matching sheet when require_all=FALSE", {
  dir <- withr::local_tempdir()
  write_xlsx_fixture(file.path(dir, "good.xlsx"), "Data", data.frame(ORG.CODE = "RRK", VALUE = 1))
  write_xlsx_fixture(file.path(dir, "bad.xlsx"), "Other Sheet")

  warnings_seen <- testthat::capture_warnings(
    result <- read_excel_files(dir, sheet = "Data", require_all = FALSE)
  )
  all_warnings <- paste(warnings_seen, collapse = "\n")

  expect_match(all_warnings, "Sheet 'Data' not found in 'bad\\.xlsx'")
  expect_match(all_warnings, "Excel read completed with 1 failures out of 2 files")
  expect_equal(nrow(result), 1)
  expect_equal(result$ORG.CODE, "RRK")
})

test_that("read_excel_files() stops when the matched sheet is empty and require_all=TRUE", {
  dir <- withr::local_tempdir()
  # A sheet with a header row but zero data rows
  write_xlsx_fixture(file.path(dir, "a.xlsx"), "Data", data.frame(ORG.CODE = character(0), VALUE = numeric(0)))

  expect_error(
    read_excel_files(dir, sheet = "Data", require_all = TRUE),
    regexp = "Sheet is empty: a\\.xlsx"
  )
})

test_that("read_excel_files() warns and skips an empty sheet when require_all=FALSE, keeping good files", {
  dir <- withr::local_tempdir()
  write_xlsx_fixture(file.path(dir, "empty.xlsx"), "Data", data.frame(ORG.CODE = character(0), VALUE = numeric(0)))
  write_xlsx_fixture(file.path(dir, "good.xlsx"), "Data", data.frame(ORG.CODE = "RRK", VALUE = 1))

  warnings_seen <- testthat::capture_warnings(
    result <- read_excel_files(dir, sheet = "Data", require_all = FALSE)
  )
  all_warnings <- paste(warnings_seen, collapse = "\n")

  expect_match(all_warnings, "Sheet is empty: empty\\.xlsx")
  expect_match(all_warnings, "Excel read completed with 1 failures out of 2 files")
  expect_equal(nrow(result), 1)
})

test_that("read_excel_files() stops with the underlying read error when require_all=TRUE", {
  # get_available_sheets() stubbed to report a sheet the real file doesn't
  # have, so the "sheet exists" check (which calls the real get_available_sheets()
  # too) passes but readxl::read_excel() itself genuinely fails - isolating
  # read_excel_files()'s own error-catch branch from get_available_sheets()'s.
  dir <- withr::local_tempdir()
  write_xlsx_fixture(file.path(dir, "a.xlsx"), "Data")

  original_fn <- get_available_sheets
  target_env <- environment(get_available_sheets)
  assign("get_available_sheets", function(file_path) c("Data", "Phantom Sheet"), envir = target_env)
  on.exit(assign("get_available_sheets", original_fn, envir = target_env))

  expect_error(
    read_excel_files(dir, sheet = "Phantom Sheet", require_all = TRUE),
    regexp = "Error reading 'a\\.xlsx'"
  )
})

test_that("read_excel_files() lists every failure when nothing could be read at all", {
  dir <- withr::local_tempdir()
  write_xlsx_fixture(file.path(dir, "a.xlsx"), "Data")

  original_fn <- get_available_sheets
  target_env <- environment(get_available_sheets)
  assign("get_available_sheets", function(file_path) c("Data", "Phantom Sheet"), envir = target_env)
  on.exit(assign("get_available_sheets", original_fn, envir = target_env))

  err <- tryCatch(
    suppressWarnings(read_excel_files(dir, sheet = "Phantom Sheet", require_all = FALSE)),
    error = function(e) e
  )

  expect_match(conditionMessage(err), "No data was successfully read from any Excel file", fixed = TRUE)
  expect_match(conditionMessage(err), "Failures:\n  - a.xlsx", fixed = TRUE)
})

# -----------------------------------------------------------------------------
# validation_utils.R tests
# -----------------------------------------------------------------------------

test_that("validate_df_not_empty() passes for non-empty dataframe", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  result <- validate_df_not_empty(df, "test context")

  expect_equal(result, df)
})

test_that("validate_df_not_empty() errors on empty dataframe", {
  df <- data.frame()

  expect_error(
    validate_df_not_empty(df, "test context"),
    regexp = "Empty dataframe \\(0 rows\\) in test context"
  )
})

test_that("validate_df_not_empty() errors on NULL", {
  expect_error(
    validate_df_not_empty(NULL, "test context"),
    regexp = "NULL dataframe in test context"
  )
})

test_that("validate_df_not_empty_warn() returns NULL with warning for empty dataframe", {
  df <- data.frame()

  expect_warning(
    result <- validate_df_not_empty_warn(df, "test context"),
    regexp = "Empty dataframe"
  )

  expect_null(result)
})

test_that("validate_df_not_empty_warn() returns NULL with warning for NULL input", {
  expect_warning(
    result <- validate_df_not_empty_warn(NULL, "test context"),
    regexp = "NULL dataframe in test context"
  )

  expect_null(result)
})

test_that("validate_filter_result() passes for non-empty result", {
  df <- data.frame(x = 1:3)
  result <- validate_filter_result(df, "x", 1, context = "test")

  expect_equal(result, df)
})

test_that("validate_filter_result() errors on empty result with context", {
  df <- data.frame()
  original <- data.frame(x = c("a", "b", "c"))

  expect_error(
    validate_filter_result(df, "x", "d", original, "test filter"),
    regexp = "Filter returned no results"
  )
})

test_that("validate_filter_result() formats a multi-value filter as a bracketed list", {
  df <- data.frame()
  original <- data.frame(x = c("a", "b", "c"))

  expect_error(
    validate_filter_result(df, "x", c("d", "e"), original, "test filter"),
    regexp = "Filter: x == \\[d, e\\]"
  )
})

test_that("validate_filter_result() truncates available values to the first 20 when there are more", {
  df <- data.frame()
  original <- data.frame(x = as.character(1:25))

  expect_error(
    validate_filter_result(df, "x", "nope", original, "test filter"),
    regexp = "Available values \\(first 20\\): 1, 2, 3.*\\.\\.\\."
  )
})

# -----------------------------------------------------------------------------
# validate_sheet_exists() tests
# -----------------------------------------------------------------------------

test_that("validate_sheet_exists() returns TRUE when the sheet is present", {
  fixture <- get_fixture_path("test_cwt_data.xlsx")
  skip_if_not(file.exists(fixture), "test_cwt_data.xlsx fixture not available")

  expect_true(validate_sheet_exists(fixture, "28-DAY FDS"))
})

test_that("validate_sheet_exists() returns FALSE when the sheet is absent", {
  fixture <- get_fixture_path("test_cwt_data.xlsx")
  skip_if_not(file.exists(fixture), "test_cwt_data.xlsx fixture not available")

  expect_false(validate_sheet_exists(fixture, "Not A Real Sheet"))
})

test_that("validate_sheet_exists() warns and returns FALSE when the file can't be read", {
  expect_warning(
    result <- validate_sheet_exists("does_not_exist_at_all.xlsx", "28-DAY FDS"),
    regexp = "Could not read sheets from file"
  )

  expect_false(result)
})

# -----------------------------------------------------------------------------
# get_available_sheets() tests
# -----------------------------------------------------------------------------

test_that("get_available_sheets() lists every sheet in the workbook", {
  fixture <- get_fixture_path("test_cwt_data.xlsx")
  skip_if_not(file.exists(fixture), "test_cwt_data.xlsx fixture not available")

  result <- get_available_sheets(fixture)

  expect_true("28-DAY FDS" %in% result)
})

test_that("get_available_sheets() warns and returns NULL when the file can't be read", {
  expect_warning(
    result <- get_available_sheets("does_not_exist_at_all.xlsx"),
    regexp = "Could not read sheets from file"
  )

  expect_null(result)
})

test_that("assert() passes for TRUE condition", {
  expect_invisible(assert(TRUE, "Should not see this"))
})

test_that("assert() errors for FALSE condition", {
  expect_error(
    assert(FALSE, "Custom error message"),
    regexp = "Custom error message"
  )
})

test_that("safe_execute() returns result on success", {
  result <- safe_execute(1 + 1, "addition")
  expect_equal(result, 2)
})

test_that("safe_execute() errors with context on failure (on_error='stop')", {
  expect_error(
    safe_execute(stop("Original error"), "test operation"),
    regexp = "Error in test operation: Original error"
  )
})

test_that("safe_execute() returns NULL on failure (on_error='null')", {
  result <- safe_execute(stop("Original error"), "test operation", on_error = "null")
  expect_null(result)
})

test_that("safe_execute() warns and returns NULL (on_error='warn')", {
  expect_warning(
    result <- safe_execute(stop("Original error"), "test operation", on_error = "warn"),
    regexp = "Error in test operation"
  )
  expect_null(result)
})

test_that("validate_files_exist() passes for existing files", {
  tmp <- tempfile()
  file.create(tmp)
  on.exit(unlink(tmp))
  result <- validate_files_exist(tmp, "temp file")
  expect_equal(result, tmp)
})

test_that("validate_files_exist() errors on missing files", {
  expect_error(
    validate_files_exist(c("existing.txt", "missing.txt"), "test files"),
    regexp = "Missing files for test files"
  )
})

test_that("validate_files_exist() errors on empty input", {
  expect_error(
    validate_files_exist(character(0), "test context"),
    regexp = "No files provided for test context"
  )
})


# =============================================================================
# clean_lwbc_monthly_df() tests
# =============================================================================

test_that("clean_lwbc_monthly_df() removes YEAR column", {
  df <- data.frame(
    YEAR = 2024,
    MONTH = as.Date("2024-01-01"),
    `Cancer Alliance` = "WMCA",
    `Total Count` = 10,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- clean_lwbc_monthly_df(df)

  expect_false("YEAR" %in% names(result))
})

test_that("clean_lwbc_monthly_df() renames MONTH column to Date", {
  df <- data.frame(
    YEAR = 2024,
    MONTH = as.Date("2024-01-01"),
    `Cancer Alliance` = "WMCA",
    `Total Count` = 10,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- clean_lwbc_monthly_df(df)

  expect_false("MONTH" %in% names(result))
  expect_true("Date" %in% names(result))
})

test_that("clean_lwbc_monthly_df() Date column is a Date object", {
  df <- data.frame(
    YEAR = 2024,
    MONTH = as.Date("2024-06-01"),
    `Cancer Alliance` = "WMCA",
    `Total Count` = 5,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- clean_lwbc_monthly_df(df)

  expect_s3_class(result$Date, "Date")
})

test_that("clean_lwbc_monthly_df() filters out rows with NA MONTH", {
  df <- data.frame(
    YEAR = c(2024, 2024),
    MONTH = c(as.Date("2024-01-01"), NA),
    `Cancer Alliance` = c("WMCA", "WMCA"),
    `Total Count` = c(10, 5),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- clean_lwbc_monthly_df(df)

  expect_equal(nrow(result), 1)
})

test_that("clean_lwbc_monthly_df() filters out rows with NA Cancer Alliance", {
  df <- data.frame(
    YEAR = c(2024, 2024),
    MONTH = c(as.Date("2024-01-01"), as.Date("2024-02-01")),
    `Cancer Alliance` = c("WMCA", NA),
    `Total Count` = c(10, 5),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- clean_lwbc_monthly_df(df)

  expect_equal(nrow(result), 1)
})

test_that("clean_lwbc_monthly_df() replaces NA Total Count with 0", {
  df <- data.frame(
    YEAR = 2024,
    MONTH = as.Date("2024-01-01"),
    `Cancer Alliance` = "WMCA",
    `Total Count` = NA_real_,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- clean_lwbc_monthly_df(df)

  expect_equal(result$`Total Count`, 0)
})

test_that("clean_lwbc_monthly_df() preserves rows where all fields are valid", {
  df <- data.frame(
    YEAR = c(2024, 2024, 2024),
    MONTH = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    `Cancer Alliance` = c("WMCA", "WMCA", "WMCA"),
    `Total Count` = c(10, 20, 30),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- clean_lwbc_monthly_df(df)

  expect_equal(nrow(result), 3)
})
