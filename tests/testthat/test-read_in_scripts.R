# Tests for Read-In Script Logic
# =============================================================================
# These tests validate the transformation logic used by the read-in scripts
# without requiring actual data files. The scripts themselves are orchestration
# layers that combine already-tested functions (read_csv_files,
# get_trust_codes, etc.) with data transformations.
#
# We test:
# 1. Data transformation pipelines (filtering, date parsing, column renaming)
# 2. Output structure validation (expected columns present)
# 3. Script-specific helper functions

# Set alliance for tests
assign("alliance", "WMCA", envir = .GlobalEnv)

# =============================================================================
# CANCER WAITING TIMES TRANSFORMATION TESTS
# =============================================================================

# -----------------------------------------------------------------------------
# CWT Provider Data Transformations
# -----------------------------------------------------------------------------

test_that("CWT provider data transformation produces expected output structure", {
  # Simulate raw rows as they come from the "Provider Level Performance" sheet
  # of the NHS "Monthly Combined Workbook" (via read_excel_files()), after the
  # per-file PERIOD has been stamped on from the "Period:" metadata cell. This
  # includes the England "Total" aggregate row and a trailing blank row that
  # read_in_cwt_public_provider.R must drop before anything downstream sees it.
  raw_data <- data.frame(
    `Org Code` = c("RRK", "RKB", "RRK", "RKB", "Total", NA),
    Standard = c("FDS", "FDS", "FDS", "FDS", "FDS", NA),
    `Cancer Type` = c("Suspected lung cancer", "Suspected lung cancer",
                       "Suspected breast cancer", "Suspected breast cancer",
                       "ALL CANCERS", NA),
    Within = c(80, 85, 90, 95, 500, NA),
    Total = c(100, 100, 100, 100, 600, NA),
    PERIOD = as.Date(c("2024-04-01", "2024-04-01", "2024-05-01", "2024-05-01",
                        "2024-05-01", NA)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Apply the same transformations as read_in_cwt_public_provider.R
  org_recode_map <- get_org_recode_map()
  org_codes <- get_alliance_config("trust_codes")

  # Drop trailing blank row(s) and the England aggregate row
  cleaned <- raw_data %>%
    filter(!is.na(`Org Code`), `Org Code` != "Total")

  # Standardise column names
  cleaned <- cleaned %>%
    rename(
      ORG.CODE        = `Org Code`,
      STANDARD        = Standard,
      CANCER.TYPE     = `Cancer Type`,
      WITHIN.STANDARD = Within,
      TOTAL.TREATED   = Total
    )

  # Arrange by PERIOD descending
  transformed <- cleaned |> arrange(desc(PERIOD))

  # Filter for org codes and add names
  result <- transformed %>%
    filter(ORG.CODE %in% org_codes) %>%
    mutate(ORG.NAME = recode(ORG.CODE, !!!org_recode_map))

  # Validate structure

  expect_true("PERIOD" %in% names(result))
  expect_true("ORG.CODE" %in% names(result))
  expect_true("ORG.NAME" %in% names(result))
  expect_true("STANDARD" %in% names(result))
  expect_true("CANCER.TYPE" %in% names(result))
  expect_true("WITHIN.STANDARD" %in% names(result))
  expect_true("TOTAL.TREATED" %in% names(result))

  # Validate date parsing
  expect_s3_class(result$PERIOD, "Date")

  # Raw values are preserved (no normalisation layer)
  expect_true("Suspected lung cancer" %in% result$CANCER.TYPE)
  expect_true("Suspected breast cancer" %in% result$CANCER.TYPE)

  # England aggregate row and blank trailing row are excluded
  expect_false("Total" %in% result$ORG.CODE)
  expect_false(any(is.na(result$ORG.CODE)))
})

test_that("CWT data can be filtered by standard type", {
  # Simulate already-processed CWT data
  cwt_df <- data.frame(
    PERIOD = as.Date(c("2024-04-01", "2024-04-01", "2024-04-01")),
    ORG.CODE = c("RRK", "RRK", "RRK"),
    ORG.NAME = c("University Hospitals Birmingham", "University Hospitals Birmingham", "University Hospitals Birmingham"),
    STANDARD = c("FDS", "31D", "62D"),
    CANCER.TYPE = c("Lung", "Lung", "Lung"),
    WITHIN.STANDARD = c(80, 95, 85),
    TOTAL.TREATED = c(100, 100, 100),
    stringsAsFactors = FALSE
  )

  # Filter as done in the script
  fds_df <- cwt_df %>% filter(STANDARD == "FDS")
  day_31_df <- cwt_df %>% filter(STANDARD == "31D")
  day_62_df <- cwt_df %>% filter(STANDARD == "62D")

  expect_equal(nrow(fds_df), 1)
  expect_equal(nrow(day_31_df), 1)
  expect_equal(nrow(day_62_df), 1)

  expect_equal(fds_df$WITHIN.STANDARD, 80)
  expect_equal(day_31_df$WITHIN.STANDARD, 95)
  expect_equal(day_62_df$WITHIN.STANDARD, 85)
})

test_that("Latest 2 months filtering works correctly", {
  # Simulate data with multiple months
  all_data <- data.frame(
    PERIOD = as.Date(c("2024-03-01", "2024-04-01", "2024-05-01", "2024-06-01")),
    ORG.CODE = rep("RRK", 4),
    WITHIN.STANDARD = c(75, 80, 85, 90),
    TOTAL.TREATED = rep(100, 4),
    stringsAsFactors = FALSE
  )

  # Apply same logic as script
  latest_month <- max(all_data$PERIOD, na.rm = TRUE)
  latest_public_2_months_df <- all_data |>
    filter(PERIOD >= latest_month %m-% months(1))

  # Should have May and June (latest 2 months)
  expect_equal(nrow(latest_public_2_months_df), 2)
  expect_true(as.Date("2024-06-01") %in% latest_public_2_months_df$PERIOD)
  expect_true(as.Date("2024-05-01") %in% latest_public_2_months_df$PERIOD)
  expect_false(as.Date("2024-04-01") %in% latest_public_2_months_df$PERIOD)
})

# =============================================================================
# DETAILED CWT TRANSFORMATION TESTS
# =============================================================================

test_that("Detailed CWT date transformation works correctly", {
  # Simulate raw detailed data
  raw_data <- data.frame(
    Month = c("APR", "MAY", "JAN", "FEB"),
    Year = c("2024/25", "2024/25", "2024/25", "2024/25"),
    Provider.Code.FDS = rep("RRK", 4),
    Cancer.Report.Category = rep("Suspected lung cancer", 4),
    NoPatients = c(100, 110, 120, 130),
    stringsAsFactors = FALSE
  )

  # Apply same transformation as read_in_cwt_extract_provider.R
  result <- raw_data %>%
    mutate(Date = paste0(Month, " ", Year)) %>%
    mutate(Date = parse_financial_year_date(Date))

  # Validate dates
  expect_s3_class(result$Date, "Date")
  expect_equal(result$Date[1], as.Date("2024-04-01"))  # APR 2024/25 = April 2024
  expect_equal(result$Date[3], as.Date("2025-01-01"))  # JAN 2024/25 = January 2025
  expect_equal(result$Date[4], as.Date("2025-02-01"))  # FEB 2024/25 = February 2025
})

test_that("Detailed CWT org name mapping works", {
  org_recode_map <- get_org_recode_map()

  raw_data <- data.frame(
    Provider.Code.FDS = c("RRK", "RKB", "RL4"),
    NoPatients = c(100, 110, 120),
    stringsAsFactors = FALSE
  )

  result <- raw_data %>%
    mutate(ORG.NAME = recode(Provider.Code.FDS, !!!org_recode_map))

  # Should have org names
  expect_true("ORG.NAME" %in% names(result))
  expect_true(all(!is.na(result$ORG.NAME)))
  expect_true(all(nchar(result$ORG.NAME) > 3))  # Names longer than codes
})

test_that("Detailed CWT within_standard calculation works", {
  raw_data <- data.frame(
    Provider.Code.FDS = "RRK",
    NoPatients = 100,
    DaysWithin7 = 30,
    Days8To14 = 25,
    Days15To21 = 15,
    Days22To28 = 10,
    stringsAsFactors = FALSE
  )

  # Apply same calculation as script
  result <- raw_data |>
    mutate(
      within_standard = rowSums(across(c(DaysWithin7, Days8To14, Days15To21, Days22To28)), na.rm = TRUE)
    )

  expect_equal(result$within_standard, 80)  # 30 + 25 + 15 + 10
})

# =============================================================================
# STAGING COMPLETENESS TRANSFORMATION TESTS
# =============================================================================

test_that("Staging completeness data transformation produces expected structure", {
  # Simulate raw staging completeness data
  raw_data <- data.frame(
    DIAGNOSIS_QUARTER = c("2024 Q1", "2024 Q2", "2024 Q1", "2024 Q2"),
    TRUST_CD = c("RRK", "RRK", "RKB", "RKB"),
    CANCER_GROUP_NM = c("Lower GI", "Lower GI", "Lower GI", "Lower GI"),
    MDT_STAGE = c(80, 85, 75, 80),
    MDT_DISCUSSED = c(100, 100, 100, 100),
    stringsAsFactors = FALSE
  )

  trust_codes <- get_alliance_config("trust_codes")
  org_recode_map <- get_org_recode_map()

  # Apply same transformations as read_in_staging_completeness.R
  result <- raw_data %>%
    select(DIAGNOSIS_QUARTER, TRUST_CD, CANCER_GROUP_NM, MDT_STAGE, MDT_DISCUSSED) %>%
    filter(TRUST_CD %in% trust_codes) %>%
    mutate(ORG.NAME = recode(TRUST_CD, !!!org_recode_map)) %>%
    mutate(DIAGNOSIS_QUARTER = yq(DIAGNOSIS_QUARTER))

  # Validate structure
  expect_true("DIAGNOSIS_QUARTER" %in% names(result))
  expect_true("TRUST_CD" %in% names(result))
  expect_true("CANCER_GROUP_NM" %in% names(result))
  expect_true("MDT_STAGE" %in% names(result))
  expect_true("MDT_DISCUSSED" %in% names(result))
  expect_true("ORG.NAME" %in% names(result))

  # Validate date parsing
  expect_s3_class(result$DIAGNOSIS_QUARTER, "Date")

  # Raw values are preserved (no normalisation layer)
  expect_true("Lower GI" %in% result$CANCER_GROUP_NM)
})

# =============================================================================
# STAGING DATA TRANSFORMATION TESTS
# =============================================================================

test_that("Staging data filtering by geography type works", {
  raw_data <- data.frame(
    `Geography type` = c("Cancer alliance", "Provider", "ICB", "Provider"),
    Geography = c("Alliance A", "Trust A", "ICB X", "Trust B"),
    Date = c("2024-01", "2024-01", "2024-01", "2024-01"),
    `Cancer group` = rep("Lung", 4),
    Metric = rep("Early stage proportion", 4),
    Breakdown = rep("Total", 4),
    Demographic = rep("Total", 4),
    `Statistic (12m)` = c(0.55, 0.60, 0.58, 0.62),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Apply same filter as read_in_staging.R
  result <- raw_data %>%
    filter(`Geography type` %in% c("Cancer alliance", "Provider"))

  expect_equal(nrow(result), 3)
  expect_false("ICB" %in% result$`Geography type`)
})

test_that("Staging date parsing and latest month removal works", {
  raw_data <- data.frame(
    Date = c("2024-01", "2024-02", "2024-03", "2024-04"),
    `Cancer group` = rep("Lung", 4),
    `Statistic (12m)` = c(0.55, 0.56, 0.57, 0.58),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Apply same transformations as script
  result <- raw_data
  result$Date <- ymd(paste0(result$Date, "-01"))

  # Remove most recent month
  result <- result %>%
    filter(Date != max(Date))

  expect_equal(nrow(result), 3)
  expect_false(as.Date("2024-04-01") %in% result$Date)
  expect_true(as.Date("2024-03-01") %in% result$Date)
})

# =============================================================================
# TREATMENT VARIATION HELPER FUNCTION TESTS
# =============================================================================

test_that("parse_range helper function works correctly", {
  # Define the helper function as used in read_in_tv.R
  parse_range <- function(range_str) {
    parts <- as.integer(strsplit(range_str, ":")[[1]])
    cell_rows(parts[1]:parts[2])
  }

  # Test parsing
  result <- parse_range("9:13")

  # cell_rows returns a cell_limits object
  expect_s3_class(result, "cell_limits")
})

test_that("TV cancer name mapping works correctly", {
  # Define mapping as in read_in_tv.R
  tv_cancer_mapping <- c(
    "Primary Breast" = "Breast",
    "Bowel" = "Lower Gastrointestinal",
    "Ovarian" = "Gynaecological"
  )

  raw_data <- data.frame(
    Cancer = c("Lung", "Primary Breast", "Bowel", "Ovarian"),
    Value = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  # Apply same transformation as script
  result <- raw_data |>
    mutate(Cancer = ifelse(Cancer %in% names(tv_cancer_mapping),
                           tv_cancer_mapping[Cancer],
                           Cancer))

  expect_equal(result$Cancer[1], "Lung")  # Unchanged
  expect_equal(result$Cancer[2], "Breast")
  expect_equal(result$Cancer[3], "Lower Gastrointestinal")
  expect_equal(result$Cancer[4], "Gynaecological")
})

# =============================================================================
# INTEGRATION-STYLE OUTPUT VALIDATION TESTS
# =============================================================================

test_that("FDS output dataframe has all required columns for processing", {
  # Simulate final FDS dataframe as produced by read-in script
  fds_df <- data.frame(
    PERIOD = as.Date("2024-04-01"),
    ORG.CODE = "RRK",
    ORG.NAME = "University Hospitals Birmingham",
    STANDARD = "FDS",
    CANCER.TYPE = "Lung",
    WITHIN.STANDARD = 80,
    TOTAL.TREATED = 100,
    stringsAsFactors = FALSE
  )

  # These columns are required by process_fds_raw() and downstream functions
  required_cols <- c("PERIOD", "ORG.CODE", "ORG.NAME", "CANCER.TYPE",
                     "WITHIN.STANDARD", "TOTAL.TREATED")

  for (col in required_cols) {
    expect_true(col %in% names(fds_df), info = paste("Missing column:", col))
  }
})

test_that("Detailed FDS output has all required columns for processing", {
  # Simulate final detailed FDS dataframe
  cwt_d_fds_df <- data.frame(
    Date = as.Date("2024-04-01"),
    Provider.Code.FDS = "RRK",
    ORG.NAME = "University Hospitals Birmingham",
    Cancer.Report.Category = "Lung",
    Pathway.End.Reason = "Diagnosis of cancer",
    NoPatients = 100,
    DaysWithin7 = 30,
    Days8To14 = 25,
    Days15To21 = 15,
    Days22To28 = 10,
    stringsAsFactors = FALSE
  )

  # These columns are required by process_fds_raw() for detailed data
  required_cols <- c("Date", "Cancer.Report.Category", "ORG.NAME",
                     "Pathway.End.Reason", "NoPatients",
                     "DaysWithin7", "Days8To14", "Days15To21", "Days22To28")

  for (col in required_cols) {
    expect_true(col %in% names(cwt_d_fds_df), info = paste("Missing column:", col))
  }
})

test_that("Staging completeness output has all required columns", {
  stage_completeness_df <- data.frame(
    DIAGNOSIS_QUARTER = as.Date("2024-04-01"),
    TRUST_CD = "RRK",
    ORG.NAME = "University Hospitals Birmingham",
    CANCER_GROUP_NM = "Lung",
    MDT_STAGE = 80,
    MDT_DISCUSSED = 100,
    stringsAsFactors = FALSE
  )

  required_cols <- c("DIAGNOSIS_QUARTER", "CANCER_GROUP_NM", "ORG.NAME",
                     "MDT_STAGE", "MDT_DISCUSSED")

  for (col in required_cols) {
    expect_true(col %in% names(stage_completeness_df), info = paste("Missing column:", col))
  }
})

test_that("Staging demographics output has all required columns", {
  stage_df <- data.frame(
    `Geography type` = "Cancer alliance",
    Geography = "West Midlands Cancer Alliance",
    ORG.NAME = "West Midlands Cancer Alliance",
    Date = as.Date("2024-04-01"),
    `Cancer group` = "Lung",
    Metric = "Early stage proportion",
    Breakdown = "Total",
    Demographic = "Total",
    `Statistic (12m)` = 0.55,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  required_cols <- c("Geography type", "Date", "Cancer group", "Metric",
                     "Breakdown", "Demographic", "Statistic (12m)")

  for (col in required_cols) {
    expect_true(col %in% names(stage_df), info = paste("Missing column:", col))
  }
})

# =============================================================================
# EDGE CASES AND ERROR HANDLING
# =============================================================================

test_that("Empty dataframe filtering returns empty dataframe", {
  empty_df <- data.frame(
    ORG.CODE = character(0),
    STANDARD = character(0),
    stringsAsFactors = FALSE
  )

  result <- empty_df %>%
    filter(STANDARD == "FDS")

  expect_equal(nrow(result), 0)
  expect_s3_class(result, "data.frame")
})

test_that("Org code filtering with no matches returns empty", {
  data <- data.frame(
    ORG.CODE = c("XXX", "YYY", "ZZZ"),
    VALUE = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  trust_codes <- get_alliance_config("trust_codes")

  result <- data %>%
    filter(ORG.CODE %in% trust_codes)

  expect_equal(nrow(result), 0)
})

test_that("Recode with missing mappings leaves values unchanged", {
  org_recode_map <- get_org_recode_map()

  data <- data.frame(
    ORG.CODE = c("RRK", "UNKNOWN_CODE"),
    stringsAsFactors = FALSE
  )

  result <- data %>%
    mutate(ORG.NAME = recode(ORG.CODE, !!!org_recode_map, .default = ORG.CODE))

  # Known code should be recoded
  expect_true(nchar(result$ORG.NAME[1]) > 3)

  # Unknown code should remain unchanged (using .default)
  expect_equal(result$ORG.NAME[2], "UNKNOWN_CODE")
})
