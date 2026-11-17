# Tests for tumour_processing.R functions
# =============================================================================
# These tests use the fixture factory in fixtures/create_processing_fixtures.R
# to create realistic test data for the processing functions.

# Set alliance for tests that use alliance accessor functions
assign("alliance", "WMCA", envir = .GlobalEnv)

# =============================================================================
# FDS PROCESSING TESTS
# =============================================================================

# -----------------------------------------------------------------------------
# process_fds_raw() tests
# -----------------------------------------------------------------------------

test_that("process_fds_raw() returns list with expected components", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")

  result <- process_fds_raw(global_data, config)

  expect_type(result, "list")
  expect_true("provider" %in% names(result))
  expect_true("provider_comm" %in% names(result))
  expect_true("detailed" %in% names(result))
  expect_true("detailed_comm" %in% names(result))
})

test_that("process_fds_raw() filters data correctly", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")

  result <- process_fds_raw(global_data, config)

  # provider is always NULL now (derived downstream, not filtered here)
  expect_null(result$provider)

  # All detailed rows should have Lung cancer type
  expect_true(all(result$detailed$Cancer.Report.Category == "Lung"))
})

test_that("process_fds_raw() filters detailed data on cwt_fds_detailed, independent of cwt_fds_public", {
  public_name <- "Suspected cancer - non-specific symptoms"
  detailed_name <- "Suspected cancer - referral to non-specific symptom clinic"

  config <- create_mock_config(
    "Non-specific Symptoms",
    cwt_fds_public = public_name, cwt_fds_detailed = detailed_name
  )

  global_data <- create_mock_global_data("Lung")
  global_data$cwt_d_fds_df <- create_fds_detailed_fixture(detailed_name)
  global_data$cwt_d_fds_comm_df <- create_fds_detailed_fixture(detailed_name)

  # The detailed filter must hit the detailed dataset's value; a mismatch
  # empties the filter and validate_filter_result() stops. cwt_fds_public is
  # not read by process_fds_raw() at all any more (only process_benchmarking()
  # reads it), so it plays no part here.
  expect_no_error(result <- process_fds_raw(global_data, config))

  expect_true(all(result$detailed$Cancer.Report.Category == detailed_name))
  expect_true(all(result$detailed_comm$Cancer.Report.Category == detailed_name))
})

test_that("process_fds_raw() returns dataframes with expected columns", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")

  result <- process_fds_raw(global_data, config)

  # Detailed should have pathway columns
  expect_true("Pathway.End.Reason" %in% names(result$detailed))
  expect_true("NoPatients" %in% names(result$detailed))
  expect_true("Cancer.Report.Category" %in% names(result$detailed))
  expect_null(result$provider)
})

test_that("process_fds_raw() reads the public extract when cwt_fds_detailed is null", {
  config <- create_mock_config("Lung", cwt_fds_detailed = NULL, cwt_fds_public = "Suspected lung cancer")
  global_data <- create_mock_global_data("Suspected lung cancer")

  result <- process_fds_raw(global_data, config)

  expect_null(result$detailed)
  expect_null(result$detailed_comm)
  expect_true(all(result$provider$CANCER.TYPE == "Suspected lung cancer"))
  expect_true(all(result$provider_comm$`Cancer Type` == "Suspected lung cancer"))
})

# -----------------------------------------------------------------------------
# process_fds_summary() tests
# -----------------------------------------------------------------------------

test_that("process_fds_summary() aggregates data correctly", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  fds_raw <- process_fds_raw(global_data, config)

  result <- process_fds_summary(fds_raw)

  expect_type(result, "list")
  expect_true("provider" %in% names(result))

  # Should have single summary row (latest period)
  expect_equal(nrow(result$provider), 1)

  # Should have compliance column
  expect_true("COMPLIANCE" %in% names(result$provider))
})

test_that("process_fds_summary() calculates compliance correctly", {
  # provider is always derived from detailed now (process_fds_raw() never
  # populates it directly) — supply detailed rows instead of a pre-aggregated
  # provider dataframe.
  fds_raw <- list(
    detailed = data.frame(
      Date = as.Date(c("2024-04-01", "2024-04-01", "2024-05-01", "2024-05-01")),
      Provider.Code.FDS = c("RRK", "RKB", "RRK", "RKB"),
      DaysWithin7 = c(80, 90, 85, 95),
      Days8To14 = 0, Days15To21 = 0, Days22To28 = 0,
      NoPatients = c(100, 100, 100, 100)
    )
  )

  result <- process_fds_summary(fds_raw)

  # Latest period is 2024-05-01, total within = 85 + 95 = 180, total treated = 200
  # Compliance = (180/200) * 100 = 90%
  expect_equal(result$provider$COMPLIANCE, 90.0)
})

test_that("process_fds_summary() calculates patients above target", {
  fds_raw <- list(
    detailed = data.frame(
      Date = as.Date("2024-05-01"),
      Provider.Code.FDS = "RRK",
      DaysWithin7 = 85,
      Days8To14 = 0, Days15To21 = 0, Days22To28 = 0,
      NoPatients = 100
    )
  )

  result <- process_fds_summary(fds_raw)

  # FDS target is 80%, so 80 patients needed
  # 85 - 80 = 5 above target
  expect_equal(result$provider$Patients_Above_FDS_Target, 5)
})

# -----------------------------------------------------------------------------
# process_fds_rule_in() tests
# -----------------------------------------------------------------------------

test_that("process_fds_rule_in() returns expected components", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  fds_raw <- process_fds_raw(global_data, config)

  result <- process_fds_rule_in(fds_raw)

  expect_type(result, "list")
  expect_true("raw" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("trust" %in% names(result))
  expect_true("trusts_above" %in% names(result))
  expect_true("trusts_below" %in% names(result))
  expect_true("count_above" %in% names(result))
  expect_true("count_below" %in% names(result))
})

test_that("process_fds_rule_in() filters to diagnosis of cancer only", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  fds_raw <- process_fds_raw(global_data, config)

  result <- process_fds_rule_in(fds_raw)

  # Raw data should only have "Diagnosis of cancer" pathway
  expect_true(all(result$raw$Pathway.End.Reason == "Diagnosis of cancer"))
})

test_that("process_fds_rule_in() counts trusts above/below correctly", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  fds_raw <- process_fds_raw(global_data, config)

  result <- process_fds_rule_in(fds_raw)

  # Count should match dataframe row counts
  expect_equal(result$count_above, nrow(result$trusts_above))
  expect_equal(result$count_below, nrow(result$trusts_below))

  # Total should equal number of trusts
  expect_equal(
    result$count_above + result$count_below,
    nrow(result$trust)
  )
})

test_that("process_fds_rule_in() and process_fds_rule_in_comm() return NULL when public-sourced", {
  config <- create_mock_config("Lung", cwt_fds_detailed = NULL, cwt_fds_public = "Lung")
  global_data <- create_mock_global_data("Lung")
  fds_raw <- process_fds_raw(global_data, config)

  expect_null(process_fds_rule_in(fds_raw))
  expect_null(process_fds_rule_in_comm(fds_raw))
})

# -----------------------------------------------------------------------------
# process_fds_plot() tests
# -----------------------------------------------------------------------------

test_that("process_fds_plot() returns expected components", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  fds_raw <- process_fds_raw(global_data, config)

  result <- process_fds_plot(fds_raw, config)

  expect_type(result, "list")
  expect_true("total_treated" %in% names(result))
  expect_true("compliance" %in% names(result))
  expect_true("trusts_above" %in% names(result))
  expect_true("trusts_below" %in% names(result))
  expect_true("riro" %in% names(result))
  expect_true("riro_bar" %in% names(result))
})

test_that("process_fds_plot() calculates compliance percentage", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  fds_raw <- process_fds_raw(global_data, config)

  result <- process_fds_plot(fds_raw, config)

  expect_true("COMPLIANCE (%)" %in% names(result$compliance))

  # Compliance should be between 0 and 100
  expect_true(all(result$compliance$`COMPLIANCE (%)` >= 0))
  expect_true(all(result$compliance$`COMPLIANCE (%)` <= 100))
})

test_that("process_fds_plot() adds label for latest period only", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  fds_raw <- process_fds_raw(global_data, config)

  result <- process_fds_plot(fds_raw, config)

  latest_period <- max(result$compliance$PERIOD)

  # Only latest period should have labels
  labels_latest <- result$compliance$Label[result$compliance$PERIOD == latest_period]
  labels_earlier <- result$compliance$Label[result$compliance$PERIOD < latest_period]

  expect_true(all(!is.na(labels_latest)))
  expect_true(all(is.na(labels_earlier)))
})

test_that("process_fds_plot() omits riro/riro_bar when public-sourced, but still builds the rest", {
  config <- create_mock_config("Lung", cwt_fds_detailed = NULL, cwt_fds_public = "Lung")
  global_data <- create_mock_global_data("Lung")
  fds_raw <- process_fds_raw(global_data, config)

  result <- process_fds_plot(fds_raw, config)

  expect_null(result$riro)
  expect_null(result$riro_bar)
  expect_false(is.null(result$compliance))
  expect_false(is.null(result$total_treated))
})

# -----------------------------------------------------------------------------
# process_fds_table() tests
# -----------------------------------------------------------------------------

test_that("process_fds_table() returns table and latest_date", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  fds_raw <- process_fds_raw(global_data, config)
  fds_plot <- process_fds_plot(fds_raw, config)

  result <- process_fds_table(fds_plot, config)

  expect_type(result, "list")
  expect_true("table" %in% names(result))
  expect_true("latest_date" %in% names(result))
})

test_that("process_fds_table() has renamed columns", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  fds_raw <- process_fds_raw(global_data, config)
  fds_plot <- process_fds_plot(fds_raw, config)

  result <- process_fds_table(fds_plot, config)

  expect_true("Latest FDS Compliance (%)" %in% names(result$table))
  expect_true("ORG.NAME" %in% names(result$table))
})

# -----------------------------------------------------------------------------
# process_fds_correlation() tests
# -----------------------------------------------------------------------------

test_that("process_fds_correlation() returns correlation dataframes", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  fds_raw <- process_fds_raw(global_data, config)
  fds_plot <- process_fds_plot(fds_raw, config)

  result <- process_fds_correlation(fds_raw, fds_plot)

  expect_type(result, "list")
  expect_true("overall" %in% names(result))
  expect_true("rule_out" %in% names(result))
  expect_true("rule_in" %in% names(result))
})

test_that("process_fds_correlation() overall has FDS compliance column", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  fds_raw <- process_fds_raw(global_data, config)
  fds_plot <- process_fds_plot(fds_raw, config)

  result <- process_fds_correlation(fds_raw, fds_plot)

  expect_true("COMPLIANCE FDS (%)" %in% names(result$overall))
})

test_that("process_fds_correlation() returns NULL rule_in/rule_out for a public-sourced tumour (no detailed FDS)", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  fds_raw <- process_fds_raw(global_data, config)
  fds_plot <- process_fds_plot(fds_raw, config)
  # `fds_raw$detailed <- NULL` would REMOVE the element, and $'s partial
  # matching would then quietly resolve fds_raw$detailed to detailed_comm's
  # value instead of NULL. `[<-` keeps the key present with a real NULL.
  fds_raw["detailed"] <- list(NULL)

  result <- process_fds_correlation(fds_raw, fds_plot)

  expect_null(result$rule_out)
  expect_null(result$rule_in)
  expect_true("COMPLIANCE FDS (%)" %in% names(result$overall))
})


# =============================================================================
# 31-DAY PROCESSING TESTS
# =============================================================================

# -----------------------------------------------------------------------------
# process_31_day_raw() tests
# -----------------------------------------------------------------------------

test_that("process_31_day_raw() returns list with public and detailed", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")

  result <- process_31_day_raw(global_data, config)

  expect_type(result, "list")
  expect_true("public" %in% names(result))
  expect_true("detailed" %in% names(result))
})

test_that("process_31_day_raw() filters detailed data correctly (public is always NULL when detailed-sourced)", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")

  result <- process_31_day_raw(global_data, config)

  expect_null(result$public)
  expect_true(all(result$detailed$CancerReportCategory == "Lung"))
})

test_that("process_31_day_raw() reads the public extract when cwt_31day_detailed is null", {
  config <- create_mock_config("Lung", cwt_31day_detailed = NULL, cwt_31day_public = "Lung")
  global_data <- create_mock_global_data("Lung")

  result <- process_31_day_raw(global_data, config)

  expect_null(result$detailed)
  expect_true(all(result$public$CANCER.TYPE == "Lung"))
})

# -----------------------------------------------------------------------------
# derive_31_day_from_detailed() tests
# -----------------------------------------------------------------------------

test_that("derive_31_day_from_detailed() returns NULL for NULL input", {
  result <- derive_31_day_from_detailed(list(detailed = NULL))

  expect_null(result)
})

test_that("derive_62_day_comm_from_detailed() returns NULL for NULL input", {
  # validate_df_not_empty_warn() returns NULL when a tumour has no rows in the
  # detailed extract, so this must not be passed to filter()
  expect_null(derive_62_day_comm_from_detailed(NULL))
})

test_that("derive_31_day_from_detailed() renames columns correctly", {
  raw_31 <- list(
    detailed = create_31_day_detailed_fixture("Brain")
  )

  result <- derive_31_day_from_detailed(raw_31)

  expect_true("PERIOD" %in% names(result))
  expect_true("ORG.CODE" %in% names(result))
  expect_true("ORG.NAME" %in% names(result))
  expect_true("TOTAL.TREATED" %in% names(result))
  expect_true("WITHIN.STANDARD" %in% names(result))
})

# -----------------------------------------------------------------------------
# process_31_day_plot() tests
# -----------------------------------------------------------------------------

test_that("process_31_day_plot() returns plot dataframe", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  raw_31 <- process_31_day_raw(global_data, config)

  result <- process_31_day_plot(raw_31, config)

  expect_type(result, "list")
  expect_true("plot" %in% names(result))
  expect_s3_class(result$plot, "data.frame")
})

test_that("process_31_day_plot() calculates compliance", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  raw_31 <- process_31_day_raw(global_data, config)

  result <- process_31_day_plot(raw_31, config)

  expect_true("COMPLIANCE (%)" %in% names(result$plot))
})

test_that("process_31_day_plot() returns NULL for NULL input", {
  config <- create_mock_config("Brain")
  config$has_public_cwt_data <- FALSE

  result <- process_31_day_plot(list(public = NULL, detailed = NULL), config)

  expect_null(result)
})

# -----------------------------------------------------------------------------
# process_31_day_table() tests
# -----------------------------------------------------------------------------

test_that("process_31_day_table() returns table and latest_date", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  raw_31 <- process_31_day_raw(global_data, config)
  plot_31 <- process_31_day_plot(raw_31, config)

  result <- process_31_day_table(plot_31, config)

  expect_type(result, "list")
  expect_true("table" %in% names(result))
  expect_true("latest_date" %in% names(result))
})

test_that("process_31_day_table() handles NULL input", {
  config <- create_mock_config("Lung")
  result <- process_31_day_table(list(plot = NULL), config)

  expect_null(result$table)
  expect_null(result$latest_date)
})


# =============================================================================
# apply_provider_group_ordering() tests
# =============================================================================

test_that("apply_provider_group_ordering() returns df unchanged when cwt_provider_ordering is NULL", {
  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- NULL

  df <- data.frame(
    ORG.CODE = c("RRK", "RKB", "RL4"),
    value = c(80, 90, 70),
    stringsAsFactors = FALSE
  )

  result <- apply_provider_group_ordering(df, config)

  expect_identical(result, df)
})

test_that("apply_provider_group_ordering() places group 1 trusts before group 2 by ORG.CODE", {
  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- list(
    list(codes = c("RL4")),
    list(codes = c("RRK", "RKB"))
  )

  df <- data.frame(
    ORG.CODE = c("RRK", "RKB", "RL4"),
    value = c(80, 90, 70),
    stringsAsFactors = FALSE
  )

  result <- apply_provider_group_ordering(df, config, code_col = "ORG.CODE")

  expect_equal(result$ORG.CODE[1], "RL4")
})

test_that("apply_provider_group_ordering() sorts descending within group by sort_col", {
  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- list(
    list(codes = c("RRK", "RKB"))
  )

  df <- data.frame(
    ORG.CODE = c("RKB", "RRK"),
    value = c(70, 90),
    stringsAsFactors = FALSE
  )

  result <- apply_provider_group_ordering(df, config,
    code_col = "ORG.CODE",
    sort_col = "value", sort_desc = TRUE
  )

  expect_equal(result$ORG.CODE[1], "RRK") # 90 > 70
})

test_that("apply_provider_group_ordering() sorts ascending within group when sort_desc = FALSE", {
  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- list(
    list(codes = c("RRK", "RKB"))
  )

  df <- data.frame(
    ORG.CODE = c("RKB", "RRK"),
    value = c(70, 90),
    stringsAsFactors = FALSE
  )

  result <- apply_provider_group_ordering(df, config,
    code_col = "ORG.CODE",
    sort_col = "value", sort_desc = FALSE
  )

  expect_equal(result$ORG.CODE[1], "RKB") # 70 < 90
})

test_that("apply_provider_group_ordering() places ungrouped trusts last", {
  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- list(
    list(codes = c("RRK"))
  )

  df <- data.frame(
    ORG.CODE = c("RL4", "RRK", "RKB"), # RL4 and RKB not in any group
    value = c(90, 80, 95),
    stringsAsFactors = FALSE
  )

  result <- apply_provider_group_ordering(df, config,
    code_col = "ORG.CODE",
    sort_col = "value", sort_desc = TRUE
  )

  expect_equal(result$ORG.CODE[1], "RRK")
  expect_true(which(result$ORG.CODE == "RRK") < which(result$ORG.CODE == "RL4"))
  expect_true(which(result$ORG.CODE == "RRK") < which(result$ORG.CODE == "RKB"))
})

test_that("apply_provider_group_ordering() removes .provider_group temp column", {
  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- list(
    list(codes = c("RRK"))
  )

  df <- data.frame(
    ORG.CODE = c("RRK", "RKB"),
    value = c(80, 90),
    stringsAsFactors = FALSE
  )

  result <- apply_provider_group_ordering(df, config, code_col = "ORG.CODE")

  expect_false(".provider_group" %in% names(result))
})

test_that("apply_provider_group_ordering() resolves ORG.NAME to code via org_lookup", {
  assign("alliance", "WMCA", envir = .GlobalEnv)

  # Use actual names from the WMCA org_lookup so reverse lookup succeeds
  org_lkp <- get_alliance_config("org_lookup")
  name_lkp <- setNames(org_lkp$ORG.NAME, org_lkp$ORG.CODE)
  rrk_name <- unname(name_lkp["RRK"])
  rkb_name <- unname(name_lkp["RKB"])
  rl4_name <- unname(name_lkp["RL4"])

  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- list(
    list(codes = c("RL4")),
    list(codes = c("RRK", "RKB"))
  )

  df <- data.frame(
    ORG.NAME = c(rrk_name, rkb_name, rl4_name),
    value = c(80, 90, 70),
    stringsAsFactors = FALSE
  )

  result <- apply_provider_group_ordering(df, config,
    code_col = "ORG.NAME",
    sort_col = "value", sort_desc = TRUE
  )

  expect_equal(result$ORG.NAME[1], rl4_name)
})


# =============================================================================
# 62-DAY PROCESSING TESTS
# =============================================================================

# -----------------------------------------------------------------------------
# derive_62_day_from_detailed() tests
# -----------------------------------------------------------------------------

test_that("derive_62_day_from_detailed() returns NULL when detailed is NULL", {
  result <- derive_62_day_from_detailed(list(detailed = NULL))

  expect_null(result)
})

test_that("derive_62_day_from_detailed() returns dataframe with WITHIN.STANDARD", {
  raw_62 <- list(detailed = create_62_day_detailed_fixture("Lung"))

  result <- derive_62_day_from_detailed(raw_62)

  expect_s3_class(result, "data.frame")
  expect_true("WITHIN.STANDARD" %in% names(result))
})

test_that("derive_62_day_from_detailed() WITHIN.STANDARD is sum of day bucket columns", {
  detailed <- create_62_day_detailed_fixture("Lung")
  expected_within <- detailed$DaysWithin31 + detailed$Days32to38 +
    detailed$Days39to48 + detailed$Days49to62

  result <- derive_62_day_from_detailed(list(detailed = detailed))

  expect_equal(result$WITHIN.STANDARD, expected_within)
})

test_that("derive_62_day_from_detailed() renames NoTreated to TOTAL.TREATED", {
  raw_62 <- list(detailed = create_62_day_detailed_fixture("Lung"))

  result <- derive_62_day_from_detailed(raw_62)

  expect_true("TOTAL.TREATED" %in% names(result))
  expect_false("NoTreated" %in% names(result))
})

test_that("derive_62_day_from_detailed() renames Date to PERIOD", {
  raw_62 <- list(detailed = create_62_day_detailed_fixture("Lung"))

  result <- derive_62_day_from_detailed(raw_62)

  expect_true("PERIOD" %in% names(result))
  expect_false("Date" %in% names(result))
})

test_that("derive_62_day_from_detailed() removes Trust_Name column", {
  raw_62 <- list(detailed = create_62_day_detailed_fixture("Lung"))

  result <- derive_62_day_from_detailed(raw_62)

  expect_false("Trust_Name" %in% names(result))
})

# -----------------------------------------------------------------------------
# process_62_day_raw() tests
# -----------------------------------------------------------------------------

test_that("process_62_day_raw() returns list with expected components", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")

  result <- process_62_day_raw(global_data, config)

  expect_type(result, "list")
  expect_true("public" %in% names(result))
  expect_true("detailed" %in% names(result))
  expect_true("comm" %in% names(result))
})

test_that("process_62_day_raw() filters detailed data correctly (public is always NULL when detailed-sourced)", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")

  result <- process_62_day_raw(global_data, config)

  expect_null(result$public)
  expect_true(all(result$detailed$CancerReportCategory == "Lung"))
})

test_that("process_62_day_raw() reads the public extract and public comm when cwt_62day_detailed is null", {
  config <- create_mock_config(
    "Lung", cwt_62day_detailed = NULL, cwt_62day_public = "Lung",
    cwt_62day_public_comm = "Lung"
  )
  global_data <- create_mock_global_data("Lung")

  result <- process_62_day_raw(global_data, config)

  expect_null(result$detailed)
  expect_true(all(result$public$CANCER.TYPE == "Lung"))
  expect_true(all(result$comm$`Cancer Type` == "Lung"))
})

# -----------------------------------------------------------------------------
# process_62_day_summary() tests
# -----------------------------------------------------------------------------

test_that("process_62_day_summary() aggregates to single row", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  raw_62 <- process_62_day_raw(global_data, config)

  result <- process_62_day_summary(raw_62, config)

  expect_type(result, "list")
  expect_true("provider" %in% names(result))
  expect_equal(nrow(result$provider), 1)
})

test_that("process_62_day_summary() calculates compliance and target", {
  # public is always NULL now (derive_62_day_from_detailed() is unconditional)
  # — supply detailed rows instead.
  raw_62 <- list(
    detailed = data.frame(
      AdmittedCare = "Admitted",
      TreatmentModality = "Surgery",
      ORG.NAME = c("Trust A", "Trust B"),
      AccountableProvider = c("RRK", "RKB"),
      Date = as.Date("2024-05-01"),
      Trust_Name = c("Trust A", "Trust B"),
      NoTreated = c(50, 50),
      DaysWithin31 = c(42.5, 42.5),
      Days32to38 = 0, Days39to48 = 0, Days49to62 = 0
    )
  )
  config <- create_mock_config("Lung")

  result <- process_62_day_summary(raw_62, config)

  # Total within = 85, total treated = 100
  # Compliance = 85%
  expect_equal(result$provider$COMPLIANCE, 85.0)
  expect_true("Patients_Above_62_Day_Target" %in% names(result$provider))
})

test_that("process_62_day_summary() returns NULL provider when there is no 62-day data at all", {
  config <- create_mock_config("Lung")

  result <- process_62_day_summary(list(public = NULL, detailed = NULL), config)

  expect_null(result$provider)
})

# -----------------------------------------------------------------------------
# process_62_day_plot() tests
# -----------------------------------------------------------------------------

test_that("process_62_day_plot() returns expected components", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  raw_62 <- process_62_day_raw(global_data, config)

  result <- process_62_day_plot(raw_62, config)

  expect_type(result, "list")
  expect_true("total_treated" %in% names(result))
  expect_true("compliance" %in% names(result))
  expect_true("trusts_above" %in% names(result))
  expect_true("trusts_below" %in% names(result))
  expect_true("count_above" %in% names(result))
  expect_true("count_below" %in% names(result))
})

test_that("process_62_day_plot() returns NULL for NULL input", {
  config <- create_mock_config("Brain")
  config$has_public_cwt_data <- FALSE

  result <- process_62_day_plot(list(public = NULL, detailed = NULL), config)

  expect_null(result)
})

# -----------------------------------------------------------------------------
# process_62_day_table() tests
# -----------------------------------------------------------------------------

test_that("process_62_day_table() returns table and latest_date", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  raw_62 <- process_62_day_raw(global_data, config)
  plot_62 <- process_62_day_plot(raw_62, config)

  result <- process_62_day_table(plot_62, config)

  expect_type(result, "list")
  expect_true("table" %in% names(result))
  expect_true("latest_date" %in% names(result))
  expect_true("Latest 62-Day Compliance (%)" %in% names(result$table))
})

test_that("process_62_day_table() handles NULL input", {
  config <- create_mock_config("Lung")
  result <- process_62_day_table(NULL, config)

  expect_null(result$table)
  expect_null(result$latest_date)
})

# -----------------------------------------------------------------------------
# process_62_day_correlation() tests
# -----------------------------------------------------------------------------

test_that("process_62_day_correlation() returns correlation dataframe", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  raw_62 <- process_62_day_raw(global_data, config)

  result <- process_62_day_correlation(raw_62, config)

  expect_s3_class(result, "data.frame")
  expect_true("COMPLIANCE 62-day (%)" %in% names(result))
})

test_that("process_62_day_correlation() returns NULL when there is no 62-day data at all", {
  config <- create_mock_config("Lung")

  result <- process_62_day_correlation(list(public = NULL, detailed = NULL), config)

  expect_null(result)
})


# =============================================================================
# STAGING PROCESSING TESTS
# =============================================================================

# -----------------------------------------------------------------------------
# process_staging_completeness_raw() tests
# -----------------------------------------------------------------------------

test_that("process_staging_completeness_raw() returns filtered data", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")

  result <- process_staging_completeness_raw(global_data, config)

  expect_s3_class(result, "data.frame")
  expect_true(all(result$CANCER_GROUP_NM == "Lung"))
})

test_that("process_staging_completeness_raw() returns NULL when staging_completeness is null", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung", staging_completeness = NULL)

  result <- process_staging_completeness_raw(global_data, config)

  expect_null(result)
})

# -----------------------------------------------------------------------------
# process_staging_completeness_plot() tests
# -----------------------------------------------------------------------------

test_that("process_staging_completeness_plot() returns expected components", {
  skip_if_not(exists("has_zoo") && has_zoo, "zoo package required for as.yearqtr()")

  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  sc_raw <- process_staging_completeness_raw(global_data, config)

  result <- process_staging_completeness_plot(sc_raw)

  expect_type(result, "list")
  expect_true("plot" %in% names(result))
  expect_true("figure" %in% names(result))
  expect_true("current_quarter" %in% names(result))
})

test_that("process_staging_completeness_plot() calculates completeness", {
  skip_if_not(exists("has_zoo") && has_zoo, "zoo package required for as.yearqtr()")

  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  sc_raw <- process_staging_completeness_raw(global_data, config)

  result <- process_staging_completeness_plot(sc_raw)

  expect_true("Stage Completeness %" %in% names(result$plot))
})

test_that("process_staging_completeness_plot() handles NULL input", {
  result <- process_staging_completeness_plot(NULL)

  expect_null(result)
})

# -----------------------------------------------------------------------------
# process_staging_raw() tests
# -----------------------------------------------------------------------------

test_that("process_staging_raw() returns filtered data", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")

  result <- process_staging_raw(global_data, config)

  expect_s3_class(result, "data.frame")
  expect_true(all(result$`Cancer group` == "Lung"))
})

test_that("process_staging_raw() returns NULL when staging_demographics is split", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung", staging_demographics = c("Lung A", "Lung B"))

  result <- process_staging_raw(global_data, config)

  expect_null(result)
})

test_that("process_staging_raw() returns NULL when staging_demographics is null", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung", staging_demographics = NULL)

  result <- process_staging_raw(global_data, config)

  expect_null(result)
})

# -----------------------------------------------------------------------------
# process_early_diagnosis_alliance() tests
# -----------------------------------------------------------------------------

test_that("process_early_diagnosis_alliance() filters to cancer alliance", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  stage_raw <- process_staging_raw(global_data, config)

  result <- process_early_diagnosis_alliance(stage_raw)

  expect_s3_class(result, "data.frame")
  expect_true(all(result$`Geography type` == "Cancer alliance"))
})

test_that("process_early_diagnosis_alliance() filters to early stage metric", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  stage_raw <- process_staging_raw(global_data, config)

  result <- process_early_diagnosis_alliance(stage_raw)

  expect_true(all(result$Metric == "Early stage proportion"))
  expect_true(all(result$Breakdown == "Total"))
})

test_that("process_early_diagnosis_alliance() handles NULL input", {
  result <- process_early_diagnosis_alliance(NULL)

  expect_null(result)
})

test_that("process_early_diagnosis_alliance() returns NULL, not a crash, when its own filter matches zero rows", {
  # Regression test: real-world failure — a split group (e.g. HPB's "Liver")
  # had rows in stage_df but none at the alliance-level "Total" cut this
  # function filters to. Base ifelse() on a zero-row input returns logical(0)
  # regardless of the yes/no argument types, so `Statistic (12m)` ended up
  # typed logical instead of numeric, and ptd_spc() downstream in the Rmd
  # errored with "value_field must be a numeric vector ('Statistic (12m)' is
  # a 'logical')." Fixed with dplyr::if_else() plus an explicit NULL return;
  # this stage_raw has rows, but none matching Geography type == "Cancer
  # alliance", reproducing the same zero-row-after-filter condition.
  stage_raw <- create_staging_demographics_fixture(
    cancer_group = "Liver",
    geography_types = "Provider"  # deliberately excludes "Cancer alliance"
  )

  expect_no_error(result <- process_early_diagnosis_alliance(stage_raw))
  expect_null(result)
})

# -----------------------------------------------------------------------------
# process_early_diagnosis_trust() tests
# -----------------------------------------------------------------------------

test_that("process_early_diagnosis_trust() filters to Provider", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  stage_raw <- process_staging_raw(global_data, config)

  result <- process_early_diagnosis_trust(stage_raw)

  expect_s3_class(result, "data.frame")
  expect_true(all(result$`Geography type` == "Provider"))
})

test_that("process_early_diagnosis_trust() returns NULL, not a crash, when its own filter matches zero rows", {
  # See the matching regression test on process_early_diagnosis_alliance().
  stage_raw <- create_staging_demographics_fixture(
    cancer_group = "Liver",
    geography_types = "Cancer alliance"  # deliberately excludes "Provider"
  )

  expect_no_error(result <- process_early_diagnosis_trust(stage_raw))
  expect_null(result)
})

test_that("process_early_diagnosis_trust() returns NULL for NULL input", {
  result <- process_early_diagnosis_trust(NULL)

  expect_null(result)
})

# -----------------------------------------------------------------------------
# process_early_diagnosis_demographic() tests
# -----------------------------------------------------------------------------

test_that("process_early_diagnosis_demographic() filters by breakdown type", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  stage_raw <- process_staging_raw(global_data, config)

  result <- process_early_diagnosis_demographic(stage_raw, "Gender")

  expect_s3_class(result, "data.frame")
  expect_true(all(result$Breakdown == "Gender"))
})

test_that("process_early_diagnosis_demographic() handles NULL input", {
  result <- process_early_diagnosis_demographic(NULL, "Gender")

  expect_null(result)
})

# -----------------------------------------------------------------------------
# process_staging_demographics() tests
# -----------------------------------------------------------------------------

test_that("process_staging_demographics() returns all demographic breakdowns", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  stage_raw <- process_staging_raw(global_data, config)

  result <- process_staging_demographics(stage_raw, config)

  expect_type(result, "list")
  expect_true("overall" %in% names(result))
  expect_true("trust" %in% names(result))
  expect_true("gender" %in% names(result))
  expect_true("age" %in% names(result))
  expect_true("ethnicity" %in% names(result))
  expect_true("deprivation" %in% names(result))
  expect_true("route" %in% names(result))
})

test_that("process_staging_demographics() returns NULL when disabled", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung", staging_demographics = NULL)
  stage_raw <- process_staging_raw(global_data, config)

  result <- process_staging_demographics(stage_raw, config)

  expect_null(result)
})


# =============================================================================
# CORRELATION PROCESSING TESTS
# =============================================================================

test_that("process_fds_62_correlation() returns merged dataframes", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")

  fds_raw <- process_fds_raw(global_data, config)
  fds_plot <- process_fds_plot(fds_raw, config)
  fds_correlation <- process_fds_correlation(fds_raw, fds_plot)

  raw_62 <- process_62_day_raw(global_data, config)
  correlation_62 <- process_62_day_correlation(raw_62, config)

  result <- process_fds_62_correlation(fds_correlation, correlation_62)

  expect_type(result, "list")
  expect_true("fds_vs_62" %in% names(result))
  expect_true("fds_ro_vs_62" %in% names(result))
  expect_true("fds_ri_vs_62" %in% names(result))
})

test_that("process_fds_62_correlation() handles NULL inputs", {
  result <- process_fds_62_correlation(NULL, NULL)

  expect_null(result)
})


# =============================================================================
# LWBC PROCESSING TESTS
# =============================================================================

test_that("process_lwbc() filters by config name", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")

  result <- process_lwbc(global_data, config)

  # LWBC is always available — data is filtered by config$name
  expect_type(result, "list")
  expect_true("plot" %in% names(result))
})

test_that("process_lwbc() returns NULL when config$lwbc is NULL", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  config$lwbc <- NULL

  result <- process_lwbc(global_data, config)

  expect_null(result)
})

test_that("process_lwbc() returns one set of dataframes per group when split", {
  # No real tumour currently splits lwbc (see the null / scalar / array
  # convention in config/tumours.yml's header) - this exercises the branch
  # directly via a synthetic split config and matching fixture data, the same
  # way test-tumour_config.R exercises other schema edge cases
  # create_mock_config() alone can't reach.
  global_data <- create_mock_global_data("Lung")
  global_data$tumour_pack_lwbc_plot_df <- rbind(
    create_lwbc_plot_fixture("Group A"), create_lwbc_plot_fixture("Group B")
  )
  global_data$tumour_pack_lwbc_pa_plot_df <- rbind(
    create_lwbc_pa_plot_fixture("Group A"), create_lwbc_pa_plot_fixture("Group B")
  )
  global_data$tumour_pack_trust_lwbc_plot_df <- rbind(
    create_lwbc_trust_plot_fixture("Group A"), create_lwbc_trust_plot_fixture("Group B")
  )
  global_data$tumour_pack_trust_lwbc_pa_plot_df <- rbind(
    create_lwbc_trust_pa_plot_fixture("Group A"), create_lwbc_trust_pa_plot_fixture("Group B")
  )
  global_data$tumour_pack_alliance_trust_lwbc_table_df <- rbind(
    create_lwbc_table_fixture("Group A"), create_lwbc_table_fixture("Group B")
  )

  config <- create_mock_config("Lung")
  config$lwbc <- c("Group A", "Group B")

  result <- process_lwbc(global_data, config)

  expect_type(result, "list")
  expect_named(result, c("Group A", "Group B"))
  expect_true("plot" %in% names(result[["Group A"]]))
  expect_true("plot" %in% names(result[["Group B"]]))
})


# =============================================================================
# TREATMENT VARIATION PROCESSING TESTS
# =============================================================================

test_that("process_treatment_variation() returns NULL when not configured", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung")
  config$treatment_variation_data <- NULL

  result <- process_treatment_variation(global_data, config)

  expect_null(result)
})

test_that("process_treatment_variation() warns and returns NULL when tv_long is missing", {
  global_data <- create_mock_global_data("Lung")
  global_data$tv_long <- NULL
  config <- create_mock_config("Lung", treatment_variation = "Lung")

  expect_warning(
    result <- process_treatment_variation(global_data, config),
    "tv_long.*missing or empty"
  )
  expect_null(result)
})

test_that("process_treatment_variation() warns and returns NULL when tv_indicators is missing", {
  global_data <- create_mock_global_data("Lung")
  global_data$tv_indicators <- NULL
  config <- create_mock_config("Lung", treatment_variation = "Lung")

  expect_warning(
    result <- process_treatment_variation(global_data, config),
    "tv_indicators.*missing or empty"
  )
  expect_null(result)
})

test_that("process_treatment_variation() warns and returns NULL when no rows match the tumour's filter", {
  global_data <- create_mock_global_data("Lung")
  config <- create_mock_config("Lung", treatment_variation = "Not A Real Tumour Site")

  expect_warning(
    result <- process_treatment_variation(global_data, config),
    "No treatment variation rows"
  )
  expect_null(result)
})

test_that("process_treatment_variation() warns and returns NULL when no indicator string matches", {
  global_data <- create_mock_global_data("Lung")
  # tv_long has a "Lung" row but tv_indicators doesn't
  global_data$tv_indicators <- create_tv_indicators_fixture()[
    create_tv_indicators_fixture()$`Tumour Site` != "Lung",
  ]
  config <- create_mock_config("Lung", treatment_variation = "Lung")

  expect_warning(
    result <- process_treatment_variation(global_data, config),
    "No indicator string"
  )
  expect_null(result)
})

test_that("process_treatment_variation() multiplies moving average by 100 when the alliance's tv_config requires it", {
  # No real alliance currently sets needs_percentage_conversion = TRUE with
  # fraction-form input available to verify against (EMCA's real tv_config is
  # missing alliance_name - a separate, pre-existing gap - and its own
  # read-in script doesn't appear to feed tv_long/tv_indicators at all), so
  # this flips WMCA's own flag for the duration of the test rather than
  # switching alliance, keeping every other WMCA-shaped value (org_lookup,
  # trust_to_icb, tv_geography_map) realistic and already correct.
  global_data <- create_mock_global_data("Lung")
  global_data$tv_long$`Moving average (%)` <- 0.75  # fraction form pre-conversion
  config <- create_mock_config("Lung", treatment_variation = "Lung")

  original_tv_config <- .alliance_configs$WMCA$tv_config
  .alliance_configs$WMCA$tv_config$needs_percentage_conversion <<- TRUE
  on.exit(.alliance_configs$WMCA$tv_config <<- original_tv_config)

  result <- process_treatment_variation(global_data, config)

  expect_true(all(result$overview$table$`Moving average (%)` == 75))
  expect_true(all(result$tumour$data$`Moving avg (%)` == 75))
})


# =============================================================================
# STANDARDS TABLE TESTS
# =============================================================================

test_that("create_standards_table() combines FDS, 31-day, and 62-day tables", {
  fds_table <- data.frame(
    ORG.NAME = c("Trust A", "Trust B"),
    `Latest FDS Compliance (%)` = c(85, 90),
    Patients_Above_FDS_Target_Latest = c(5, 10),
    check.names = FALSE
  )

  table_31 <- data.frame(
    ORG.NAME = c("Trust A", "Trust B"),
    `Latest 31-Day Compliance (%)` = c(97, 98),
    Patients_Above_31_Day_Target_Latest = c(3, 5),
    check.names = FALSE
  )

  table_62 <- data.frame(
    ORG.NAME = c("Trust A", "Trust B"),
    `Latest 62-Day Compliance (%)` = c(87, 92),
    Patients_Above_62_Day_Target_Latest = c(2, 7),
    check.names = FALSE
  )

  config <- create_mock_config("Lung")
  result <- create_standards_table(fds_table, table_31, table_62, config)

  # Should have all columns from all tables
  expect_true("Latest FDS Compliance (%)" %in% names(result))
  expect_true("Latest 31-Day Compliance (%)" %in% names(result))
  expect_true("Latest 62-Day Compliance (%)" %in% names(result))

  # Should be renamed
  expect_true("No. patients above FDS target" %in% names(result))
  expect_true("No. patients above 31 Day target" %in% names(result))
  expect_true("No. patients above 62 Day target" %in% names(result))
})

test_that("create_standards_table() handles NULL 31-day and 62-day tables", {
  # FDS-only tumours (e.g. non-specific symptoms referral routes) have no
  # 31-day or 62-day data, so those tables are never joined
  fds_table <- data.frame(
    ORG.NAME = c("Trust A", "Trust B"),
    `Latest FDS Compliance (%)` = c(85, 90),
    Patients_Above_FDS_Target_Latest = c(5, 10),
    check.names = FALSE
  )

  config <- create_mock_config("Non-specific Symptoms")

  expect_no_error(
    result <- create_standards_table(fds_table, NULL, NULL, config)
  )

  # FDS column renamed, absent standards silently skipped
  expect_true("No. patients above FDS target" %in% names(result))
  expect_false("No. patients above 31 Day target" %in% names(result))
  expect_false("No. patients above 62 Day target" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("apply_provider_group_ordering() ignores a sort_col missing from the data", {
  df <- data.frame(
    ORG.NAME = c("Trust A", "Trust B"),
    `Latest FDS Compliance (%)` = c(85, 90),
    check.names = FALSE
  )

  config <- create_mock_config("Non-specific Symptoms")
  config$cwt_provider_ordering <- list(list(name = "Diag", codes = "RRK"))

  # Sorting on a 62-day column that was never joined must not error
  expect_no_error(
    result <- apply_provider_group_ordering(
      df, config,
      code_col = "ORG.NAME",
      sort_col = "Latest 62-Day Compliance (%)", sort_desc = TRUE
    )
  )

  expect_equal(nrow(result), 2)
  expect_false(".provider_group" %in% names(result))
})


# =============================================================================
# EDGE CASE TESTS
# =============================================================================

test_that("Processing functions handle single-row dataframes", {
  single_provider <- data.frame(
    CANCER.TYPE = "Lung",
    ORG.CODE = "RRK",
    ORG.NAME = "Test Trust",
    PERIOD = as.Date("2024-05-01"),
    WITHIN.STANDARD = 80,
    TOTAL.TREATED = 100,
    stringsAsFactors = FALSE
  )

  fds_raw <- list(
    provider = single_provider,
    provider_comm = single_provider,
    detailed = data.frame(
      Cancer.Report.Category = "Lung",
      Date = as.Date("2024-05-01"),
      Provider.Code.FDS = "RRK",
      ORG.NAME = "Test Trust",
      Pathway.End.Reason = "Diagnosis of cancer",
      NoPatients = 100,
      DaysWithin7 = 30,
      Days8To14 = 25,
      Days15To21 = 15,
      Days22To28 = 10,
      stringsAsFactors = FALSE
    ),
    detailed_comm = data.frame(
      Cancer.Report.Category = "Lung",
      Date = as.Date("2024-05-01"),
      Provider.Code.FDS = "RRK",
      ORG.NAME = "Test Trust",
      Pathway.End.Reason = "Diagnosis of cancer",
      NoPatients = 100,
      DaysWithin7 = 30,
      Days8To14 = 25,
      Days15To21 = 15,
      Days22To28 = 10,
      stringsAsFactors = FALSE
    )
  )

  # Should work with single row
  summary_result <- process_fds_summary(fds_raw)

  expect_equal(nrow(summary_result$provider), 1)
  expect_equal(summary_result$provider$COMPLIANCE, 80.0)
})

# Note: Empty dataframe handling would require modifications to the source
# functions, as case_when() and some dplyr operations don't handle empty
# vectors gracefully in all cases.


# =============================================================================
# PROVIDER GROUP LABEL TESTS
# =============================================================================

# -----------------------------------------------------------------------------
# add_provider_group_labels() tests
# -----------------------------------------------------------------------------

test_that("add_provider_group_labels() returns data unchanged when cwt_provider_ordering is NULL", {
  df <- data.frame(
    ORG.NAME = c("University Hospitals Birmingham", "University Hospitals Coventry & Warwickshire"),
    value = c(1, 2),
    stringsAsFactors = FALSE
  )
  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- NULL

  result <- add_provider_group_labels(df, config)

  expect_identical(result, df)
})

test_that("add_provider_group_labels() returns data unchanged when groups have no name field", {
  df <- data.frame(
    ORG.NAME = c("University Hospitals Birmingham", "University Hospitals Coventry & Warwickshire"),
    value = c(1, 2),
    stringsAsFactors = FALSE
  )
  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- list(
    list(codes = c("RRK", "RKB"))
  )

  result <- add_provider_group_labels(df, config)

  expect_identical(result, df)
})

test_that("add_provider_group_labels() prepends group names to character ORG.NAME", {
  df <- data.frame(
    ORG.NAME = c("Shrewsbury & Telford", "University Hospitals Birmingham", "ZZZ - Other Trust"),
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  config <- create_mock_config("Head and Neck")
  config$cwt_provider_ordering <- list(
    list(name = "Treatment", codes = c("RXW", "RNA")),
    list(name = "Diagnostic", codes = c("RRK", "RKB"))
  )

  result <- add_provider_group_labels(df, config)

  expect_equal(result$ORG.NAME[1], "[Treatment] Shrewsbury & Telford")
  expect_equal(result$ORG.NAME[2], "[Diagnostic] University Hospitals Birmingham")
  expect_equal(result$ORG.NAME[3], "ZZZ - Other Trust")
})

test_that("add_provider_group_labels() preserves factor level ordering", {
  df <- data.frame(
    ORG.NAME = factor(
      c("Shrewsbury & Telford", "University Hospitals Birmingham"),
      levels = c("Shrewsbury & Telford", "University Hospitals Birmingham")
    ),
    value = c(1, 2)
  )
  config <- create_mock_config("Head and Neck")
  config$cwt_provider_ordering <- list(
    list(name = "Treatment", codes = c("RXW")),
    list(name = "Diagnostic", codes = c("RRK"))
  )

  result <- add_provider_group_labels(df, config)

  expect_s3_class(result$ORG.NAME, "factor")
  expect_equal(
    levels(result$ORG.NAME),
    c("[Treatment] Shrewsbury & Telford", "[Diagnostic] University Hospitals Birmingham")
  )
  # Values should also be updated
  expect_equal(as.character(result$ORG.NAME[1]), "[Treatment] Shrewsbury & Telford")
  expect_equal(as.character(result$ORG.NAME[2]), "[Diagnostic] University Hospitals Birmingham")
})

test_that("add_provider_group_labels() leaves ungrouped trusts unprefixed", {
  df <- data.frame(
    ORG.NAME = c("ZZZ - Ungrouped Trust"),
    value = 1,
    stringsAsFactors = FALSE
  )
  config <- create_mock_config("Head and Neck")
  config$cwt_provider_ordering <- list(
    list(name = "Treatment", codes = c("RXW"))
  )

  result <- add_provider_group_labels(df, config)

  expect_equal(result$ORG.NAME, "ZZZ - Ungrouped Trust")
})

# -----------------------------------------------------------------------------
# add_group_labels_to_plot_list() tests
# -----------------------------------------------------------------------------

test_that("add_group_labels_to_plot_list() returns NULL for NULL input", {
  config <- create_mock_config("Head and Neck")
  config$cwt_provider_ordering <- list(
    list(name = "Treatment", codes = c("RXW"))
  )

  result <- add_group_labels_to_plot_list(NULL, config)

  expect_null(result)
})

test_that("add_group_labels_to_plot_list() returns unchanged when no ordering", {
  plot_list <- list(
    compliance = data.frame(ORG.NAME = "University Hospitals Birmingham", value = 1, stringsAsFactors = FALSE),
    count_above = 3
  )
  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- NULL

  result <- add_group_labels_to_plot_list(plot_list, config)

  expect_identical(result, plot_list)
})

test_that("add_group_labels_to_plot_list() applies labels to dataframes with ORG.NAME", {
  plot_list <- list(
    compliance = data.frame(
      ORG.NAME = c("Shrewsbury & Telford", "University Hospitals Birmingham"),
      value = c(1, 2),
      stringsAsFactors = FALSE
    ),
    count_above = 3,
    total_treated = data.frame(
      PERIOD = as.Date("2024-01-01"),
      TOTAL.TREATED = 100,
      stringsAsFactors = FALSE
    )
  )
  config <- create_mock_config("Head and Neck")
  config$cwt_provider_ordering <- list(
    list(name = "Treatment", codes = c("RXW")),
    list(name = "Diagnostic", codes = c("RRK"))
  )

  result <- add_group_labels_to_plot_list(plot_list, config)

  # compliance df should be prefixed
  expect_equal(result$compliance$ORG.NAME[1], "[Treatment] Shrewsbury & Telford")
  expect_equal(result$compliance$ORG.NAME[2], "[Diagnostic] University Hospitals Birmingham")

  # Non-dataframe elements unchanged
  expect_equal(result$count_above, 3)

  # Dataframes without ORG.NAME unchanged
  expect_equal(result$total_treated$TOTAL.TREATED, 100)
})


# =============================================================================
# PROVIDER GROUP BOUNDARY TESTS
# =============================================================================

# -----------------------------------------------------------------------------
# get_provider_group_boundaries() tests
# -----------------------------------------------------------------------------

test_that("get_provider_group_boundaries() returns integer(0) when cwt_provider_ordering is NULL", {
  df <- data.frame(
    ORG.NAME = c("University Hospitals Birmingham", "University Hospitals Coventry & Warwickshire"),
    value = c(1, 2),
    stringsAsFactors = FALSE
  )
  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- NULL

  result <- get_provider_group_boundaries(df, config, code_col = "ORG.NAME")

  expect_identical(result, integer(0))
})

test_that("get_provider_group_boundaries() returns correct boundary for two groups", {
  # Rows 1-3 are Group 1 (Treatment), rows 4-5 are Group 2 (Diagnostic)
  df <- data.frame(
    ORG.NAME = c(
      "Shrewsbury & Telford", "Dudley Group", "South Warwickshire",
      "University Hospitals Birmingham", "University Hospitals Coventry & Warwickshire"
    ),
    value = 1:5,
    stringsAsFactors = FALSE
  )
  config <- create_mock_config("Head and Neck")
  config$cwt_provider_ordering <- list(
    list(name = "Treatment", codes = c("RXW", "RNA", "RJC")),
    list(name = "Diagnostic", codes = c("RRK", "RKB"))
  )

  result <- get_provider_group_boundaries(df, config, code_col = "ORG.NAME")

  # Boundary should be at row 3 (last row of Treatment group)
  expect_equal(result, 3L)
})

test_that("get_provider_group_boundaries() applies offset for benchmark tables", {
  # Benchmark tables have National and Alliance rows prepended before providers
  df <- data.frame(
    ORG.NAME = c(
      "National", "West Midlands Cancer Alliance",
      "Shrewsbury & Telford", "Dudley Group", "University Hospitals Birmingham"
    ),
    value = 1:5,
    stringsAsFactors = FALSE
  )
  config <- create_mock_config("Head and Neck")
  config$cwt_provider_ordering <- list(
    list(name = "Treatment", codes = c("RXW", "RNA")),
    list(name = "Diagnostic", codes = c("RRK"))
  )

  result <- get_provider_group_boundaries(df, config, code_col = "ORG.NAME", offset = 2)

  # National/Alliance rows are skipped. Provider boundary between Treatment
  # (rows 3-4) and Diagnostic (row 5) is at provider-relative position 2,
  # plus offset 2 = flextable body row 4
  expect_equal(result, 4L)
})

test_that("get_provider_group_boundaries() returns integer(0) when offset consumes every row", {
  df <- data.frame(
    ORG.NAME = c("National", "West Midlands Cancer Alliance"),
    value = 1:2,
    stringsAsFactors = FALSE
  )
  config <- create_mock_config("Head and Neck")
  config$cwt_provider_ordering <- list(
    list(name = "Treatment", codes = c("RXW")),
    list(name = "Diagnostic", codes = c("RRK"))
  )

  result <- get_provider_group_boundaries(df, config, code_col = "ORG.NAME", offset = 2)

  expect_identical(result, integer(0))
})

test_that("get_provider_group_boundaries() works with ORG.CODE directly, no org_lookup resolution", {
  df <- data.frame(
    ORG.CODE = c("RXW", "RNA", "RRK", "RKB"),
    value = 1:4,
    stringsAsFactors = FALSE
  )
  config <- create_mock_config("Head and Neck")
  config$cwt_provider_ordering <- list(
    list(name = "Treatment", codes = c("RXW", "RNA")),
    list(name = "Diagnostic", codes = c("RRK", "RKB"))
  )

  result <- get_provider_group_boundaries(df, config, code_col = "ORG.CODE")

  expect_equal(result, 2L)
})

test_that("get_provider_group_boundaries() returns integer(0) for single group", {
  df <- data.frame(
    ORG.NAME = c("Shrewsbury & Telford", "Dudley Group"),
    value = 1:2,
    stringsAsFactors = FALSE
  )
  config <- create_mock_config("Head and Neck")
  config$cwt_provider_ordering <- list(
    list(name = "Treatment", codes = c("RXW", "RNA"))
  )

  result <- get_provider_group_boundaries(df, config, code_col = "ORG.NAME")

  expect_identical(result, integer(0))
})

test_that("get_provider_group_boundaries() works with Provider code_col", {
  df <- data.frame(
    Provider = c("Shrewsbury & Telford", "University Hospitals Birmingham"),
    value = 1:2,
    stringsAsFactors = FALSE
  )
  config <- create_mock_config("Head and Neck")
  config$cwt_provider_ordering <- list(
    list(name = "Treatment", codes = c("RXW")),
    list(name = "Diagnostic", codes = c("RRK"))
  )

  result <- get_provider_group_boundaries(df, config, code_col = "Provider")

  expect_equal(result, 1L)
})

test_that("get_provider_group_boundaries() handles multiple boundaries", {
  df <- data.frame(
    ORG.NAME = c(
      "Shrewsbury & Telford", "Dudley Group",
      "University Hospitals Birmingham", "University Hospitals Coventry & Warwickshire",
      "ZZZ - Ungrouped Trust"
    ),
    value = 1:5,
    stringsAsFactors = FALSE
  )
  config <- create_mock_config("Head and Neck")
  config$cwt_provider_ordering <- list(
    list(name = "Treatment", codes = c("RXW", "RNA")),
    list(name = "Diagnostic", codes = c("RRK", "RKB"))
  )

  result <- get_provider_group_boundaries(df, config, code_col = "ORG.NAME")

  # Boundary between Treatment and Diagnostic at row 2,
  # and between Diagnostic and ungrouped at row 4
  expect_equal(result, c(2L, 4L))
})


# =============================================================================
# set_provider_factor_levels() tests
# =============================================================================

test_that("set_provider_factor_levels() returns df unchanged when cwt_provider_ordering is NULL", {
  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- NULL

  df <- data.frame(
    ORG.NAME = c("Trust A", "Trust B"),
    PERIOD = as.Date(c("2024-06-01", "2024-06-01")),
    stringsAsFactors = FALSE
  )

  result <- set_provider_factor_levels(df, config)

  expect_identical(result, df)
})

test_that("set_provider_factor_levels() converts code_col to factor when ordering defined", {
  assign("alliance", "WMCA", envir = .GlobalEnv)

  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- list(
    list(codes = c("RRK", "RKB")),
    list(codes = c("RL4"))
  )

  df <- data.frame(
    ORG.NAME = unname(DEFAULT_TRUST_NAMES[c("RL4", "RKB", "RRK")]),
    PERIOD = as.Date(rep("2024-06-01", 3)),
    value = c(80, 85, 90),
    stringsAsFactors = FALSE
  )

  result <- set_provider_factor_levels(df, config, sort_col = "value")

  expect_true(is.factor(result$ORG.NAME))
})

test_that("set_provider_factor_levels() places group 1 trusts first in factor levels", {
  assign("alliance", "WMCA", envir = .GlobalEnv)

  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- list(
    list(codes = c("RRK")),
    list(codes = c("RL4"))
  )

  df <- data.frame(
    ORG.NAME = unname(DEFAULT_TRUST_NAMES),
    PERIOD = as.Date(rep("2024-06-01", 3)),
    value = c(80, 85, 90),
    stringsAsFactors = FALSE
  )

  result <- set_provider_factor_levels(df, config, sort_col = "value")
  lvls <- levels(result$ORG.NAME)

  rrk_name <- unname(DEFAULT_TRUST_NAMES["RRK"])
  rl4_name <- unname(DEFAULT_TRUST_NAMES["RL4"])
  expect_true(which(lvls == rrk_name) < which(lvls == rl4_name))
})

test_that("set_provider_factor_levels() all original values appear in factor levels", {
  assign("alliance", "WMCA", envir = .GlobalEnv)

  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- list(
    list(codes = c("RRK"))
  )

  df <- data.frame(
    ORG.NAME = unname(DEFAULT_TRUST_NAMES),
    PERIOD = as.Date(rep("2024-06-01", 3)),
    stringsAsFactors = FALSE
  )

  result <- set_provider_factor_levels(df, config)

  expect_true(all(unname(DEFAULT_TRUST_NAMES) %in% levels(result$ORG.NAME)))
})

test_that("set_provider_factor_levels() uses the whole dataframe when period_col is absent", {
  assign("alliance", "WMCA", envir = .GlobalEnv)

  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- list(
    list(codes = c("RRK")),
    list(codes = c("RL4"))
  )

  # No PERIOD column at all - set_provider_factor_levels() must fall back to
  # treating every row as "latest" rather than erroring.
  df <- data.frame(
    ORG.NAME = unname(DEFAULT_TRUST_NAMES),
    value = c(80, 85, 90),
    stringsAsFactors = FALSE
  )

  result <- set_provider_factor_levels(df, config, sort_col = "value")
  lvls <- levels(result$ORG.NAME)

  rrk_name <- unname(DEFAULT_TRUST_NAMES["RRK"])
  rl4_name <- unname(DEFAULT_TRUST_NAMES["RL4"])
  expect_true(which(lvls == rrk_name) < which(lvls == rl4_name))
})


# =============================================================================
# process_fds_rule_in_comm() tests
# =============================================================================

test_that("process_fds_rule_in_comm() returns list with all expected components", {
  fds_raw <- list(
    detailed_comm = create_fds_detailed_fixture("Lung")
  )

  result <- process_fds_rule_in_comm(fds_raw)

  expect_type(result, "list")
  expect_true(all(c(
    "raw", "summary", "trust", "trusts_above", "trusts_below",
    "count_above", "count_below"
  ) %in% names(result)))
})

test_that("process_fds_rule_in_comm() raw contains only Diagnosis of cancer pathway", {
  fds_raw <- list(
    detailed_comm = create_fds_detailed_fixture("Lung",
      pathway_reasons = c("Diagnosis of cancer", "Ruling out of cancer")
    )
  )

  result <- process_fds_rule_in_comm(fds_raw)

  expect_true(all(result$raw$Pathway.End.Reason == "Diagnosis of cancer"))
})

test_that("process_fds_rule_in_comm() count_above + count_below equals total trust rows", {
  fds_raw <- list(
    detailed_comm = create_fds_detailed_fixture("Lung")
  )

  result <- process_fds_rule_in_comm(fds_raw)

  expect_equal(result$count_above + result$count_below, nrow(result$trust))
})

test_that("process_fds_rule_in_comm() summary has COMPLIANCE and Patients_Above_FDS_Target", {
  fds_raw <- list(
    detailed_comm = create_fds_detailed_fixture("Lung")
  )

  result <- process_fds_rule_in_comm(fds_raw)

  expect_true("COMPLIANCE" %in% names(result$summary))
  expect_true("Patients_Above_FDS_Target" %in% names(result$summary))
})

test_that("process_fds_rule_in_comm() trust has Latest FDS Compliance (%) column", {
  fds_raw <- list(
    detailed_comm = create_fds_detailed_fixture("Lung")
  )

  result <- process_fds_rule_in_comm(fds_raw)

  expect_true("Latest FDS Compliance (%)" %in% names(result$trust))
})


# =============================================================================
# process_staging_completeness_subtypes() tests
# =============================================================================

test_that("process_staging_completeness_subtypes() returns NULL when config has no sub_types", {
  config <- create_mock_config("Lung")
  config$sub_types <- NULL

  global_data <- create_mock_global_data("Lung")

  result <- process_staging_completeness_subtypes(global_data, config)

  expect_null(result)
})

test_that("process_staging_completeness_subtypes() returns NULL for lower_gi sub-types without staging_completeness", {
  skip_if_not(exists("has_zoo") && has_zoo, "zoo package required for staging completeness plot")

  config <- get_tumour_config("lower_gi")
  global_data <- create_mock_global_data(tumour_key = "lower_gi")

  result <- process_staging_completeness_subtypes(global_data, config)

  expect_null(result)
})

test_that("process_staging_completeness_subtypes() returns named list for urology", {
  skip_if_not(exists("has_zoo") && has_zoo, "zoo package required for staging completeness plot")

  config <- get_tumour_config("urology")
  global_data <- create_mock_global_data(tumour_key = "urology")
  global_data$stage_completeness_df <- create_staging_completeness_fixture(
    cancer_group = c("Urological", "Prostate")
  )

  result <- process_staging_completeness_subtypes(global_data, config)

  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("process_staging_completeness_subtypes() returns NULL when every split group's filter matches zero rows", {
  config <- get_tumour_config("urology")
  global_data <- create_mock_global_data(tumour_key = "urology")
  # Neither "Urological" nor "Prostate" (urology's split values) appear here.
  global_data$stage_completeness_df <- create_staging_completeness_fixture(
    cancer_group = "Totally Unrelated"
  )

  result <- suppressWarnings(process_staging_completeness_subtypes(global_data, config))

  expect_null(result)
})

test_that("process_staging_completeness_subtypes() keys are the raw split values for urology", {
  skip_if_not(exists("has_zoo") && has_zoo, "zoo package required for staging completeness plot")

  config <- get_tumour_config("urology")
  global_data <- create_mock_global_data(tumour_key = "urology")
  global_data$stage_completeness_df <- create_staging_completeness_fixture(
    cancer_group = c("Urological", "Prostate")
  )

  result <- process_staging_completeness_subtypes(global_data, config)

  # urology's staging_completeness split is ["Urological", "Prostate"] — the
  # raw value doubles as the key, there is no separate display name any more.
  expect_true("Urological" %in% names(result))
  expect_true("Prostate" %in% names(result))
})


# =============================================================================
# process_staging_with_subtypes() tests
# =============================================================================

test_that("process_staging_with_subtypes() returns NULL when staging_demographics is not split", {
  config <- create_mock_config("Lung")  # staging_demographics defaults to a single string

  result <- process_staging_with_subtypes(create_mock_global_data("Lung"), config)

  expect_null(result)
})

test_that("process_staging_with_subtypes() returns NULL when staging_demographics is null", {
  config <- get_tumour_config("lower_gi")
  config$staging_demographics <- NULL

  result <- process_staging_with_subtypes(create_mock_global_data(tumour_key = "lower_gi"), config)

  expect_null(result)
})

test_that("process_staging_with_subtypes() returns all demographic keys for lower_gi", {
  config <- get_tumour_config("lower_gi")

  global_data <- create_mock_global_data(tumour_key = "lower_gi")
  global_data$stage_df <- create_staging_demographics_fixture(
    cancer_group = c("Colon", "Rectal")
  )

  result <- process_staging_with_subtypes(global_data, config)

  expect_type(result, "list")
  expect_true(all(c(
    "overall", "trust", "gender", "age",
    "ethnicity", "deprivation", "route"
  ) %in% names(result)))
})

test_that("process_staging_with_subtypes() inner keys are sub-type display names", {
  config <- get_tumour_config("lower_gi")

  global_data <- create_mock_global_data(tumour_key = "lower_gi")
  global_data$stage_df <- create_staging_demographics_fixture(
    cancer_group = c("Colon", "Rectal")
  )

  result <- process_staging_with_subtypes(global_data, config)

  expect_true("Colon" %in% names(result$overall))
  expect_true("Rectal" %in% names(result$overall))
})

test_that("process_staging_with_subtypes() each demographic entry is a dataframe or NULL", {
  config <- get_tumour_config("lower_gi")

  global_data <- create_mock_global_data(tumour_key = "lower_gi")
  global_data$stage_df <- create_staging_demographics_fixture(
    cancer_group = c("Colon", "Rectal")
  )

  result <- process_staging_with_subtypes(global_data, config)

  for (entry in result$overall) {
    expect_true(is.null(entry) || is.data.frame(entry))
  }
})

test_that("process_staging_with_subtypes() drops a group from one demographic's names() when that cut has no data, without crashing", {
  # Regression test for the HPB "Liver"/"Pancreas" crash: a split group with
  # rows in stage_df but none at the alliance-level "Total" cut must not
  # appear in result$overall's names — a NULL list element assignment (list()
  # [[<-] <- NULL) removes the key entirely, which is what the Rmd template's
  # `for (grp in names(tumour_data$sub_types$overall))` loop relies on to
  # skip it cleanly. "Rectal" still has full data and must still appear.
  config <- create_mock_config("Test Tumour", staging_demographics = c("Colon", "Rectal"))
  global_data <- create_mock_global_data("Test Tumour")
  global_data$stage_df <- rbind(
    create_staging_demographics_fixture(cancer_group = "Colon", geography_types = "Provider"),
    create_staging_demographics_fixture(cancer_group = "Rectal")
  )

  result <- NULL
  expect_no_error(result <- process_staging_with_subtypes(global_data, config))

  expect_false("Colon" %in% names(result$overall))
  expect_true("Rectal" %in% names(result$overall))
})


# =============================================================================
# process_benchmarking() tests
# =============================================================================

test_that("process_benchmarking() always returns fds_end_reason", {
  assign("alliance", "WMCA", envir = .GlobalEnv)

  config <- get_tumour_config("lung")
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- process_benchmarking(global_data, config)

  # fds_end_reason comes from the detailed extract, so it is not gated on
  # cwt_public_data_exists_for_benchmarking
  expect_true("fds_end_reason" %in% names(result))
})

test_that("process_benchmarking() omits fds_end_reason when cwt_fds_detailed is null", {
  assign("alliance", "WMCA", envir = .GlobalEnv)

  config <- create_mock_config("Lung", cwt_fds_detailed = NULL, cwt_fds_public = "Lung")
  global_data <- create_mock_global_data("Lung")

  result <- process_benchmarking(global_data, config)

  expect_false("fds_end_reason" %in% names(result))
})

test_that("process_benchmarking() returns day_28, day_31 and day_62 when cwt_public_data_exists_for_benchmarking is TRUE", {
  assign("alliance", "WMCA", envir = .GlobalEnv)

  config <- get_tumour_config("lung")
  config$cwt_public_data_exists_for_benchmarking <- TRUE

  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- process_benchmarking(global_data, config)

  expect_true("day_28" %in% names(result))
  expect_true("day_31" %in% names(result))
  expect_true("day_62" %in% names(result))
})

test_that("process_benchmarking() omits standards opted out via display_benchmarking_*", {
  assign("alliance", "WMCA", envir = .GlobalEnv)

  # FDS-only pathway: public benchmarking data exists, but the 31-day and
  # 62-day slides are not wanted
  config <- get_tumour_config("lung")
  config$cwt_public_data_exists_for_benchmarking <- TRUE
  config$display_benchmarking_31day <- FALSE
  config$display_benchmarking_62day <- FALSE

  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- process_benchmarking(global_data, config)

  expect_true("day_28" %in% names(result))
  expect_false("day_31" %in% names(result))
  expect_false("day_62" %in% names(result))

  # Opting out of benchmarking must not affect the FDS end reason table, which
  # is not a benchmarking slide and is always produced
  expect_true("fds_end_reason" %in% names(result))
})

test_that("process_benchmarking() omits day_28, day_31 and day_62 when cwt_public_data_exists_for_benchmarking is FALSE", {
  assign("alliance", "WMCA", envir = .GlobalEnv)

  config <- get_tumour_config("lung")
  config$cwt_public_data_exists_for_benchmarking <- FALSE

  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- process_benchmarking(global_data, config)

  expect_false("day_28" %in% names(result))
  expect_false("day_31" %in% names(result))
  expect_false("day_62" %in% names(result))
})

test_that("process_benchmarking() day_28 is a dataframe when cwt_public_data_exists_for_benchmarking is TRUE", {
  assign("alliance", "WMCA", envir = .GlobalEnv)

  config <- get_tumour_config("lung")
  config$cwt_public_data_exists_for_benchmarking <- TRUE

  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- process_benchmarking(global_data, config)

  expect_s3_class(result$day_28, "data.frame")
})

test_that("process_benchmarking() splits a benchmarking standard whose public field is split", {
  assign("alliance", "WMCA", envir = .GlobalEnv)

  config <- get_tumour_config("urology")
  global_data <- create_mock_global_data(tumour_key = "urology")

  result <- process_benchmarking(global_data, config)

  # cwt_31day_public/cwt_62day_public are split for urology — a named list of
  # tables, one per group, not a single combined table
  expect_true(is.list(result$day_31) && !is.data.frame(result$day_31))
  expect_setequal(names(result$day_31), c("Urological - Other (a)", "Urological - Prostate"))
  expect_s3_class(result$day_31[[1]], "data.frame")

  # cwt_fds_public is NOT split for urology — stays a single table
  expect_s3_class(result$day_28, "data.frame")
})


# =============================================================================
# process_benchmark_standard() tests
# =============================================================================

test_that("process_benchmark_standard() returns dataframe with National Average as first row", {
  assign("alliance", "WMCA", envir = .GlobalEnv)

  data <- create_benchmarking_fixture("Lung")
  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- NULL

  result <- process_benchmark_standard(data, "FDS", "Lung", config)

  expect_s3_class(result, "data.frame")
  expect_equal(result$ORG.NAME[1], "National Average")
})

test_that("process_benchmark_standard() returns Alliance Average as second row", {
  assign("alliance", "WMCA", envir = .GlobalEnv)

  data <- create_benchmarking_fixture("Lung")
  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- NULL

  result <- process_benchmark_standard(data, "FDS", "Lung", config)

  expect_equal(result$ORG.NAME[2], "Alliance Average")
})

test_that("process_benchmark_standard() trust rows follow the two average rows", {
  assign("alliance", "WMCA", envir = .GlobalEnv)

  data <- create_benchmarking_fixture("Lung")
  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- NULL

  result <- process_benchmark_standard(data, "FDS", "Lung", config)

  expect_true(nrow(result) > 2)
  trust_rows <- result[-(1:2), ]
  expect_true(all(trust_rows$ORG.CODE %in% DEFAULT_TRUSTS))
})

test_that("process_benchmark_standard() includes required compliance columns", {
  assign("alliance", "WMCA", envir = .GlobalEnv)

  data <- create_benchmarking_fixture("Lung")
  config <- create_mock_config("Lung")
  config$cwt_provider_ordering <- NULL

  result <- process_benchmark_standard(data, "FDS", "Lung", config)

  expect_true("Latest FDS Compliance (%)" %in% names(result))
  expect_true("Difference from the target (no. patients)" %in% names(result))
})
