# Integration Tests for read_tumour_data.R
# =============================================================================
# These tests verify that the full pipeline works correctly when all components
# are combined. They test:
# 1. read_tumour_data() produces the expected nested structure
# 2. create_global_data_list() works correctly
# 3. read_all_tumour_data() handles multiple tumours
# 4. Output structure matches what RMarkdown templates expect

# Set alliance for tests
assign("alliance", "WMCA", envir = .GlobalEnv)

# =============================================================================
# read_tumour_data() INTEGRATION TESTS
# =============================================================================

# -----------------------------------------------------------------------------
# Basic Structure Tests
# -----------------------------------------------------------------------------

test_that("read_tumour_data() returns a list with expected top-level components", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  expect_type(result, "list")

  # Check all top-level components exist
  expected_components <- c(
    "config", "fds", "day_31", "day_62", "staging",
    "sub_types", "benchmarking", "correlation",
    "standards_table", "lwbc", "treatment_variation",
    "counts", "dates"
  )

  for (comp in expected_components) {
    expect_true(comp %in% names(result), info = paste("Missing component:", comp))
  }
})

test_that("read_tumour_data() includes the config in result", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  expect_type(result$config, "list")
  expect_equal(result$config$name, "Lung")
})

# -----------------------------------------------------------------------------
# FDS Structure Tests
# -----------------------------------------------------------------------------

test_that("read_tumour_data() FDS component has expected structure", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  # Check FDS sub-components
  expect_true("raw" %in% names(result$fds))
  expect_true("summary" %in% names(result$fds))
  expect_true("rule_in" %in% names(result$fds))
  expect_true("rule_in_comm" %in% names(result$fds))
  expect_true("plot" %in% names(result$fds))
  expect_true("table" %in% names(result$fds))
  expect_true("correlation" %in% names(result$fds))
})

test_that("read_tumour_data() FDS raw has provider and detailed data", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  expect_true("provider" %in% names(result$fds$raw))
  expect_true("provider_comm" %in% names(result$fds$raw))
  expect_true("detailed" %in% names(result$fds$raw))
  expect_true("detailed_comm" %in% names(result$fds$raw))
})

test_that("read_tumour_data() FDS plot has compliance and counts", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  expect_true("compliance" %in% names(result$fds$plot))
  expect_true("trusts_above" %in% names(result$fds$plot))
  expect_true("trusts_below" %in% names(result$fds$plot))
  expect_true("count_above" %in% names(result$fds$plot))
  expect_true("count_below" %in% names(result$fds$plot))
})

# -----------------------------------------------------------------------------
# 31-Day Structure Tests
# -----------------------------------------------------------------------------

test_that("read_tumour_data() day_31 component has expected structure", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  expect_true("raw" %in% names(result$day_31))
  expect_true("plot" %in% names(result$day_31))
  expect_true("table" %in% names(result$day_31))
})

# -----------------------------------------------------------------------------
# 62-Day Structure Tests
# -----------------------------------------------------------------------------

test_that("read_tumour_data() day_62 component has expected structure", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  expect_true("raw" %in% names(result$day_62))
  expect_true("summary" %in% names(result$day_62))
  expect_true("plot" %in% names(result$day_62))
  expect_true("table" %in% names(result$day_62))
  expect_true("correlation" %in% names(result$day_62))
})

# -----------------------------------------------------------------------------
# Staging Structure Tests
# -----------------------------------------------------------------------------

test_that("read_tumour_data() staging component has expected structure", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  # Lung has staging completeness
  expect_true("completeness" %in% names(result$staging))
})

test_that("read_tumour_data() staging early_diagnosis has demographics", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  # Lung has staging demographics
  if (!is.null(result$staging$early_diagnosis)) {
    expected_demographics <- c("overall", "trust", "gender", "age",
                               "ethnicity", "deprivation", "route")
    for (demo in expected_demographics) {
      expect_true(demo %in% names(result$staging$early_diagnosis),
                  info = paste("Missing demographic:", demo))
    }
  }
})

# -----------------------------------------------------------------------------
# Counts Tests
# -----------------------------------------------------------------------------

test_that("read_tumour_data() counts component has numeric values", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  # FDS counts
  expect_true("fds_trusts_above" %in% names(result$counts))
  expect_true("fds_trusts_below" %in% names(result$counts))
  expect_type(result$counts$fds_trusts_above, "integer")
  expect_type(result$counts$fds_trusts_below, "integer")
})

# -----------------------------------------------------------------------------
# Dates Tests
# -----------------------------------------------------------------------------

test_that("read_tumour_data() dates component has expected entries", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  expect_true("fds" %in% names(result$dates))
  expect_true("day_31" %in% names(result$dates))
  expect_true("day_62" %in% names(result$dates))
})

# -----------------------------------------------------------------------------
# Benchmarking Tests
# -----------------------------------------------------------------------------

test_that("read_tumour_data() benchmarking component has tables", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  expect_type(result$benchmarking, "list")
  # Lung has a public benchmarking bucket, so day_28/day_31/day_62 are
  # populated even though has_public_cwt_data is false (that flag only
  # controls the FDS/31/62-day trend data, not benchmarking); fds_end_reason
  # comes from the detailed extract and is always present
  expect_true("fds_end_reason" %in% names(result$benchmarking))
  expect_true("day_28" %in% names(result$benchmarking))
})

test_that("read_tumour_data() omits benchmarking tables when cwt_public_data_exists_for_benchmarking is FALSE", {
  global_data <- create_mock_global_data(tumour_key = "brain_cns")

  result <- suppressMessages(read_tumour_data("brain_cns", global_data))

  expect_type(result$benchmarking, "list")
  expect_true("fds_end_reason" %in% names(result$benchmarking))
  expect_false("day_28" %in% names(result$benchmarking))
})

# -----------------------------------------------------------------------------
# Correlation Tests
# -----------------------------------------------------------------------------

test_that("read_tumour_data() correlation component has FDS vs 62 data", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  if (!is.null(result$correlation)) {
    expect_true("fds_vs_62" %in% names(result$correlation))
  }
})

# -----------------------------------------------------------------------------
# Standards Table Tests
# -----------------------------------------------------------------------------

test_that("read_tumour_data() creates standards_table", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  expect_s3_class(result$standards_table, "data.frame")
  expect_true("ORG.NAME" %in% names(result$standards_table))
})

# =============================================================================
# MULTIPLE TUMOUR TYPE TESTS
# =============================================================================

test_that("read_tumour_data() works for Breast tumour type", {
  # Create global data with Breast as the cancer type
  global_data <- create_mock_global_data(tumour_key = "breast")

  result <- suppressMessages(read_tumour_data("breast", global_data))

  expect_type(result, "list")
  expect_equal(result$config$name, "Breast")
  expect_true("fds" %in% names(result))
})

test_that("read_tumour_data() works for Head and Neck tumour type", {
  global_data <- create_mock_global_data(tumour_key = "head_neck")

  result <- suppressMessages(read_tumour_data("head_neck", global_data))

  expect_type(result, "list")
  expect_equal(result$config$name, "Head and Neck")
})

# =============================================================================
# create_global_data_list() TESTS
# =============================================================================

test_that("create_global_data_list() returns a list", {
  # Create a test environment with only 2 of 17 required variables;
  # warnings are expected for the 15 missing ones
  test_env <- new.env()
  test_env$fds_df <- data.frame(x = 1)
  test_env$day_31_df <- data.frame(x = 2)

  warnings <- capture_warnings(result <- create_global_data_list(env = test_env))
  expect_true(any(grepl("Missing required variable", warnings)))

  expect_type(result, "list")
})

test_that("create_global_data_list() includes existing variables", {
  test_env <- new.env()
  test_env$fds_df <- data.frame(ORG.CODE = "RRK", VALUE = 100)
  test_env$cwt_fds_comm_df <- data.frame(ORG.CODE = "RKB", VALUE = 200)

  result <- suppressWarnings(create_global_data_list(env = test_env))

  expect_equal(result$fds_df$VALUE, 100)
  expect_equal(result$cwt_fds_comm_df$VALUE, 200)
})

test_that("create_global_data_list() warns about missing required variables", {
  test_env <- new.env()
  test_env$fds_df <- data.frame(x = 1)
  # Missing most required variables

  warnings <- capture_warnings(create_global_data_list(env = test_env))
  expect_true(any(grepl("Missing required variable", warnings)))
})

test_that("create_global_data_list() includes optional TV variables when present", {
  test_env <- new.env()
  test_env$fds_df <- data.frame(x = 1)
  test_env$tv_long <- data.frame(`Tumour Site` = "Lung", check.names = FALSE)
  test_env$tv_indicators <- data.frame(`Tumour Site` = "Lung", check.names = FALSE)

  result <- suppressWarnings(create_global_data_list(env = test_env))

  expect_true("tv_long" %in% names(result))
  expect_true("tv_indicators" %in% names(result))
})

# =============================================================================
# read_all_tumour_data() TESTS
# =============================================================================

test_that("read_all_tumour_data() processes multiple tumours", {
  # Create separate mock data for each tumour and merge
  lung_data <- create_mock_global_data(tumour_key = "lung")
  breast_data <- create_mock_global_data(tumour_key = "breast")

  # Merge all datasets by rbinding corresponding elements
  global_data <- lung_data
  for (nm in names(global_data)) {
    if (is.data.frame(global_data[[nm]]) && is.data.frame(breast_data[[nm]])) {
      global_data[[nm]] <- rbind(global_data[[nm]], breast_data[[nm]])
    }
  }

  # Add treatment variation data for Breast (which requires it)
  global_data$tv_long <- create_tv_long_fixture()
  global_data$tv_indicators <- create_tv_indicators_fixture()

  # Process just lung and breast (faster than all tumours)
  result <- suppressMessages(suppressWarnings(
    read_all_tumour_data(global_data, tumour_keys = c("lung", "breast"))
  ))

  expect_type(result, "list")
  expect_true("lung" %in% names(result))
  expect_true("breast" %in% names(result))
})

test_that("read_all_tumour_data() handles errors gracefully", {
  # Create minimal global data that will fail for some tumours
  global_data <- create_mock_global_data(tumour_key = "lung")

  # Should warn but not error
  expect_warning(
    result <- suppressMessages(
      read_all_tumour_data(global_data, tumour_keys = c("lung", "nonexistent"))
    )
  )

  # Lung should still work
  expect_true("lung" %in% names(result))
})

test_that("read_all_tumour_data() defaults tumour_keys to get_tumour_keys() when omitted", {
  global_data <- create_mock_global_data(tumour_key = "lung")
  global_data$tv_long <- create_tv_long_fixture()
  global_data$tv_indicators <- create_tv_indicators_fixture()

  # Every real tumour except "lung" fails against lung-only mock data and is
  # caught by read_all_tumour_data()'s tryCatch (one "Failed to process"
  # warning each) - if tumour_keys correctly defaulted to get_tumour_keys(),
  # that's every OTHER configured tumour, confirming the default actually ran
  # across all of them rather than, say, an empty vector.
  warnings_seen <- character()
  result <- withCallingHandlers(
    suppressMessages(read_all_tumour_data(global_data)),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(length(warnings_seen), length(get_tumour_keys()) - 1)
  expect_true("lung" %in% names(result))
})

# =============================================================================
# apply_cwt_provider_exclusions() TESTS
# =============================================================================

test_that("apply_cwt_provider_exclusions() returns global_data unchanged when the tumour has none", {
  global_data <- list(fds_df = data.frame(ORG.CODE = "RRK", Value = 1))
  config <- create_mock_config("Lung")
  config$cwt_provider_exclusions <- NULL

  result <- apply_cwt_provider_exclusions(global_data, config)

  expect_identical(result, global_data)
})

test_that("apply_cwt_provider_exclusions() removes matching rows and messages which were removed", {
  config <- create_mock_config("Head and Neck")
  config$cwt_provider_exclusions <- c("RRK")

  global_data <- list(
    fds_df = data.frame(ORG.CODE = c("RRK", "RKB", "RL4"), Value = 1:3),
    day_31_df = data.frame(ORG.CODE = c("RRK", "RKB"), Value = 1:2),
    # No ORG.CODE column - must be left alone, not error
    cwt_d_31_day_df = data.frame(ProviderCode = c("RKB"), Value = 1)
  )

  msgs <- testthat::capture_messages(
    result <- apply_cwt_provider_exclusions(global_data, config)
  )
  all_msgs <- paste(msgs, collapse = "\n")

  expect_false("RRK" %in% result$fds_df$ORG.CODE)
  expect_equal(nrow(result$fds_df), 2)
  expect_false("RRK" %in% result$day_31_df$ORG.CODE)
  expect_equal(nrow(result$day_31_df), 1)
  expect_equal(nrow(result$cwt_d_31_day_df), 1)  # untouched, no matching column

  expect_match(all_msgs, "fds_df: removed 1 rows \\(column: ORG\\.CODE\\)")
  expect_match(all_msgs, "day_31_df: removed 1 rows \\(column: ORG\\.CODE\\)")
})

# =============================================================================
# get_data_path_mapping() TESTS
# =============================================================================

test_that("get_data_path_mapping() returns a dataframe", {
  result <- get_data_path_mapping("lung")

  expect_s3_class(result, "data.frame")
  expect_true("old_name" %in% names(result))
  expect_true("new_path" %in% names(result))
})

test_that("get_data_path_mapping() has mappings for key variables", {
  result <- get_data_path_mapping("lung")

  # Check some key mappings exist
  expect_true("wmca_tumour_fds_df" %in% result$old_name)
  expect_true("wmca_tumour_31_df" %in% result$old_name)
  expect_true("wmca_tumour_62_df" %in% result$old_name)
  expect_true("wmca_tumour_standards_table_df" %in% result$old_name)
})

# =============================================================================
# END-TO-END SMOKE TESTS
# =============================================================================

test_that("Full pipeline runs without error for Lung", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  # Should complete without error
  expect_no_error({
    result <- suppressMessages(read_tumour_data("lung", global_data))
  })

  # Basic sanity checks
  expect_type(result, "list")
  expect_true(length(result) > 0)
})

test_that("Full pipeline runs without error for tumour with sub-types", {
  # Lower GI has sub-types (Colon, Rectal)
  global_data <- create_mock_global_data(tumour_key = "lower_gi")

  # Add sub-type staging data
  global_data$stage_df <- rbind(
    create_staging_demographics_fixture("Colon"),
    create_staging_demographics_fixture("Rectal")
  )

  expect_no_error({
    result <- suppressMessages(read_tumour_data("lower_gi", global_data))
  })
})

test_that("Full pipeline runs without error for a tumour with split staging completeness", {
  # Urology has a split staging_completeness (Urological, Prostate) - exercises
  # read_tumour_data()'s has_split(config, "staging_completeness") branch,
  # which no other full-pipeline test here reaches (process_staging_completeness_subtypes()
  # itself is unit-tested directly elsewhere, but not via the full read_tumour_data() path).
  global_data <- create_mock_global_data(tumour_key = "urology")
  global_data$stage_completeness_df <- create_staging_completeness_fixture(
    cancer_group = c("Urological", "Prostate")
  )

  expect_no_error({
    result <- suppressMessages(suppressWarnings(read_tumour_data("urology", global_data)))
  })

  expect_true(length(result$staging$completeness_subtypes) > 0)
  expect_true(!is.null(result$dates$staging))
})

test_that("Full pipeline runs without error for tumour without public CWT data", {
  # Brain & CNS doesn't have public CWT data
  global_data <- create_mock_global_data(tumour_key = "brain_cns")

  expect_no_error({
    result <- suppressMessages(suppressWarnings(
      read_tumour_data("brain_cns", global_data)
    ))
  })
})

test_that("Full pipeline runs without error for a tumour with a public-sourced standard (HPB-like)", {
  original_config <- tumour_config
  tumour_config$test_public_sourced_pipeline <<- create_mock_config(
    "HPB-like",
    cwt_31day_detailed = NULL, cwt_31day_public = "Specific Bucket",
    cwt_62day_detailed = NULL, cwt_62day_public = "Specific Bucket",
    cwt_62day_public_comm = "Specific Bucket"
  )
  on.exit({
    tumour_config <<- original_config
  })

  global_data <- create_mock_global_data(tumour_key = "test_public_sourced_pipeline")

  result <- NULL
  expect_no_error({
    result <- suppressMessages(suppressWarnings(
      read_tumour_data("test_public_sourced_pipeline", global_data)
    ))
  })

  # FDS stays detailed-sourced (not overridden) — Rule-in/out still build
  expect_false(is.null(result$fds$rule_in))
  # 31-day/62-day are public-sourced — no detailed data to build from
  expect_null(result$day_31$raw$detailed)
  expect_null(result$day_62$raw$detailed)
  expect_true(all(result$day_31$raw$public$CANCER.TYPE == "Specific Bucket"))
})

# =============================================================================
# OUTPUT STRUCTURE VALIDATION FOR RMARKDOWN
# =============================================================================

test_that("Output structure supports FDS slide data access patterns", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  # These are common access patterns in RMarkdown slides
  expect_no_error(result$fds$summary$provider)
  expect_no_error(result$fds$plot$compliance)
  expect_no_error(result$fds$table)
  expect_no_error(result$counts$fds_trusts_above)
  expect_no_error(result$dates$fds)
})

test_that("Output structure supports 62-day slide data access patterns", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  # Common 62-day access patterns
  expect_no_error(result$day_62$summary$provider)
  expect_no_error(result$day_62$plot$compliance)
  expect_no_error(result$day_62$table)
  expect_no_error(result$dates$day_62)
})

test_that("Output structure supports staging slide data access patterns", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  # Common staging access patterns
  if (!is.null(result$staging$completeness)) {
    expect_no_error(result$staging$completeness$plot)
    expect_no_error(result$staging$completeness$figure)
  }
})

test_that("Output structure supports benchmarking table access", {
  global_data <- create_mock_global_data(tumour_key = "lung")

  result <- suppressMessages(read_tumour_data("lung", global_data))

  expect_no_error(result$benchmarking$day_28)
  expect_no_error(result$standards_table)
})
