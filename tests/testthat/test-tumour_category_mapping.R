# Tests for R/tumour_category_mapping.R

# =============================================================================
# get_dataset_category_values()
# =============================================================================

test_that("get_dataset_category_values() reads each dataframe's own values, never merged", {
  global_data <- list(
    # Benchmarking and detailed extracts deliberately given DIFFERENT values,
    # so any accidental merging between them would be caught here.
    latest_public_2_months_df = data.frame(
      STANDARD = c("FDS", "31D", "62D"),
      CANCER.TYPE = c("Suspected lung cancer", "Lung", "Head & Neck"),
      stringsAsFactors = FALSE
    ),
    cwt_d_fds_df = data.frame(
      Cancer.Report.Category = c("Suspected lung cancer", "Suspected lung cancer", "Suspected breast cancer"),
      stringsAsFactors = FALSE
    ),
    cwt_d_31_day_df = data.frame(CancerReportCategory = c("Lung", "Breast"), stringsAsFactors = FALSE),
    cwt_d_62_day_df = data.frame(CancerReportCategory = c("Lung", "Head & Neck"), stringsAsFactors = FALSE),
    stage_completeness_df = data.frame(CANCER_GROUP_NM = c("Melanoma Skin", "Lower GI"), stringsAsFactors = FALSE),
    stage_df = data.frame(`Cancer group` = c("Melanoma", "Colon", "Rectal"), check.names = FALSE, stringsAsFactors = FALSE),
    tumour_pack_lwbc_plot_df = data.frame(`Tumour Group` = c("Lower GI", "Urological"), check.names = FALSE, stringsAsFactors = FALSE),
    tv_long = data.frame(`Tumour Site` = c("Lung", "Breast"), check.names = FALSE, stringsAsFactors = FALSE)
  )

  result <- get_dataset_category_values(global_data)

  expect_named(
    result,
    c("cwt_fds_public", "cwt_31day_public", "cwt_62day_public",
      "cwt_fds_provider", "cwt_31day_provider", "cwt_62day_provider", "cwt_62day_public_comm",
      "cwt_fds_detailed", "cwt_31day_detailed", "cwt_62day_detailed",
      "staging_completeness", "staging_demographics", "lwbc", "treatment_variation")
  )

  # Benchmarking values come only from latest_public_2_months_df, per STANDARD
  expect_equal(result$cwt_fds_public, "Suspected lung cancer")
  expect_equal(result$cwt_31day_public, "Lung")
  expect_equal(result$cwt_62day_public, "Head & Neck")

  expect_equal(result$cwt_fds_detailed, c("Suspected breast cancer", "Suspected lung cancer"))
  expect_equal(result$cwt_31day_detailed, c("Breast", "Lung"))
  expect_equal(result$staging_demographics, c("Colon", "Melanoma", "Rectal"))
  expect_equal(result$lwbc, c("Lower GI", "Urological"))
  expect_equal(result$treatment_variation, c("Breast", "Lung"))
})

test_that("get_dataset_category_values() degrades to character(0) for missing data, not an error", {
  result <- get_dataset_category_values(list())

  expect_equal(result$cwt_fds_public, character(0))
  expect_equal(result$staging_demographics, character(0))
  expect_equal(result$lwbc, character(0))
  expect_equal(result$treatment_variation, character(0))

  # A dataframe present but missing the expected column is handled the same way
  result2 <- get_dataset_category_values(list(stage_df = data.frame(x = 1)))
  expect_equal(result2$staging_demographics, character(0))
})


# =============================================================================
# build_category_mapping_table()
# =============================================================================

test_that("build_category_mapping_table() shows detailed and benchmarking rows (lung)", {
  global_data <- create_mock_global_data(tumour_key = "lung")
  dataset_values <- get_dataset_category_values(global_data)

  tbl <- build_category_mapping_table("lung", dataset_values)

  expect_true(all(c("Tumour", "Dataset field", "Selected value(s)", "All possible values") %in% names(tbl)))
  expect_setequal(
    tbl$`Dataset field`,
    c("cwt_fds_detailed", "cwt_31day_detailed", "cwt_62day_detailed",
      "cwt_fds_public", "cwt_31day_public", "cwt_62day_public",
      "staging_completeness", "staging_demographics", "lwbc", "treatment_variation")
  )
  expect_true(all(tbl$Tumour == "Lung"))

  fds_row <- tbl[tbl$`Dataset field` == "cwt_fds_public", ]
  expect_equal(fds_row$`Selected value(s)`, get_name_in_dataset(get_tumour_config("lung"), "cwt_fds_public"))
  expect_equal(fds_row$`All possible values`, paste(dataset_values$cwt_fds_public, collapse = "; "))
})

test_that("build_category_mapping_table() omits opted-out benchmarking and absent-data rows (nss)", {
  global_data <- create_mock_global_data(tumour_key = "nss")
  dataset_values <- get_dataset_category_values(global_data)

  tbl <- build_category_mapping_table("nss", dataset_values)

  # nss opts out of 31-day/62-day benchmarking (display_benchmarking_31day/
  # 62day false) and has no staging, lwbc or treatment_variation data at all
  expect_false("cwt_31day_public" %in% tbl$`Dataset field`)
  expect_false("cwt_62day_public" %in% tbl$`Dataset field`)
  expect_false("staging_demographics" %in% tbl$`Dataset field`)
  expect_false("staging_completeness" %in% tbl$`Dataset field`)
  expect_false("lwbc" %in% tbl$`Dataset field`)
  expect_false("treatment_variation" %in% tbl$`Dataset field`)

  # cwt_fds_public IS present: nss doesn't opt out of FDS benchmarking
  expect_true("cwt_fds_public" %in% tbl$`Dataset field`)

  expect_setequal(
    tbl$`Dataset field`,
    c("cwt_fds_detailed", "cwt_31day_detailed", "cwt_62day_detailed", "cwt_fds_public")
  )

  fds_detailed_row <- tbl[tbl$`Dataset field` == "cwt_fds_detailed", ]
  expect_equal(
    fds_detailed_row$`Selected value(s)`,
    get_name_in_dataset(get_tumour_config("nss"), "cwt_fds_detailed")
  )
})

test_that("build_category_mapping_table() labels split rows with their own raw value (urology)", {
  global_data <- create_mock_global_data(tumour_key = "urology")
  dataset_values <- get_dataset_category_values(global_data)

  tbl <- build_category_mapping_table("urology", dataset_values)

  staging_tumours <- tbl$Tumour[tbl$`Dataset field` == "staging_demographics"]
  completeness_tumours <- tbl$Tumour[tbl$`Dataset field` == "staging_completeness"]
  benchmarking_31_tumours <- tbl$Tumour[tbl$`Dataset field` == "cwt_31day_public"]

  expect_setequal(staging_tumours, c("Urological excl. prostate", "Prostate"))
  expect_setequal(completeness_tumours, c("Urological", "Prostate"))
  expect_setequal(benchmarking_31_tumours, c("Urological - Other (a)", "Urological - Prostate"))
  expect_false("Urology" %in% staging_tumours)
})

test_that("build_category_mapping_table() keeps a non-split field as one combined row (lower_gi)", {
  global_data <- create_mock_global_data(tumour_key = "lower_gi")
  dataset_values <- get_dataset_category_values(global_data)

  tbl <- build_category_mapping_table("lower_gi", dataset_values)

  staging_tumours <- tbl$Tumour[tbl$`Dataset field` == "staging_demographics"]
  completeness_tumours <- tbl$Tumour[tbl$`Dataset field` == "staging_completeness"]

  expect_setequal(staging_tumours, c("Colon", "Rectal"))
  expect_equal(completeness_tumours, "Lower Gastrointestinal")
})

test_that("build_category_mapping_table() omits a field entirely when its config value is null", {
  cfg <- create_mock_config("Test Tumour", cwt_public_data_exists_for_benchmarking = FALSE, lwbc = NULL)
  original_config <- tumour_config
  tumour_config$test_null_fields <<- cfg
  on.exit({
    tumour_config <<- original_config
  })

  global_data <- create_mock_global_data(tumour_key = "test_null_fields")
  dataset_values <- get_dataset_category_values(global_data)

  tbl <- build_category_mapping_table("test_null_fields", dataset_values)

  expect_false("lwbc" %in% tbl$`Dataset field`)
  expect_false("cwt_fds_public" %in% tbl$`Dataset field`)
  expect_false("cwt_31day_public" %in% tbl$`Dataset field`)
  expect_false("cwt_62day_public" %in% tbl$`Dataset field`)
})

test_that("build_category_mapping_table() shows a provider row (not a detailed row) for a public-sourced standard", {
  cfg <- create_mock_config(
    "HPB-like",
    cwt_31day_detailed = NULL, cwt_31day_public = "Specific Bucket",
    cwt_62day_detailed = NULL, cwt_62day_public = "Specific Bucket",
    cwt_62day_public_comm = "Specific Bucket"
  )
  original_config <- tumour_config
  tumour_config$test_public_sourced <<- cfg
  on.exit({
    tumour_config <<- original_config
  })

  global_data <- create_mock_global_data(tumour_key = "test_public_sourced")
  dataset_values <- get_dataset_category_values(global_data)

  tbl <- build_category_mapping_table("test_public_sourced", dataset_values)

  # 31-day/62-day are public-sourced: a "_provider" row appears (the own-
  # figures dataframe), not a "_detailed" row
  expect_true("cwt_31day_provider" %in% tbl$`Dataset field`)
  expect_true("cwt_62day_provider" %in% tbl$`Dataset field`)
  expect_false("cwt_31day_detailed" %in% tbl$`Dataset field`)
  expect_false("cwt_62day_detailed" %in% tbl$`Dataset field`)
  expect_true("cwt_62day_public_comm" %in% tbl$`Dataset field`)

  # FDS is still detailed-sourced (not overridden above)
  expect_true("cwt_fds_detailed" %in% tbl$`Dataset field`)
})
