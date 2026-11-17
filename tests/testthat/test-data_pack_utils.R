# Tests for data_pack_utils.R functions
# =============================================================================

# These tests need both alliances loaded; see load_all_alliance_configs() in
# setup.R for why this is called here as well as at setup time.
load_all_alliance_configs()

# -----------------------------------------------------------------------------
# calc_cwt_compliance() tests
# -----------------------------------------------------------------------------

test_that("calc_cwt_compliance() calculates correct percentage", {
  # Simple case: 80 out of 100 = 80%
  expect_equal(calc_cwt_compliance(80, 100), 80.0)

  # Another simple case: 8 out of 10 = 80%
  expect_equal(calc_cwt_compliance(8, 10), 80.0)

  # Non-round numbers
  expect_equal(calc_cwt_compliance(73, 100), 73.0)

  # Rounding to 1 decimal place
  expect_equal(calc_cwt_compliance(22, 30), 73.3)  # 73.333... rounds to 73.3
})

test_that("calc_cwt_compliance() handles half-patients (62-day standard)", {
  # 62-day standard uses half-patients
  expect_equal(calc_cwt_compliance(22.5, 142.5), 15.8)  # 15.789... rounds to 15.8

  # More half-patient examples
  expect_equal(calc_cwt_compliance(10.5, 20), 52.5)
  expect_equal(calc_cwt_compliance(15.5, 31), 50.0)
})

test_that("calc_cwt_compliance() returns NA for zero denominator", {
  expect_true(is.na(calc_cwt_compliance(0, 0)))
  expect_true(is.na(calc_cwt_compliance(10, 0)))
})

test_that("calc_cwt_compliance() handles vector inputs", {
  within <- c(80, 90, 75)
  total <- c(100, 100, 100)

  result <- calc_cwt_compliance(within, total)

  expect_equal(result, c(80.0, 90.0, 75.0))
})

test_that("calc_cwt_compliance() handles mixed vector with zeros", {
  within <- c(80, 90, 0)
  total <- c(100, 0, 100)

  result <- calc_cwt_compliance(within, total)

  expect_equal(result[1], 80.0)
  expect_true(is.na(result[2]))  # Division by zero
  expect_equal(result[3], 0.0)   # 0/100 = 0%
})

test_that("calc_cwt_compliance() handles 100% compliance", {
  expect_equal(calc_cwt_compliance(100, 100), 100.0)
  expect_equal(calc_cwt_compliance(50, 50), 100.0)
})

test_that("calc_cwt_compliance() handles 0% compliance", {
  expect_equal(calc_cwt_compliance(0, 100), 0.0)
  expect_equal(calc_cwt_compliance(0, 50), 0.0)
})

# -----------------------------------------------------------------------------
# calc_staging_completeness() tests
# -----------------------------------------------------------------------------

test_that("calc_staging_completeness() calculates correct percentage", {
  # Simple case: 80 out of 100 = 80%
  expect_equal(calc_staging_completeness(80, 100), 80.0)

  # 8 out of 10 = 80%
  expect_equal(calc_staging_completeness(8, 10), 80.0)
})

test_that("calc_staging_completeness() returns 0 for zero denominator", {
  # Note: This function returns 0 instead of NA for zero denominator
  expect_equal(calc_staging_completeness(0, 0), 0)
  expect_equal(calc_staging_completeness(10, 0), 0)
})

test_that("calc_staging_completeness() rounds to 3 decimal places", {
  # 1/3 = 33.333...% should round to 33.333
  result <- calc_staging_completeness(1, 3)
  expect_equal(round(result, 3), round(33.333, 3))
})

# -----------------------------------------------------------------------------
# num_is_whole_half() tests
# -----------------------------------------------------------------------------

test_that("num_is_whole_half() identifies whole numbers", {
  expect_true(num_is_whole_half(5))
  expect_true(num_is_whole_half(0))
  expect_true(num_is_whole_half(100))
  expect_true(num_is_whole_half(-5))
})

test_that("num_is_whole_half() identifies half numbers", {
  expect_true(num_is_whole_half(3.5))
  expect_true(num_is_whole_half(0.5))
  expect_true(num_is_whole_half(100.5))
  expect_true(num_is_whole_half(-2.5))
})

test_that("num_is_whole_half() rejects other decimals", {
  expect_false(num_is_whole_half(2.3))
  expect_false(num_is_whole_half(5.7))
  expect_false(num_is_whole_half(0.25))
  expect_false(num_is_whole_half(3.14159))
})

test_that("num_is_whole_half() handles vectors", {
  result <- num_is_whole_half(c(1, 1.5, 2.3, 3, 4.5))
  expect_equal(result, c(TRUE, TRUE, FALSE, TRUE, TRUE))
})

# -----------------------------------------------------------------------------
# ceiling_to_nearest_half() tests
# -----------------------------------------------------------------------------

test_that("ceiling_to_nearest_half() rounds up correctly", {
  expect_equal(ceiling_to_nearest_half(1.2), 1.5)
  expect_equal(ceiling_to_nearest_half(16.8), 17.0)
  expect_equal(ceiling_to_nearest_half(72), 72)
  expect_equal(ceiling_to_nearest_half(1.1), 1.5)
  expect_equal(ceiling_to_nearest_half(1.6), 2.0)
})

test_that("ceiling_to_nearest_half() keeps whole/half numbers unchanged", {
  expect_equal(ceiling_to_nearest_half(5), 5)
  expect_equal(ceiling_to_nearest_half(5.5), 5.5)
  expect_equal(ceiling_to_nearest_half(0), 0)
})

# -----------------------------------------------------------------------------
# round_to_nearest_half() tests
# -----------------------------------------------------------------------------

test_that("round_to_nearest_half() rounds correctly", {
  expect_equal(round_to_nearest_half(1.2), 1.0)   # 1.2 * 2 = 2.4 -> 2 -> 1.0
  expect_equal(round_to_nearest_half(1.6), 1.5)   # 1.6 * 2 = 3.2 -> 3 -> 1.5
  expect_equal(round_to_nearest_half(2.7), 2.5)   # 2.7 * 2 = 5.4 -> 5 -> 2.5
  expect_equal(round_to_nearest_half(3.1), 3.0)   # 3.1 * 2 = 6.2 -> 6 -> 3.0
  expect_equal(round_to_nearest_half(3.5), 3.5)   # 3.5 * 2 = 7.0 -> 7 -> 3.5
  expect_equal(round_to_nearest_half(3.9), 4.0)   # 3.9 * 2 = 7.8 -> 8 -> 4.0
})

# -----------------------------------------------------------------------------
# get_62_day_target_as_patients() tests
# -----------------------------------------------------------------------------

test_that("get_62_day_target_as_patients() calculates correct targets", {
  # Need CWT_STANDARD_62DAY constant (0.85)
  # 22 * 0.85 = 18.7 -> ceiling to 19
  result <- get_62_day_target_as_patients(22)
  expect_true(num_is_whole_half(result))

  # 100 * 0.85 = 85 (already whole)
  expect_equal(get_62_day_target_as_patients(100), 85)

  # 65 * 0.85 = 55.25 -> ceiling to 55.5
  result_65 <- get_62_day_target_as_patients(65)
  expect_true(num_is_whole_half(result_65))
})

# -----------------------------------------------------------------------------
# get_cwt_targets_as_patients() tests
# -----------------------------------------------------------------------------

test_that("get_cwt_targets_as_patients() handles FDS standard", {
  # FDS target is 80% (CWT_STANDARD_FDS = 0.8)
  # 53 * 0.8 = 42.4 -> ceiling to 43
  expect_equal(get_cwt_targets_as_patients("fds", 53), 43)
  expect_equal(get_cwt_targets_as_patients("FDS", 100), 80)
  expect_equal(get_cwt_targets_as_patients("28-day FDS", 100), 80)
})

test_that("get_cwt_targets_as_patients() handles 31-day standard", {
  # 31-day target is 96% (CWT_STANDARD_31DAY = 0.96)
  # 123 * 0.96 = 118.08 -> ceiling to 119
  expect_equal(get_cwt_targets_as_patients("31 day", 123), 119)
  expect_equal(get_cwt_targets_as_patients("31-day", 100), 96)
  expect_equal(get_cwt_targets_as_patients("31D", 100), 96)
})

test_that("get_cwt_targets_as_patients() handles 62-day standard", {
  # 62-day target is 85% (CWT_STANDARD_62DAY = 0.85)
  # Uses half-patient rounding
  result <- get_cwt_targets_as_patients("62 day", 78)
  expect_true(num_is_whole_half(result))

  expect_equal(get_cwt_targets_as_patients("62-day", 100), 85)
})

test_that("get_cwt_targets_as_patients() returns NA for zero patients", {
  expect_true(is.na(get_cwt_targets_as_patients("fds", 0)))
  expect_true(is.na(get_cwt_targets_as_patients("31 day", 0)))
  expect_true(is.na(get_cwt_targets_as_patients("62 day", 0)))
})

# -----------------------------------------------------------------------------
# get_pt_diff_friendly() tests
# -----------------------------------------------------------------------------

test_that("get_pt_diff_friendly() formats positive numbers correctly", {
  expect_equal(get_pt_diff_friendly(54), "54 above")
  expect_equal(get_pt_diff_friendly(1), "1 above")
})

test_that("get_pt_diff_friendly() formats negative numbers correctly", {
  expect_equal(get_pt_diff_friendly(-3), "3 below")
  expect_equal(get_pt_diff_friendly(-10), "10 below")
})

test_that("get_pt_diff_friendly() handles zero", {
  expect_equal(get_pt_diff_friendly(0), "met std")
})

# -----------------------------------------------------------------------------
# parse_financial_year_date() tests
# -----------------------------------------------------------------------------

test_that("parse_financial_year_date() handles start-of-year months", {
  # April 2024/25 should be 2024-04-01
  result <- parse_financial_year_date("APR 2024/25")
  expect_equal(result, as.Date("2024-04-01"))

  # December 2024/25 should be 2024-12-01
  result <- parse_financial_year_date("DEC 2024/25")
  expect_equal(result, as.Date("2024-12-01"))
})

test_that("parse_financial_year_date() handles end-of-year months", {
  # January 2024/25 should be 2025-01-01 (end year of financial year)
  result <- parse_financial_year_date("JAN 2024/25")
  expect_equal(result, as.Date("2025-01-01"))

  # February 2024/25 should be 2025-02-01
  result <- parse_financial_year_date("FEB 2024/25")
  expect_equal(result, as.Date("2025-02-01"))

  # March 2024/25 should be 2025-03-01
  result <- parse_financial_year_date("MAR 2024/25")
  expect_equal(result, as.Date("2025-03-01"))
})

test_that("parse_financial_year_date() handles vectors", {
  dates <- c("APR 2024/25", "JAN 2024/25", "MAR 2024/25")
  result <- parse_financial_year_date(dates)

  expect_equal(result[1], as.Date("2024-04-01"))
  expect_equal(result[2], as.Date("2025-01-01"))
  expect_equal(result[3], as.Date("2025-03-01"))
})

# -----------------------------------------------------------------------------
# date_to_fiscal_q() tests
# -----------------------------------------------------------------------------

test_that("date_to_fiscal_q() returns correct fiscal quarters", {
  # April is Q1
  expect_equal(date_to_fiscal_q(as.Date("2024-04-15")), "Q1 24/25")

  # July is Q2
  expect_equal(date_to_fiscal_q(as.Date("2024-07-01")), "Q2 24/25")

  # October is Q3
  expect_equal(date_to_fiscal_q(as.Date("2024-10-31")), "Q3 24/25")

  # January is Q4 (of the previous financial year start)
  expect_equal(date_to_fiscal_q(as.Date("2025-01-15")), "Q4 24/25")
})

# -----------------------------------------------------------------------------
# fiscal_q_months_to_date() tests
# -----------------------------------------------------------------------------

test_that("fiscal_q_months_to_date() labels the quarter's months up to the date", {
  # First month of a quarter
  expect_equal(fiscal_q_months_to_date(as.Date("2026-04-01")), "Apr")

  # Part way through a quarter
  expect_equal(fiscal_q_months_to_date(as.Date("2026-05-01")), "Apr-May")

  # Last month of a quarter, mid-month date
  expect_equal(fiscal_q_months_to_date(as.Date("2026-06-15")), "Apr-Jun")

  # Each later quarter, including Q4 across the calendar year end
  expect_equal(fiscal_q_months_to_date(as.Date("2026-08-01")), "Jul-Aug")
  expect_equal(fiscal_q_months_to_date(as.Date("2026-12-31")), "Oct-Dec")
  expect_equal(fiscal_q_months_to_date(as.Date("2027-01-01")), "Jan")
  expect_equal(fiscal_q_months_to_date(as.Date("2027-03-01")), "Jan-Mar")
})

# =============================================================================
# Alliance Accessor Function Tests
# =============================================================================

# -----------------------------------------------------------------------------
# get_current_alliance() tests
# -----------------------------------------------------------------------------

test_that("get_current_alliance() returns alliance when passed explicitly", {
  expect_equal(get_current_alliance("wmca"), "WMCA")
  expect_equal(get_current_alliance("WMCA"), "WMCA")
  expect_equal(get_current_alliance("emca"), "EMCA")
  expect_equal(get_current_alliance("EMCA"), "EMCA")
})

test_that("get_current_alliance() reads from global environment", {
  # Save any existing alliance value
  had_alliance <- exists("alliance", envir = .GlobalEnv)
  if (had_alliance) {
    old_alliance <- get("alliance", envir = .GlobalEnv)
  }

  # Set alliance in global env
  assign("alliance", "WMCA", envir = .GlobalEnv)
  expect_equal(get_current_alliance(), "WMCA")

  assign("alliance", "emca", envir = .GlobalEnv)
  expect_equal(get_current_alliance(), "EMCA")

  # Restore original state
  if (had_alliance) {
    assign("alliance", old_alliance, envir = .GlobalEnv)
  } else {
    rm("alliance", envir = .GlobalEnv)
  }
})

test_that("get_current_alliance() errors when no alliance set", {
  # Save any existing alliance value
  had_alliance <- exists("alliance", envir = .GlobalEnv)
  if (had_alliance) {
    old_alliance <- get("alliance", envir = .GlobalEnv)
    rm("alliance", envir = .GlobalEnv)
  }

  expect_error(get_current_alliance(), "Global 'alliance' variable not set")

  # Restore original state
  if (had_alliance) {
    assign("alliance", old_alliance, envir = .GlobalEnv)
  }
})

# -----------------------------------------------------------------------------
# get_alliance_config() tests
# -----------------------------------------------------------------------------

test_that("get_alliance_config() returns full config for valid alliance", {
  config <- get_alliance_config(alliance = "WMCA")

  expect_type(config, "list")
  expect_true("trust_codes" %in% names(config))
  expect_true("org_lookup" %in% names(config))
  expect_true("alliance" %in% names(config))
})

test_that("get_alliance_config() returns specific key when requested", {
  alliance_name <- get_alliance_config(key = "alliance", alliance = "WMCA")
  expect_equal(alliance_name, "West Midlands")

  alliance_name_emca <- get_alliance_config(key = "alliance", alliance = "EMCA")
  expect_equal(alliance_name_emca, "East Midlands")
})

test_that("get_alliance_config() errors for unknown alliance", {
  expect_error(
    get_alliance_config(alliance = "INVALID"),
    "Unknown alliance"
  )
})

test_that("get_alliance_config() errors for unknown key", {
  expect_error(
    get_alliance_config(key = "nonexistent_key", alliance = "WMCA"),
    "Unknown config key"
  )
})

# -----------------------------------------------------------------------------
# get_org_recode_map() tests
# -----------------------------------------------------------------------------

test_that("get_org_recode_map() returns named character vector", {
  recode_map <- get_org_recode_map(alliance = "WMCA")

  expect_type(recode_map, "character")
  expect_true(!is.null(names(recode_map)))
  expect_true(length(recode_map) > 0)
})

# -----------------------------------------------------------------------------
# get_trust_to_icb() tests
# -----------------------------------------------------------------------------

test_that("get_trust_to_icb() returns named character vector", {
  mapping <- get_trust_to_icb(alliance = "WMCA")

  expect_type(mapping, "character")
  expect_true(!is.null(names(mapping)))
})

# -----------------------------------------------------------------------------
# get_source_name_map() tests
# -----------------------------------------------------------------------------

test_that("get_source_name_map() returns named character vector", {
  map <- get_source_name_map("stage_geography_map", alliance = "WMCA")

  expect_type(map, "character")
  expect_true(!is.null(names(map)))
  expect_true(length(map) > 0)
})

test_that("get_source_name_map() resolves codes to display names from org_lookup", {
  map <- get_source_name_map("stage_geography_map", alliance = "WMCA")
  lookup <- get_alliance_config("org_lookup", alliance = "WMCA")

  # Values should be display names, not codes
  expect_true(all(map %in% lookup$ORG.NAME))
})

test_that("get_source_name_map() works for all geography maps", {
  for (map_name in c("stage_geography_map", "lwbc_geography_map", "tv_geography_map")) {
    for (alliance_name in c("WMCA", "EMCA")) {
      map <- get_source_name_map(map_name, alliance = alliance_name)

      expect_true(is.character(map),
        info = paste(alliance_name, map_name))
      expect_true(length(map) > 0,
        info = paste(alliance_name, map_name, "should not be empty"))
    }
  }
})

test_that("get_source_name_map() errors for invalid map name", {
  expect_error(
    get_source_name_map("nonexistent_map", alliance = "WMCA"),
    "Unknown config key"
  )
})

# -----------------------------------------------------------------------------
# get_tv_config() tests
# -----------------------------------------------------------------------------

test_that("get_tv_config() returns full config when no key specified", {
  tv_config <- get_tv_config(alliance = "WMCA")

  expect_type(tv_config, "list")
  expect_true("file_path" %in% names(tv_config))
  expect_true("all_data_sheet" %in% names(tv_config))
})

test_that("get_tv_config() returns specific key when requested", {
  file_path <- get_tv_config(key = "file_path", alliance = "WMCA")

  expect_type(file_path, "character")
  expect_true(nchar(file_path) > 0)
})

test_that("get_tv_config() errors for unknown key", {
  expect_error(
    get_tv_config(key = "invalid_key", alliance = "WMCA"),
    "Unknown TV config key"
  )
})

# -----------------------------------------------------------------------------
# needs_tv_percentage_conversion() tests
# -----------------------------------------------------------------------------

test_that("needs_tv_percentage_conversion() returns logical", {
  result <- needs_tv_percentage_conversion(alliance = "WMCA")

  expect_type(result, "logical")
})

# =============================================================================
# Complex Compliance/Benchmark Function Tests
# =============================================================================

# -----------------------------------------------------------------------------
# add_compliance_rank() tests
# -----------------------------------------------------------------------------

test_that("add_compliance_rank() adds compliance and rank columns", {
  test_data <- data.frame(
    PERIOD = as.Date(c("2024-04-01", "2024-04-01", "2024-04-01")),
    ORG.CODE = c("RRK", "RKB", "RL4"),
    WITHIN.STANDARD = c(80, 90, 75),
    TOTAL.TREATED = c(100, 100, 100)
  )

  result <- add_compliance_rank(test_data)

  expect_true("Compliance_%" %in% names(result))
  expect_true("RANK" %in% names(result))
})

test_that("add_compliance_rank() calculates correct compliance", {
  test_data <- data.frame(
    PERIOD = as.Date(c("2024-04-01", "2024-04-01")),
    ORG.CODE = c("RRK", "RKB"),
    WITHIN.STANDARD = c(80, 45),
    TOTAL.TREATED = c(100, 50)
  )

  result <- add_compliance_rank(test_data)

  expect_equal(result$`Compliance_%`[1], 80)
  expect_equal(result$`Compliance_%`[2], 90)
})

test_that("add_compliance_rank() ranks highest compliance as 1", {
  test_data <- data.frame(
    PERIOD = as.Date(c("2024-04-01", "2024-04-01", "2024-04-01")),
    ORG.CODE = c("RRK", "RKB", "RL4"),
    WITHIN.STANDARD = c(80, 90, 75),
    TOTAL.TREATED = c(100, 100, 100)
  )

  result <- add_compliance_rank(test_data)

  # RKB has 90% compliance, should be rank 1
  rkb_row <- result[result$ORG.CODE == "RKB", ]
  expect_equal(rkb_row$RANK, 1)

  # RRK has 80%, should be rank 2
  rrk_row <- result[result$ORG.CODE == "RRK", ]
  expect_equal(rrk_row$RANK, 2)
})

test_that("add_compliance_rank() handles zero total treated", {
  test_data <- data.frame(
    PERIOD = as.Date(c("2024-04-01", "2024-04-01")),
    ORG.CODE = c("RRK", "RKB"),
    WITHIN.STANDARD = c(80, 0),
    TOTAL.TREATED = c(100, 0)
  )

  result <- add_compliance_rank(test_data)

  expect_equal(result$`Compliance_%`[1], 80)
  expect_true(is.na(result$`Compliance_%`[2]))
})

# -----------------------------------------------------------------------------
# add_rank_change() tests
# -----------------------------------------------------------------------------

test_that("add_rank_change() calculates rank differences", {
  test_data <- data.frame(
    PERIOD = as.Date(c(
      "2024-04-01", "2024-04-01",
      "2024-05-01", "2024-05-01"
    )),
    ORG.CODE = c("RRK", "RKB", "RRK", "RKB"),
    WITHIN.STANDARD = c(80, 90, 90, 80),
    TOTAL.TREATED = c(100, 100, 100, 100)
  )

  ranked_data <- add_compliance_rank(test_data)
  result <- add_rank_change(ranked_data)

  # Should only return latest period
  expect_equal(nrow(result), 2)

  # RRK improved from rank 2 to rank 1
  rrk_row <- result[result$ORG.CODE == "RRK", ]
  expect_equal(rrk_row$RANK_DIFF, 1)
  expect_true(grepl("↑", rrk_row$RANK_ARROW))

  # RKB dropped from rank 1 to rank 2
  rkb_row <- result[result$ORG.CODE == "RKB", ]
  expect_equal(rkb_row$RANK_DIFF, -1)
  expect_true(grepl("↓", rkb_row$RANK_ARROW))
})

test_that("add_rank_change() returns latest period only", {
  test_data <- data.frame(
    PERIOD = as.Date(c(
      "2024-04-01", "2024-05-01", "2024-06-01"
    )),
    ORG.CODE = c("RRK", "RRK", "RRK"),
    WITHIN.STANDARD = c(80, 85, 90),
    TOTAL.TREATED = c(100, 100, 100)
  )

  ranked_data <- add_compliance_rank(test_data)
  result <- add_rank_change(ranked_data)

  expect_equal(nrow(result), 1)
  expect_equal(result$PERIOD[1], as.Date("2024-06-01"))
})

test_that("add_rank_change() shows arrow for no change", {
  test_data <- data.frame(
    PERIOD = as.Date(c("2024-04-01", "2024-05-01")),
    ORG.CODE = c("RRK", "RRK"),
    WITHIN.STANDARD = c(80, 80),
    TOTAL.TREATED = c(100, 100)
  )

  ranked_data <- add_compliance_rank(test_data)
  result <- add_rank_change(ranked_data)

  expect_equal(result$RANK_ARROW[1], "→ 0")
})

# -----------------------------------------------------------------------------
# add_national_alliance_rows() tests
# -----------------------------------------------------------------------------

test_that("add_national_alliance_rows() prepends average rows", {
  trust_df <- data.frame(
    ORG.CODE = c("RRK", "RKB"),
    ORG.NAME = c("Trust A", "Trust B"),
    `Latest FDS Compliance (%)` = c(80, 90),
    `Difference from the target (no. patients)` = c(5, 10),
    `National FDS Ranking` = c(2, 1),
    `Ranking difference from previous month` = c("↑1", "↓1"),
    check.names = FALSE
  )

  national_df <- data.frame(
    `Compliance_%` = 85,
    `Patients_Above_Target` = 100,
    check.names = FALSE
  )

  alliance_df <- data.frame(
    `Compliance_%` = 87,
    `Patients_Above_Target` = 50,
    check.names = FALSE
  )

  result <- add_national_alliance_rows(trust_df, national_df, alliance_df)

  # Should have 4 rows total (2 averages + 2 trusts)
  expect_equal(nrow(result), 4)

  # First two rows should be the averages
  expect_equal(result$ORG.NAME[1], "National Average")
  expect_equal(result$ORG.NAME[2], "Alliance Average")

  # Values should be correct
  expect_equal(result$`Latest FDS Compliance (%)`[1], 85)
  expect_equal(result$`Latest FDS Compliance (%)`[2], 87)
})

test_that("add_national_alliance_rows() accepts custom labels", {
  trust_df <- data.frame(
    ORG.CODE = "RRK",
    ORG.NAME = "Trust A",
    `Latest FDS Compliance (%)` = 80,
    `Difference from the target (no. patients)` = 5,
    `National FDS Ranking` = 1,
    `Ranking difference from previous month` = "→0",
    check.names = FALSE
  )

  national_df <- data.frame(
    `Compliance_%` = 85,
    `Patients_Above_Target` = 100,
    check.names = FALSE
  )

  alliance_df <- data.frame(
    `Compliance_%` = 87,
    `Patients_Above_Target` = 50,
    check.names = FALSE
  )

  result <- add_national_alliance_rows(
    trust_df, national_df, alliance_df,
    label_national = "England Average",
    label_alliance = "WMCA Average"
  )

  expect_equal(result$ORG.NAME[1], "England Average")
  expect_equal(result$ORG.NAME[2], "WMCA Average")
})

# =============================================================================
# split_cancer_group() tests
# =============================================================================

test_that("split_cancer_group() splits dataframe by cancer group", {
  test_df <- data.frame(
    `Cancer group` = c("Lung", "Lung", "Breast", "Breast", "Skin"),
    Value = c(1, 2, 3, 4, 5),
    check.names = FALSE
  )

  result <- split_cancer_group(test_df)

  expect_type(result, "list")
  expect_equal(length(result), 3)
  expect_true("Lung" %in% names(result))
  expect_true("Breast" %in% names(result))
  expect_true("Skin" %in% names(result))
})

test_that("split_cancer_group() preserves rows for each group", {
  test_df <- data.frame(
    `Cancer group` = c("Lung", "Lung", "Breast"),
    Value = c(1, 2, 3),
    check.names = FALSE
  )

  result <- split_cancer_group(test_df)

  expect_equal(nrow(result$Lung), 2)
  expect_equal(nrow(result$Breast), 1)
})

test_that("split_cancer_group() handles single group", {
  test_df <- data.frame(
    `Cancer group` = c("Lung", "Lung", "Lung"),
    Value = c(1, 2, 3),
    check.names = FALSE
  )

  result <- split_cancer_group(test_df)

  expect_equal(length(result), 1)
  expect_equal(names(result), "Lung")
  expect_equal(nrow(result$Lung), 3)
})


# =============================================================================
# standard_compliance_calc() tests
# =============================================================================

test_that("standard_compliance_calc() trust level returns ORG.CODE column", {
  assign("alliance", "WMCA", envir = .GlobalEnv)
  data <- create_benchmarking_fixture("Lung")

  result <- standard_compliance_calc(data, "FDS", "Lung", "trust")

  expect_true("ORG.CODE" %in% names(result))
  expect_true("PERIOD" %in% names(result))
  expect_true("WITHIN.STANDARD" %in% names(result))
  expect_true("TOTAL.TREATED" %in% names(result))
  expect_false("Patients_Above_Target" %in% names(result))
})

test_that("standard_compliance_calc() national level returns single row with Patients_Above_Target", {
  assign("alliance", "WMCA", envir = .GlobalEnv)
  data <- create_benchmarking_fixture("Lung")

  result <- standard_compliance_calc(data, "FDS", "Lung", "national")

  expect_true("Patients_Above_Target" %in% names(result))
  expect_equal(nrow(result), 1)
})

test_that("standard_compliance_calc() alliance level returns single row with Patients_Above_Target", {
  assign("alliance", "WMCA", envir = .GlobalEnv)
  data <- create_benchmarking_fixture("Lung")

  result <- standard_compliance_calc(data, "FDS", "Lung", "alliance")

  expect_true("Patients_Above_Target" %in% names(result))
  expect_equal(nrow(result), 1)
})

test_that("standard_compliance_calc() cancer type filter reduces totals", {
  assign("alliance", "WMCA", envir = .GlobalEnv)
  data <- create_benchmarking_fixture(c("Lung", "Breast"))

  result_lung <- standard_compliance_calc(data, "FDS", "Lung", "trust")
  result_all  <- standard_compliance_calc(data, "FDS", NULL, "trust")

  expect_true(sum(result_lung$TOTAL.TREATED) < sum(result_all$TOTAL.TREATED))
})

test_that("standard_compliance_calc() 31-day excludes non-qualifying STAGE.ROUTE", {
  assign("alliance", "WMCA", envir = .GlobalEnv)
  data <- create_benchmarking_fixture("Lung")
  extra <- data[1, ]
  extra$STANDARD <- "31D"
  extra$STAGE.ROUTE <- "INVALID"
  extra$TREATMENT.MODALITY <- "ALL MODALITIES"
  extra$WITHIN.STANDARD <- 9999
  data <- rbind(data, extra)

  result <- standard_compliance_calc(data, "31D", "Lung", "trust")

  expect_true(all(result$WITHIN.STANDARD != 9999))
})

test_that("standard_compliance_calc() 62-day excludes non-ALL MODALITIES rows", {
  assign("alliance", "WMCA", envir = .GlobalEnv)
  data <- create_benchmarking_fixture("Lung")
  extra <- data[1, ]
  extra$STANDARD <- "62D"
  extra$TREATMENT.MODALITY <- "SURGERY ONLY"
  extra$WITHIN.STANDARD <- 9999
  data <- rbind(data, extra)

  result <- standard_compliance_calc(data, "62D", "Lung", "trust")

  expect_true(all(result$WITHIN.STANDARD != 9999))
})

test_that("standard_compliance_calc() 62-day excludes non-ALL ROUTES rows", {
  assign("alliance", "WMCA", envir = .GlobalEnv)
  data <- create_benchmarking_fixture("Lung")
  extra <- data[1, ]
  extra$STANDARD <- "62D"
  extra$STAGE.ROUTE <- "Screening"
  extra$TREATMENT.MODALITY <- "ALL MODALITIES"
  extra$WITHIN.STANDARD <- 9999
  data <- rbind(data, extra)

  result <- standard_compliance_calc(data, "62D", "Lung", "trust")

  expect_true(all(result$WITHIN.STANDARD != 9999))
})


# =============================================================================
# tumour_benchmark_table() tests
# =============================================================================

test_that("tumour_benchmark_table() returns dataframe containing alliance trusts", {
  assign("alliance", "WMCA", envir = .GlobalEnv)
  data <- create_benchmarking_fixture("Lung")

  result <- tumour_benchmark_table(data, "FDS", "Lung", "trust")

  expect_s3_class(result, "data.frame")
  expect_true("ORG.CODE" %in% names(result))
  expect_true("ORG.NAME" %in% names(result))
  expect_true(all(DEFAULT_TRUSTS %in% result$ORG.CODE))
})

test_that("tumour_benchmark_table() has renamed compliance columns", {
  assign("alliance", "WMCA", envir = .GlobalEnv)
  data <- create_benchmarking_fixture("Lung")

  result <- tumour_benchmark_table(data, "FDS", "Lung", "trust")

  expect_true("Latest FDS Compliance (%)" %in% names(result))
  expect_true("Difference from the target (no. patients)" %in% names(result))
  expect_true("National FDS Ranking" %in% names(result))
  expect_true("Ranking difference from previous month" %in% names(result))
})


# =============================================================================
# calc_fds_end_reason_compliance() tests
# =============================================================================

test_that("calc_fds_end_reason_compliance() returns correct column names with em-dashes", {
  assign("alliance", "WMCA", envir = .GlobalEnv)
  data <- create_latest_cwt_detailed_fixture("Lung")

  result <- calc_fds_end_reason_compliance(data, "Lung")

  expect_true("Provider" %in% names(result))
  expect_true("Diagnosis – Compliance_%" %in% names(result))
  expect_true("Diagnosis – Patients above target" %in% names(result))
  expect_true("Ruling out – Compliance_%" %in% names(result))
  expect_true("Ruling out – Patients above target" %in% names(result))
})

test_that("calc_fds_end_reason_compliance() returns one row per trust", {
  assign("alliance", "WMCA", envir = .GlobalEnv)
  data <- create_latest_cwt_detailed_fixture("Lung")

  result <- calc_fds_end_reason_compliance(data, "Lung")

  expect_equal(nrow(result), length(DEFAULT_TRUSTS))
})

test_that("calc_fds_end_reason_compliance() cancer type filter excludes non-matching rows", {
  assign("alliance", "WMCA", envir = .GlobalEnv)
  data <- create_latest_cwt_detailed_fixture("Lung")

  result <- calc_fds_end_reason_compliance(data, "Breast")

  expect_equal(nrow(result), 0)
})

test_that("calc_fds_end_reason_compliance() NULL cancer_type includes all providers", {
  assign("alliance", "WMCA", envir = .GlobalEnv)
  data <- create_latest_cwt_detailed_fixture("Lung")

  result <- calc_fds_end_reason_compliance(data)

  expect_equal(nrow(result), length(DEFAULT_TRUSTS))
})



# lwbc_dual_axis_scale() --------------------------------------------------

lwbc_scale_df <- function(hna, pcsp, eots) {
  tibble::tibble(
    `% HNA's Accepted` = hna,
    `% PCSP's Completed` = pcsp,
    `No. EOTS's Completed` = eots
  )
}

test_that("lwbc_dual_axis_scale() puts the EOTS peak level with the HNA peak", {
  result <- lwbc_dual_axis_scale(lwbc_scale_df(c(20, 40), c(50, 60), c(100, 200)))

  expect_equal(result$scale_factor, 200 / 40)
  expect_equal(result$y_max, 60 * 1.1)
})

test_that("lwbc_dual_axis_scale() limit covers an HNA peak above the PCSP peak", {
  result <- lwbc_dual_axis_scale(lwbc_scale_df(c(80, 90), c(50, 60), c(10, 20)))

  expect_equal(result$y_max, 90 * 1.1)
})

test_that("lwbc_dual_axis_scale() is finite when no HNAs were accepted", {
  result <- lwbc_dual_axis_scale(lwbc_scale_df(c(0, 0), c(30, 40), c(5, 10)))

  expect_equal(result$scale_factor, 10 / 40)
  expect_equal(result$y_max, 40 * 1.1)
})

test_that("lwbc_dual_axis_scale() is finite when every value is zero", {
  result <- lwbc_dual_axis_scale(lwbc_scale_df(c(0, 0), c(0, 0), c(0, 0)))

  expect_equal(result$scale_factor, 1)
  expect_equal(result$y_max, 100 * 1.1)
})

test_that("lwbc_dual_axis_scale() is finite with no rows or only NA values", {
  empty <- lwbc_dual_axis_scale(lwbc_scale_df(numeric(), numeric(), numeric()))
  all_na <- lwbc_dual_axis_scale(lwbc_scale_df(NA_real_, NA_real_, NA_real_))

  expect_equal(empty, list(scale_factor = 1, y_max = 100 * 1.1))
  expect_equal(all_na, list(scale_factor = 1, y_max = 100 * 1.1))
})

# =============================================================================
# calc_axis_limit()
# =============================================================================

test_that("calc_axis_limit() rounds up to the nearest 10 below 100", {
  expect_equal(calc_axis_limit(42), 50)
  expect_equal(calc_axis_limit(0), 0)
  expect_equal(calc_axis_limit(99), 100)
})

test_that("calc_axis_limit() rounds up to the nearest 50 between 100 and 1000", {
  expect_equal(calc_axis_limit(100), 100)
  expect_equal(calc_axis_limit(420), 450)
  expect_equal(calc_axis_limit(999), 1000)
})

test_that("calc_axis_limit() rounds up to the nearest 100 at or above 1000", {
  expect_equal(calc_axis_limit(1000), 1000)
  expect_equal(calc_axis_limit(1240), 1300)
})
