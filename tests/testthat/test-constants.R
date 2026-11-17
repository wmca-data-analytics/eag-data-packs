# Tests for constants.R configuration
# =============================================================================
# These tests validate that the configuration in constants.R is correct and
# internally consistent. They don't test functions (constants.R has none),
# but ensure the data structures are valid.

# These tests need both alliances loaded; see load_all_alliance_configs() in
# setup.R for why this is called here as well as at setup time.
load_all_alliance_configs()

# -----------------------------------------------------------------------------
# CWT Standards tests
# -----------------------------------------------------------------------------

test_that("CWT standards are defined as global variables", {
  expect_true(exists("CWT_STANDARD_FDS"))
  expect_true(exists("CWT_STANDARD_31DAY"))
  expect_true(exists("CWT_STANDARD_62DAY"))
  expect_true(exists("CWT_STANDARD_FDS_PC"))
  expect_true(exists("CWT_STANDARD_31DAY_PC"))
  expect_true(exists("CWT_STANDARD_62DAY_PC"))
})

test_that("CWT standards have correct decimal values", {
  expect_equal(CWT_STANDARD_FDS, 0.80)
  expect_equal(CWT_STANDARD_31DAY, 0.96)
  expect_equal(CWT_STANDARD_62DAY, 0.85)
})

test_that("CWT percentage standards match decimal standards", {
  expect_equal(CWT_STANDARD_FDS_PC, CWT_STANDARD_FDS * 100)
  expect_equal(CWT_STANDARD_31DAY_PC, CWT_STANDARD_31DAY * 100)
  expect_equal(CWT_STANDARD_62DAY_PC, CWT_STANDARD_62DAY * 100)
})

test_that("CWT standards are in valid range (0-1 for decimal, 0-100 for percentage)", {
  expect_true(CWT_STANDARD_FDS >= 0 && CWT_STANDARD_FDS <= 1)
  expect_true(CWT_STANDARD_31DAY >= 0 && CWT_STANDARD_31DAY <= 1)
  expect_true(CWT_STANDARD_62DAY >= 0 && CWT_STANDARD_62DAY <= 1)

  expect_true(CWT_STANDARD_FDS_PC >= 0 && CWT_STANDARD_FDS_PC <= 100)
  expect_true(CWT_STANDARD_31DAY_PC >= 0 && CWT_STANDARD_31DAY_PC <= 100)
  expect_true(CWT_STANDARD_62DAY_PC >= 0 && CWT_STANDARD_62DAY_PC <= 100)
})

# -----------------------------------------------------------------------------
# Staging targets tests
# -----------------------------------------------------------------------------

test_that("Staging targets are defined", {
  expect_true(exists("STAGING_COMPLETENESS_TARGET"))
  expect_true(exists("STAGING_COMPLETENESS_TARGET_PC"))
  expect_true(exists("EARLY_STAGING_TARGET_PC"))
})

test_that("Staging completeness target percentage matches decimal", {
  expect_equal(STAGING_COMPLETENESS_TARGET_PC, STAGING_COMPLETENESS_TARGET * 100)
})

# -----------------------------------------------------------------------------
# Treatment Variation targets tests
# -----------------------------------------------------------------------------

test_that("Treatment variation targets are defined", {
  expect_true(exists("TV_LUNG_TARGET"))
  expect_true(exists("TV_LOWER_GI_TARGET"))
  expect_true(exists("TV_BREAST_TARGET"))
  expect_true(exists("TV_GYNAE_TARGET"))
})

test_that("Treatment variation targets are in valid percentage range", {
  expect_true(TV_LUNG_TARGET >= 0 && TV_LUNG_TARGET <= 100)
  expect_true(TV_LOWER_GI_TARGET >= 0 && TV_LOWER_GI_TARGET <= 100)
  expect_true(TV_BREAST_TARGET >= 0 && TV_BREAST_TARGET <= 100)
  expect_true(TV_GYNAE_TARGET >= 0 && TV_GYNAE_TARGET <= 100)
})

# -----------------------------------------------------------------------------
# Alliance configuration tests
# -----------------------------------------------------------------------------

test_that(".alliance_configs exists and contains expected alliances", {
  expect_true(exists(".alliance_configs"))
  expect_type(.alliance_configs, "list")
  expect_true("WMCA" %in% names(.alliance_configs))
  expect_true("EMCA" %in% names(.alliance_configs))
})

test_that("Each alliance has required metadata fields", {
  required_fields <- c("code", "alliance", "meeting_name", "initial")

  for (alliance_name in names(.alliance_configs)) {
    config <- .alliance_configs[[alliance_name]]

    for (field in required_fields) {
      expect_true(
        field %in% names(config),
        info = paste(alliance_name, "should have field:", field)
      )
      expect_true(
        !is.null(config[[field]]) && nchar(config[[field]]) > 0,
        info = paste(alliance_name, field, "should not be empty")
      )
    }
  }
})

test_that("Each alliance has trust_codes as a character vector", {
  for (alliance_name in names(.alliance_configs)) {
    config <- .alliance_configs[[alliance_name]]

    expect_true("trust_codes" %in% names(config))
    expect_type(config$trust_codes, "character")
    expect_true(length(config$trust_codes) > 0)
  }
})

test_that("WMCA has 15 trusts, EMCA has 8 trusts", {
  expect_equal(length(.alliance_configs$WMCA$trust_codes), 15)
  expect_equal(length(.alliance_configs$EMCA$trust_codes), 8)
})

test_that("Each alliance has org_lookup dataframe with required columns", {
  required_cols <- c("ORG.CODE", "ORG.NAME")

  for (alliance_name in names(.alliance_configs)) {
    config <- .alliance_configs[[alliance_name]]

    expect_true("org_lookup" %in% names(config))
    expect_s3_class(config$org_lookup, "data.frame")

    for (col in required_cols) {
      expect_true(
        col %in% names(config$org_lookup),
        info = paste(alliance_name, "org_lookup should have column:", col)
      )
    }
  }
})

test_that("org_lookup ORG.CODE matches trust_codes", {
  for (alliance_name in names(.alliance_configs)) {
    config <- .alliance_configs[[alliance_name]]

    lookup_codes <- config$org_lookup$ORG.CODE
    trust_codes <- config$trust_codes

    # All trust_codes should be in org_lookup
    expect_true(
      all(trust_codes %in% lookup_codes),
      info = paste(alliance_name, "- all trust_codes should be in org_lookup")
    )

    # org_lookup should not have extra codes
    expect_true(
      all(lookup_codes %in% trust_codes),
      info = paste(alliance_name, "- org_lookup should not have codes not in trust_codes")
    )
  }
})

test_that("Each alliance has geography maps with valid org codes", {
  map_names <- c("stage_geography_map", "lwbc_geography_map", "tv_geography_map")

  for (alliance_name in names(.alliance_configs)) {
    config <- .alliance_configs[[alliance_name]]
    trust_codes <- config$trust_codes

    for (map_name in map_names) {
      expect_true(
        map_name %in% names(config),
        info = paste(alliance_name, "should have", map_name)
      )

      map <- config[[map_name]]
      expect_type(map, "character")
      expect_true(length(map) > 0,
        info = paste(alliance_name, map_name, "should not be empty"))

      # All values (org codes) should be valid trust codes
      expect_true(
        all(map %in% trust_codes),
        info = paste(alliance_name, "-", map_name,
          "values should be valid trust codes. Invalid:",
          paste(setdiff(map, trust_codes), collapse = ", "))
      )
    }
  }
})

test_that("Each alliance has icb_recode_map", {
  for (alliance_name in names(.alliance_configs)) {
    config <- .alliance_configs[[alliance_name]]

    expect_true("icb_recode_map" %in% names(config))
    expect_type(config$icb_recode_map, "character")
  }
})

test_that("Each alliance has trust_to_icb mapping", {
  for (alliance_name in names(.alliance_configs)) {
    config <- .alliance_configs[[alliance_name]]

    expect_true("trust_to_icb" %in% names(config))
    expect_type(config$trust_to_icb, "character")
    expect_true(length(config$trust_to_icb) > 0)
  }
})

test_that("WMCA tv_config has required fields for long-format read-in", {
  required_tv_fields <- c(
    "file_path", "all_data_sheet", "indicator_sheet", "indicator_range",
    "alliance_name", "needs_percentage_conversion"
  )

  config <- .alliance_configs$WMCA
  expect_true("tv_config" %in% names(config))
  expect_type(config$tv_config, "list")

  for (field in required_tv_fields) {
    expect_true(
      field %in% names(config$tv_config),
      info = paste("WMCA tv_config should have field:", field)
    )
  }
})

# -----------------------------------------------------------------------------
# WMCA-specific tests
# -----------------------------------------------------------------------------

test_that("WMCA has correct metadata", {
  wmca <- .alliance_configs$WMCA

  expect_equal(wmca$code, "WMCA")
  expect_equal(wmca$alliance, "West Midlands")
  expect_equal(wmca$meeting_name, "EAG")
  expect_equal(wmca$initial, "W")
})

test_that("WMCA trust_codes include known major trusts", {
  wmca_codes <- .alliance_configs$WMCA$trust_codes

  # Check for some known WMCA trusts
  expect_true("RRK" %in% wmca_codes)  # UHB
  expect_true("RKB" %in% wmca_codes)  # UHCW
  expect_true("RL4" %in% wmca_codes)  # Royal Wolverhampton
})

# -----------------------------------------------------------------------------
# EMCA-specific tests
# -----------------------------------------------------------------------------

test_that("EMCA has correct metadata", {
  emca <- .alliance_configs$EMCA

  expect_equal(emca$code, "EMCA")
  expect_equal(emca$alliance, "East Midlands")
  expect_equal(emca$meeting_name, "ECAG")
  expect_equal(emca$initial, "E")
})

test_that("EMCA trust_codes include known major trusts", {
  emca_codes <- .alliance_configs$EMCA$trust_codes

  # Check for some known EMCA trusts
  expect_true("RX1" %in% emca_codes)  # Nottingham
  expect_true("RWE" %in% emca_codes)  # Leicester
})

# -----------------------------------------------------------------------------
# Cross-alliance consistency tests
# -----------------------------------------------------------------------------

test_that("Alliance trust codes do not overlap", {
  wmca_codes <- .alliance_configs$WMCA$trust_codes
  emca_codes <- .alliance_configs$EMCA$trust_codes

  overlap <- intersect(wmca_codes, emca_codes)

  expect_equal(
    length(overlap), 0,
    info = paste("Overlapping codes found:", paste(overlap, collapse = ", "))
  )
})

test_that("Alliance tv_config file paths are different", {
  wmca_path <- .alliance_configs$WMCA$tv_config$file_path
  emca_path <- .alliance_configs$EMCA$tv_config$file_path

  expect_false(wmca_path == emca_path)
})
