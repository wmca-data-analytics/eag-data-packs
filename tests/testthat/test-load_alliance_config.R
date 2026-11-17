# Tests for R/load_alliance_config.R
# =============================================================================

# =============================================================================
# load_alliance_config() - post-processed structure
# =============================================================================

test_that("load_alliance_config('WMCA') post-processes fields to the documented shapes", {
  cfg <- load_alliance_config("WMCA")

  expect_equal(cfg$code, "WMCA")

  expect_type(cfg$trust_codes, "character")
  expect_length(cfg$trust_codes, 15)
  expect_true("RRJ" %in% cfg$trust_codes)

  expect_true(is.data.frame(cfg$org_lookup))
  expect_equal(nrow(cfg$org_lookup), 15)
  expect_setequal(names(cfg$org_lookup), c("ORG.CODE", "ORG.NAME", "ORG.ABBR"))

  for (field in c("stage_geography_map", "lwbc_geography_map", "tv_geography_map",
                   "icb_recode_map", "trust_to_icb")) {
    map <- cfg[[field]]
    expect_type(map, "character")
    expect_true(!is.null(names(map)), info = paste(field, "should be a named vector"))
  }

  expect_equal(
    unname(cfg$trust_to_icb["RRK"]),
    "Birmingham & Solihull ICB"
  )
})

test_that("load_alliance_config() is case-insensitive on the alliance code", {
  expect_equal(load_alliance_config("wmca"), load_alliance_config("WMCA"))
  expect_equal(load_alliance_config("EmCa"), load_alliance_config("EMCA"))
})

test_that("load_alliance_config() errors for an unknown alliance code, listing what's available", {
  expect_error(
    load_alliance_config("ZZZZ"),
    regexp = "Alliance 'ZZZZ' not found.*Available: EMCA, WMCA|Available: WMCA, EMCA"
  )
})

test_that("tv_config is left untouched by post-processing, including nested named lists", {
  wmca <- load_alliance_config("WMCA")
  expect_type(wmca$tv_config, "list")
  expect_false(wmca$tv_config$needs_percentage_conversion)
  expect_equal(wmca$tv_config$alliance_name, "West Midlands Cancer Alliance")

  # EMCA's tv_config$sheet_names must stay a named list (not flattened to a
  # vector), because consumers access it with `$` (e.g. sheet_names$lung).
  emca <- load_alliance_config("EMCA")
  expect_true(is.list(emca$tv_config$sheet_names))
  expect_false(is.null(names(emca$tv_config$sheet_names)))
  expect_equal(emca$tv_config$sheet_names$lung, "Lung")
})

# =============================================================================
# .unlist_named_map()
# =============================================================================

test_that(".unlist_named_map() returns NULL for NULL input", {
  expect_null(.unlist_named_map(NULL))
})

test_that(".unlist_named_map() converts a named list to a named character/numeric vector", {
  result <- .unlist_named_map(list(a = "x", b = "y"))
  expect_equal(result, c(a = "x", b = "y"))
})

# =============================================================================
# Sourcing behaviour - the `alliance` global and .alliance_configs
# =============================================================================

test_that("sourcing load_alliance_config.R errors when `alliance` is not set anywhere reachable", {
  withr::local_dir(project_root)

  isolated_env <- new.env(parent = parent.env(globalenv()))
  expect_error(
    sys.source("R/load_alliance_config.R", envir = isolated_env),
    "'alliance' variable not set"
  )
})

test_that("sourcing load_alliance_config.R populates .alliance_configs keyed by the active alliance only", {
  withr::local_dir(project_root)
  # The file's trailing `.alliance_configs <<- ...` is a superassignment: if
  # no existing `.alliance_configs` binding is found while walking up the
  # sourcing environment's lexical parents, R creates it in .GlobalEnv
  # directly - regardless of the `envir` passed to sys.source(). This mirrors
  # the warning in setup.R that re-sourcing this file overwrites the global
  # .alliance_configs, so we save/restore it here rather than relying on
  # isolation that the code doesn't actually provide.
  original_configs <- .GlobalEnv$.alliance_configs
  on.exit(assign(".alliance_configs", original_configs, envir = .GlobalEnv), add = TRUE)

  isolated_env <- new.env(parent = parent.env(globalenv()))
  assign("alliance", "EMCA", envir = isolated_env)
  sys.source("R/load_alliance_config.R", envir = isolated_env)

  configs <- .GlobalEnv$.alliance_configs
  expect_named(configs, "EMCA")
  expect_equal(configs$EMCA$code, "EMCA")
})

test_that("sourcing load_alliance_config.R uppercases a lowercase `alliance` value", {
  withr::local_dir(project_root)
  original_configs <- .GlobalEnv$.alliance_configs
  on.exit(assign(".alliance_configs", original_configs, envir = .GlobalEnv), add = TRUE)

  isolated_env <- new.env(parent = parent.env(globalenv()))
  assign("alliance", "wmca", envir = isolated_env)
  sys.source("R/load_alliance_config.R", envir = isolated_env)

  configs <- .GlobalEnv$.alliance_configs
  expect_named(configs, "WMCA")
})
