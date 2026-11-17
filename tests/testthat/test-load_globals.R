# Tests for R/load_globals.R
# =============================================================================
# load_globals.R is sourced once in setup.R (not inside a test_that block,
# since it assigns into .GlobalEnv at source time). These tests check:
# 1. The globals it produces match config/globals.yml (already sourced).
# 2. .find_config_dir() resolves correctly from different working dirs.
# 3. Sourcing into an isolated environment still assigns into .GlobalEnv
#    (the loader hard-codes envir = .GlobalEnv, regardless of where it's
#    sourced from).

# =============================================================================
# Globals loaded into .GlobalEnv
# =============================================================================

test_that("load_globals.R assigns every config/globals.yml key as a global", {
  raw <- yaml::read_yaml(file.path(project_root, "config", "globals.yml"))

  for (key in names(raw)) {
    expect_true(
      exists(key, envir = .GlobalEnv, inherits = FALSE),
      info = paste("Expected global variable", key, "to be set")
    )
  }
})

test_that("integer-looking YAML values are loaded as double, not integer", {
  # The .yaml_handlers int-coercion in load_globals.R exists specifically so
  # these stay `double` (matching the original hand-written R constants),
  # not `integer`.
  expect_type(CWT_STANDARD_FDS_PC, "double")
  expect_false(is.integer(CWT_STANDARD_FDS_PC))
  expect_equal(CWT_STANDARD_FDS_PC, 80)
})

test_that("decimal standards and their percentage counterparts are consistent", {
  expect_equal(CWT_STANDARD_FDS, CWT_STANDARD_FDS_PC / 100)
  expect_equal(CWT_STANDARD_31DAY, CWT_STANDARD_31DAY_PC / 100)
  expect_equal(CWT_STANDARD_62DAY, CWT_STANDARD_62DAY_PC / 100)
})

# =============================================================================
# .find_config_dir()
# =============================================================================

test_that(".find_config_dir() finds config/ from the project root", {
  withr::local_dir(project_root)
  expect_equal(.find_config_dir(), "config")
})

test_that(".find_config_dir() finds config/ from one level down", {
  withr::local_dir(file.path(project_root, "R"))
  expect_equal(.find_config_dir(), file.path("..", "config"))
})

test_that(".find_config_dir() finds config/ from two levels down", {
  withr::local_dir(file.path(project_root, "tests", "testthat"))
  expect_equal(.find_config_dir(), file.path("..", "..", "config"))
})

test_that(".find_config_dir() errors when no config/ dir is reachable", {
  withr::local_dir(tempdir())
  expect_error(.find_config_dir(), "Could not locate config/ directory")
})

# =============================================================================
# Sourcing behaviour
# =============================================================================

test_that("sourcing load_globals.R always assigns into .GlobalEnv, even from an isolated environment", {
  withr::local_dir(project_root)

  isolated_env <- new.env(parent = parent.env(globalenv()))
  # Prove the loader doesn't just assign into its own sourcing environment.
  sys.source("R/load_globals.R", envir = isolated_env)

  expect_false(exists("CWT_STANDARD_FDS", envir = isolated_env, inherits = FALSE))
  expect_true(exists("CWT_STANDARD_FDS", envir = .GlobalEnv, inherits = FALSE))
})
