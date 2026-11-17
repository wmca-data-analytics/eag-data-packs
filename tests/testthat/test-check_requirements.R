# Tests for scripts/check_requirements.R
# =============================================================================
# check_requirements.R ends with `if (!interactive()) { ... quit(...) }`,
# which runs immediately when sourced under Rscript (testthat runs
# non-interactively, so interactive() is FALSE here too) and calls stop()
# when it doesn't see a recognised --check/--fix argument. All the functions
# we want to test are defined *before* that block, so we source the file into
# an isolated environment and swallow that expected stop() - the functions
# are already bound in the environment by the time it's thrown.
#
# We never call check_requirements()/fix_requirements() against the real
# project files - every test builds its own throwaway source tree so these
# tests can't mutate requirements.R or depend on the project's current
# package list.

load_check_requirements_env <- function() {
  env <- new.env(parent = parent.env(globalenv()))
  withr::with_dir(project_root, {
    try(sys.source("scripts/check_requirements.R", envir = env), silent = TRUE)
  })
  env
}

cr <- load_check_requirements_env()

test_that("sourcing the script defines the functions under test", {
  for (fn in c("scan_source_files", "read_required_packages",
               "fix_requirements", "check_requirements", "find_project_root")) {
    expect_true(is.function(cr[[fn]]), info = paste(fn, "should be defined"))
  }
})

# =============================================================================
# scan_source_files()
# =============================================================================

test_that("scan_source_files() finds library(), require(), requireNamespace() and :: usages", {
  dir <- withr::local_tempdir()
  writeLines(c(
    'library(dplyr)',
    'library("stringr")',
    "require(tidyr)",
    'requireNamespace("purrr")',
    "x <- cli::cli_alert_info('hi')",
    "y <- openxlsx:::write_data_internal()"
  ), file.path(dir, "script.R"))

  found <- cr$scan_source_files(dir)

  expect_setequal(found, c("dplyr", "stringr", "tidyr", "purrr", "cli", "openxlsx"))
})

test_that("scan_source_files() ignores requireNamespace calls matched as require()", {
  dir <- withr::local_tempdir()
  writeLines('requireNamespace("zoo")', file.path(dir, "script.R"))

  found <- cr$scan_source_files(dir)

  expect_equal(found, "zoo")
})

test_that("scan_source_files() strips comments before scanning", {
  dir <- withr::local_tempdir()
  writeLines("# library(dplyr) is just an example in this comment", file.path(dir, "script.R"))

  found <- cr$scan_source_files(dir)

  expect_length(found, 0)
})

test_that("scan_source_files() excludes base packages", {
  dir <- withr::local_tempdir()
  writeLines(c("library(stats)", "library(utils)", "library(dplyr)"),
             file.path(dir, "script.R"))

  found <- cr$scan_source_files(dir)

  expect_equal(found, "dplyr")
})

test_that("scan_source_files() scans .Rmd files too, recursively", {
  dir <- withr::local_tempdir()
  sub <- file.path(dir, "sub")
  dir.create(sub)
  writeLines("library(ggplot2)", file.path(sub, "report.Rmd"))

  found <- cr$scan_source_files(dir)

  expect_equal(found, "ggplot2")
})

test_that("scan_source_files() excludes requirements.R and itself", {
  dir <- withr::local_tempdir()
  writeLines('required_packages <- c("dplyr")', file.path(dir, "requirements.R"))
  writeLines("library(stringr)", file.path(dir, "check_requirements.R"))
  writeLines("library(purrr)", file.path(dir, "script.R"))

  found <- cr$scan_source_files(dir)

  expect_equal(found, "purrr")
})

# =============================================================================
# read_required_packages()
# =============================================================================

test_that("read_required_packages() parses a multi-line vector with comments", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "requirements.R")
  writeLines(c(
    "required_packages <- c(",
    "  # Data manipulation",
    '  "dplyr",',
    '  "tidyr",',
    "",
    "  # Testing",
    '  "testthat"',
    ")"
  ), path)

  pkgs <- cr$read_required_packages(path)

  expect_equal(pkgs, sort(c("dplyr", "tidyr", "testthat")))
})

test_that("read_required_packages() handles a single-line vector", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "requirements.R")
  writeLines('required_packages <- c("zoo", "cli")', path)

  pkgs <- cr$read_required_packages(path)

  expect_equal(pkgs, sort(c("zoo", "cli")))
})

test_that("read_required_packages() errors when it can't find the vector", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "requirements.R")
  writeLines("# no required_packages vector here", path)

  expect_error(cr$read_required_packages(path), "Could not parse required_packages")
})

# =============================================================================
# fix_requirements()
# =============================================================================

test_that("fix_requirements() adds a package when the closing paren shares the last entry's line", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "requirements.R")
  writeLines(c(
    "required_packages <- c(",
    '  "dplyr",',
    '  "zoo")'
  ), path)

  cr$fix_requirements(path, "cli")

  updated <- cr$read_required_packages(path)
  expect_equal(updated, sort(c("dplyr", "zoo", "cli")))

  # The closing paren must still be present and valid R.
  lines <- readLines(path)
  expect_true(any(grepl('"cli"', lines)))
  parsed <- parse(text = paste(lines, collapse = "\n"))
  expect_silent(eval(parsed, envir = new.env()))
})

test_that("fix_requirements() adds a package when the closing paren is on its own line", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "requirements.R")
  writeLines(c(
    "required_packages <- c(",
    '  "dplyr",',
    '  "zoo"',
    ")"
  ), path)

  cr$fix_requirements(path, "cli")

  updated <- cr$read_required_packages(path)
  expect_equal(updated, sort(c("dplyr", "zoo", "cli")))

  lines <- readLines(path)
  parsed <- parse(text = paste(lines, collapse = "\n"))
  expect_silent(eval(parsed, envir = new.env()))
})

test_that("fix_requirements() adds multiple packages, comma-separated", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "requirements.R")
  writeLines(c(
    "required_packages <- c(",
    '  "dplyr"',
    ")"
  ), path)

  cr$fix_requirements(path, c("cli", "zoo"))

  updated <- cr$read_required_packages(path)
  expect_equal(updated, sort(c("dplyr", "cli", "zoo")))

  lines <- readLines(path)
  parsed <- parse(text = paste(lines, collapse = "\n"))
  expect_silent(eval(parsed, envir = new.env()))
})

# =============================================================================
# check_requirements() - "check" and "fix" modes end-to-end
# =============================================================================

make_fake_project <- function(dir) {
  writeLines(c(
    "required_packages <- c(",
    '  "dplyr",',
    '  "fakeunused"',
    ")"
  ), file.path(dir, "requirements.R"))
  writeLines(c("library(dplyr)", "library(fakemissing)"), file.path(dir, "script.R"))
}

test_that("check_requirements('check') reports missing and unused packages without modifying requirements.R", {
  dir <- withr::local_tempdir()
  make_fake_project(dir)
  requirements_path <- file.path(dir, "requirements.R")
  before <- readLines(requirements_path)

  withr::with_dir(dir, {
    msgs <- testthat::capture_messages(cr$check_requirements("check"))
  })
  msgs <- paste(msgs, collapse = "\n")

  expect_true(grepl("MISSING.*fakemissing|fakemissing", msgs))
  expect_true(grepl("UNUSED.*fakeunused|fakeunused", msgs))
  expect_equal(readLines(requirements_path), before)
})

test_that("check_requirements('fix') adds the missing package and leaves unused ones for manual review", {
  dir <- withr::local_tempdir()
  make_fake_project(dir)
  requirements_path <- file.path(dir, "requirements.R")

  withr::with_dir(dir, {
    testthat::capture_messages(cr$check_requirements("fix"))
  })

  updated <- cr$read_required_packages(requirements_path)
  expect_true("fakemissing" %in% updated)
  expect_true("fakeunused" %in% updated)  # fix never removes, only adds
})

test_that("check_requirements('check') reports up to date when nothing is missing or unused", {
  dir <- withr::local_tempdir()
  writeLines('required_packages <- c("dplyr")', file.path(dir, "requirements.R"))
  writeLines("library(dplyr)", file.path(dir, "script.R"))

  withr::with_dir(dir, {
    msgs <- testthat::capture_messages(cr$check_requirements("check"))
  })

  expect_true(any(grepl("up to date", paste(msgs, collapse = "\n"))))
})
