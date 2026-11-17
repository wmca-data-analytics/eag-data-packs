# Setup file for testthat
# This file is automatically sourced before tests run

# Load required packages
library(testthat)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(lubridate)
  library(readxl)
})

# Load optional packages (some tests may be skipped if not available)
has_zoo <- requireNamespace("zoo", quietly = TRUE)
if (has_zoo) {
  library(zoo)
}

# Find project root - works whether running from project root or tests/testthat
find_project_root <- function() {
  # Start with current directory and walk up
  candidates <- c(
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "..", ".."),
    Sys.getenv("TESTTHAT_PROJECT_ROOT", unset = NA)
  )

  for (path in candidates) {
    if (is.na(path)) next
    path <- tryCatch(normalizePath(path, mustWork = TRUE), error = function(e) NA)
    if (is.na(path)) next

    r_dir <- file.path(path, "R")
    if (dir.exists(r_dir) && file.exists(file.path(r_dir, "load_globals.R"))) {
      return(path)
    }
  }

  stop("Could not find project root. Ensure R/load_globals.R exists.")
}

project_root <- find_project_root()
message("Project root: ", project_root)

# Save original working directory and change to project root
# This is needed because some R files use relative source() calls
original_wd <- getwd()
setwd(project_root)
on.exit(setwd(original_wd), add = TRUE)

# Source files in dependency order (now using relative paths from project root)
source("R/load_globals.R")

# Default alliance for tests. R/load_alliance_config.R requires `alliance` to
# be set and populates .alliance_configs with just that one entry. Tests that
# rely on accessing BOTH alliances (e.g. cross-alliance consistency checks)
# overwrite .alliance_configs below with both, using the loader function.
# Assign to .GlobalEnv explicitly because source() below defaults to
# local = FALSE — the file is evaluated in .GlobalEnv where this variable
# must already exist.
assign("alliance", "WMCA", envir = .GlobalEnv)
source("R/load_alliance_config.R")

# Expand .alliance_configs with all alliances for test cross-alliance checks.
# Production code (main.R) only ever loads the active alliance; the tests are
# the only place that needs both at once.
#
# This is a function, not a one-off assignment, because covr::file_coverage()
# (used by the CI pipeline) sources every R/*.R file AFTER this setup file.
# Re-sourcing R/load_alliance_config.R re-runs its top-level `<<-`, which
# overwrites the global .alliance_configs with the single active alliance.
# Test files that need both alliances therefore call this at their top, so
# they do not depend on setup-time state surviving.
load_all_alliance_configs <- function() {
  assign(".alliance_configs",
         list(WMCA = load_alliance_config("WMCA"),
              EMCA = load_alliance_config("EMCA")),
         envir = .GlobalEnv)
}
load_all_alliance_configs()

source("R/validation_utils.R")
source("R/load_tumour_config.R")
source("R/data_pack_utils.R")
source("R/read_in_utils.R")
source("R/tumour_processing.R")
source("R/read_tumour_data.R")
source("R/tumour_category_mapping.R")
source("R/pptx_section_subtitles.R")

# Source fixture factories
source("tests/testthat/fixtures/create_processing_fixtures.R")

# Helper function to get path to test fixtures
get_fixture_path <- function(filename) {
  file.path(project_root, "tests", "testthat", "fixtures", filename)
}
