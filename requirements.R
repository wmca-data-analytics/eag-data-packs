# requirements.R
# =============================================================================
# Single source of truth for all R package dependencies in this project.
#
# Usage:
#   Rscript requirements.R
#
# This will install any packages that are not already installed.
# It will also highlight any packages listed for installation which are not
# called in the code.
#
# Note this is also used to install packages on the DevOps CI VM.
# =============================================================================

# Package repositories — Posit Package Manager provides pre-compiled Linux
# binaries (faster CI installs), with CRAN as fallback.
options(repos = c(
  PPM  = "https://packagemanager.posit.co/cran/latest",
  CRAN = "https://cloud.r-project.org/"
))

# All packages required by this project
required_packages <- c(
  # Data manipulation
  "dplyr",
  "tidyr",
  "tidyverse",
  "purrr",
  "stringr",
  "lubridate",
  "forcats",
  "zoo",

  # File I/O
  "readxl",
  "writexl",
  "openxlsx",

  # CLI output
  "cli",

  # Testing
  "testthat",
  "cyclocomp",

  # Reporting / RMarkdown
  "rmarkdown",
  "knitr",
  "flextable",
  "officer",
  "svglite",

  # Visualisation
  "ggplot2",
  "ggrepel",
  "ggpubr",
  "scales",
  "NHSRplotthedots"
)

# Install any missing packages
missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)

  # Verify all packages installed successfully
  still_missing <- missing[!sapply(missing, requireNamespace, quietly = TRUE)]
  if (length(still_missing) > 0) {
    stop(
      "The following packages failed to install: ",
      paste(still_missing, collapse = ", "),
      call. = FALSE
    )
  }
} else {
  message("All packages are already installed.")
}
