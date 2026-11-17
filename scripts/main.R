# Clear console ----
start <- proc.time()
cat("\f")

# Libraries ----
library(dplyr)
library(purrr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(readxl)
library(writexl)
library(stringr)
library(cli)
library(zoo)

# Suppress dplyr::summarise() info
options(dplyr.summarise.inform = FALSE)


# Set options ----

## Working directory ----
# All file paths are defined from *the root* of the project
setwd(".")

## Required alliance ----
alliance <- "WMCA"

# Clear console ----
cat("\f")

# Load utilities & config ----
# Note: load_alliance_config.R reads the `alliance` variable set above and
#       populates .alliance_configs, which is used by accessor functions in
#       data_pack_utils.R. load_globals.R is independent and can be sourced
#       either before or after.
cli_alert_info("Loading utilities and config...")
source("R/load_globals.R")
source("R/load_alliance_config.R")
source("R/load_tumour_config.R")

# Load helper functions
source("R/data_pack_utils.R")
source("R/read_in_utils.R")

# Load tumour data consolidation system ----
source("R/read_tumour_data.R")
source("R/tumour_processing.R")
source("R/tumour_category_mapping.R")
source("R/pptx_section_subtitles.R")

## Alliance-specific options (derived from config) ----
alliance <- toupper(alliance)
alliance_name <- get_alliance_config("alliance")
meeting_name <- get_alliance_config("meeting_name")

cli_alert_info(paste("Running report:", alliance, meeting_name, sep = " "))


# Read in data ----

cli_alert_info("Reading in data...")

## Read in public CWT Data (provider level) ----
source("scripts/read-in/read_in_cwt_public_provider.R")
cli_alert_success("├─ Finished reading in public CWT (provider).")

## Read in public CWT Data (commissioner level) ----
source("scripts/read-in/read_in_cwt_public_comm.R")
cli_alert_success("├─ Finished reading in public CWT (commissioner).")

## Read in detailed CWT Data (provider level) ----
source("scripts/read-in/read_in_cwt_extract_provider.R")
cli_alert_success("├─ Finished reading in detailed CWT (provider).")

## Read in detailed CWT Data (commissioner level) ----
source("scripts/read-in/read_in_cwt_extract_comm.R")
cli_alert_success("├─ Finished reading in detailed CWT (commissioner).")

## Read in Staging Completeness Data ----
source("scripts/read-in/read_in_staging_completeness.R")
cli_alert_success("├─ Finished reading in staging completeness.")

## Read in Staging Data ----
source("scripts/read-in/read_in_staging.R")
cli_alert_success("├─ Finished reading in staging data.")

## Read in LWBC Data ----
source("scripts/read-in/read_in_lwbc.R")
cli_alert_success("└─ Finished reading in LWBC data.")

## Read in Treatment Variation Data ----
source("scripts/read-in/read_in_tv.R")
cli_alert_success("└─ Finished reading in Treatment Variation data.")

# Run tumour-level packs ----

## Define tumour types ----
tumour_keys <- get_tumour_keys()

## Uncomment for single tumour type ----
# tumour_keys <- c("head_neck")

## Create global data list ----
global_data <- create_global_data_list()

## Dataset category values (tumour-independent; computed once) ----
# Used by build_category_mapping_table() below to build each pack's
# "Dataset Category Mapping" appendix slide.
dataset_category_values <- get_dataset_category_values(global_data)

## Loop through each tumour type ----
for (tumour_key in tumour_keys) {
  tumour_name <- get_tumour_config(tumour_key)$name

  cli_alert_info(paste("Running", tumour_name, "pack...", sep = " "))

  ### Set presentation variables ----
  ref_doc <- paste0("../assets/pptx-templates/", paste(alliance, meeting_name, "template.pptx", sep = "_"))
  pres_title <- paste(tumour_name, meeting_name, "Data Pack", sep = " ")

  ### Process just this tumour site (pull out of global list)
  tumour_data <- read_tumour_data(tumour_key, global_data)
  cli_alert_success(paste0("├─ Finished processing ", tumour_name, " data."))

  ### Build dataset category mapping (for the appendix slide) and print it to
  ### the console so it's visible during this run, not just in the deck.
  tumour_data$category_mapping <- build_category_mapping_table(tumour_key, dataset_category_values)
  cli_alert_info(paste0("├─ Dataset category mapping for ", tumour_name, ":"))
  print(tumour_data$category_mapping)

  ### Render using parameterised template ----
  output_path <- rmarkdown::render(
    "scripts/Tumour_Data_Pack_Template.Rmd",
    output_file = paste(alliance, tumour_name, meeting_name, "Data Pack", sep = " "),
    output_dir = "outputs/",
    params = list(tumour_key = tumour_key),
    quiet = TRUE
  )
  cli_alert_success(paste0("└─ Rendered ", tumour_name, " slides."))

  ### Add section-header subtitles (filter strings) as a post-processing step.
  ### See R/pptx_section_subtitles.R for why this can't be done in the Rmd.
  add_section_subtitles(output_path, tumour_key)
  cli_alert_success(paste0("└─ Added section subtitles to ", tumour_name, " slides."))
}

# Clean up ----
cli_alert_info("Cleaning up...")
rm(tumour_keys)
# gc() # enable for debugging

cli_alert_success(paste(alliance, meeting_name, "report complete.", sep = " "))

# Timing
end <- proc.time()
elapsed <- end["elapsed"] - start["elapsed"]
minutes <- as.integer(elapsed %/% 60)
seconds <- as.integer(elapsed %% 60)
rm(start, end, elapsed)
cli_alert_info(paste0("Duration: ", minutes, "m ", seconds, "s"))
