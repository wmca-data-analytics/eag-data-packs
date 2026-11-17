### Staging Completeness Data ###########################################
################################################################################
################################################################################

# ============================================================================
# Configuration (using centralised alliance config)
# ============================================================================

# Read in downloaded files from https://nhsd-ndrs.shinyapps.io/staging_dashboard/
#stage_completeness_raw_df <- read.csv("data/staging-completeness/All stage completeness data from Q1 2021 to Q4 2025 (10 January 2025).csv")

# Read in downloaded files from https://nhsd-ndrs.shinyapps.io/staging_dashboard/alliance_initial <- get_alliance_initial()
files <- list.files(
  path = "data/staging-completeness/",
  pattern = "All stage*",
  full.names = TRUE
)

if (length(files) == 0) {
  cli_alert_warning("No staging completeness files matched: ", csv_pattern, " in data/staging=completeness")
} else if (length(files) > 1) {
  cli_alert_danger("Multiple staging completeness files matched; using the first one.\n", paste(files, collapse = "\n"))
}

stage_completeness_raw_df <- read.csv(files[1])


# Filter for organisation codes
selected_cols_df <- stage_completeness_raw_df %>%
  select(DIAGNOSIS_QUARTER, TRUST_CD, CANCER_GROUP_NM, MDT_STAGE, MDT_DISCUSSED)

# Filter using trust codes from centralised config
trust_codes <- get_alliance_config("trust_codes")
stage_completeness_df <- selected_cols_df %>%
  filter(TRUST_CD %in% trust_codes)

# Remove whole dataframe to preserve memory
rm(stage_completeness_raw_df, selected_cols_df)

# Apply org recode map from centralised config
org_recode_map <- get_org_recode_map()
stage_completeness_df <- stage_completeness_df %>%
  mutate(ORG.NAME = recode(TRUST_CD, !!!org_recode_map))

stage_completeness_df <- stage_completeness_df %>%
  mutate(DIAGNOSIS_QUARTER = yq(DIAGNOSIS_QUARTER))
