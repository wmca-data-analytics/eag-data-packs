### Cancer Stage Data ###########################################
################################################################################
################################################################################

# ============================================================================
# Configuration (using centralised alliance config)
# ============================================================================

# .rds files are from https://nhs.sharepoint.com/sites/msteams_275dcb/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=gK7S2I
alliance_initial <- get_alliance_config("initial")
rds_pattern <- paste0("^RCRD_Inc_Trt_", alliance_initial, "Midlands_CAS.*\\.rds$")
files <- list.files(
  path = "data/staging/",
  pattern = rds_pattern,
  full.names = TRUE
)

if (length(files) == 0) {
  cli_alert_warning("No staging files matched: ", rds_pattern, " in data/staging")
} else if (length(files) > 1) {
  cli_alert_danger("Multiple staging files matched; using the first one.\n", paste(files, collapse = "\n"))
}

stage_raw_df <- readRDS(files[1])

# Change date format
stage_raw_df$Date <- ymd(paste0(stage_raw_df$Date, "-01"))

# Remove the most recent month
stage_raw_df <- stage_raw_df %>%
  filter(Date != max(Date))

stage_df <- stage_raw_df %>%
  select(`Geography type`, Geography, Date, `Cancer group`, Metric, Breakdown, Demographic, `Statistic (12m)`) %>%
  filter(`Geography type` %in% c("Cancer alliance", "Provider"))

# Remove whole dataframe to preserve memory
rm(stage_raw_df)

# Apply geography to org name mapping from centralised config
stage_geography_map <- get_source_name_map("stage_geography_map")
stage_df <- stage_df %>%
  mutate(ORG.NAME = recode(`Geography`, !!!stage_geography_map))
stage_df <- stage_df %>%
  mutate(`Statistic (12m)` = ifelse(Demographic == "DCO" & is.na(`Statistic (12m)`), 0, `Statistic (12m)`))

#print(unique(stage_df$`Cancer group`))

# Use Cancer staging observed
# Use https://digital.nhs.uk/ndrs/data/data-sets/rcrd/using-rcrd-to-measure-stage
# Measure the proportion of early-stage cancers as the number of persons diagnosed at stage 1 or 2 over all staged cancers.
# The early-stage proportion measured within the Rapid Cancer Registration Data has a stable relationship to that in the National Cancer Registration Data, although there is an offset between them in the early-stage percentage measured.
# Data should be aggregated over a long enough period to smooth out random noise, a 12-month period is recommended.
# Comparisons within data sets (either within the NCRD or within the RCRD) are preferred to direct comparisons between the two.

# Most recent month needs to be discounted due to being too rapid
