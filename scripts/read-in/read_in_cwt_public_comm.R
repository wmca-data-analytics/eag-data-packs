### CWT Data - Commissioner ----

# Note this is a different download/dataset from the one used for provider-level
# figures. This one is from the "Monthly Data and Summaries" section:
# https://www.england.nhs.uk/statistics/statistical-work-areas/cancer-waiting-times/monthly-data-and-summaries/2025-26-monthly-cancer-waiting-times-statistics/

# Find newest file ----
df <- file.info(list.files("data/cwt/commissioner", full.names = T))
filename <- rownames(df)[which.max(df$mtime)]
rm(df)

# Suppress messages caused by auto-assignment of column names
temp_df <- suppressMessages(read_excel(filename, sheet = "System Level Performance", col_names = FALSE))
temp_df <- temp_df[-(1:13), ] # Delete first 8 rows
#temp_df <- temp_df[, -c(1, 10)] #  Delete cols 1 & 10
colnames(temp_df) <- as.character(unlist(temp_df[1, ])) # Use row 1 for column names
temp_df <- temp_df[-1, ] # Delete row 1 (still column names)

# Filter by alliance ----
emca_systems <- c("QJ2", "QJM", "QK1", "QPM", "QT1")
wmca_systems <- c("QHL", "QUA", "QWU", "QGH", "QOC", "QNC")
systems <- c(emca_systems, wmca_systems)
temp_df <- filter(temp_df, temp_df$`Org Code` %in% systems)
temp_df <- temp_df %>%
  mutate(Alliance = case_when(
    `Org Code` %in% emca_systems ~ "EMCA",
    `Org Code` %in% wmca_systems ~ "WMCA"
  )) %>%
  relocate(`Alliance`)

# Filter by everything else ----
# Slide don't include 31-day, so that's not required
temp_df <- filter(temp_df, `Alliance` == toupper(alliance))
temp_df <- filter(temp_df, ! temp_df$Standard %in% "31D")
temp_df <- filter(temp_df, ! temp_df$`Cancer Type` %in% "ALL CANCERS")
temp_df <- filter(temp_df, temp_df$`Referral Route/Stage` %in% "ALL ROUTES")

# Keep only req'd cols ----
temp_df <- select(temp_df,
                  "Alliance",
                  "Org Code",
                  "System",
                  "Standard",
                  "Cancer Type",
                  "Within",
                  "After",
                  "Total")

# Ensure correct data types ----
temp_df <- mutate(temp_df, Within = as.numeric(`Within`))
temp_df <- mutate(temp_df, After = as.numeric(`After`))
temp_df <- mutate(temp_df, Total = as.numeric(`Total`))

# Add date ----
fname <- basename(filename)
parts <- strsplit(fname, "-")[[1]]
this_month <- paste(parts[1], parts[2])
temp_df <- mutate(temp_df, Period = this_month)
temp_df <- relocate(temp_df, Period)
rm (fname, parts, this_month)

# Split into individual standards ----
cwt_fds_comm_df <- temp_df %>%
  filter(Standard == "FDS")
cwt_62d_comm_df <- temp_df %>%
  filter(Standard == "62D")
rm(temp_df)

# Combine ICBs and calculate Compliance ----
# Name with a key here so we can definitely access the correct output df below
dfs = list(cwt_fds_comm_df = cwt_fds_comm_df,
           cwt_62d_comm_df = cwt_62d_comm_df
           )

output_dfs <- lapply(dfs, function(df) {
  df %>%
    group_by(`Cancer Type`) %>%
    summarise(
      Period = max(Period, na.rm = TRUE),
      Standard = max(Standard, na.rm = TRUE),
      Within = sum(Within, na.rm = TRUE),
      After  = sum(After,  na.rm = TRUE),
      Total  = sum(Total, na.rm = TRUE),
    ) %>%
    mutate(Compliance = mapply(calc_cwt_compliance, Within, Total),
           ) %>%
    relocate(Period)
})

cwt_fds_comm_df <- output_dfs[["cwt_fds_comm_df"]]
cwt_62d_comm_df <- output_dfs[["cwt_62d_comm_df"]]

# Calculate CWT measures ----
# Can't do this earlier otherwise we get rounding errors when we sum,
# particularly in 'Standard as number of patients'.
dfs = list(cwt_fds_comm_df = cwt_fds_comm_df,
           cwt_62d_comm_df = cwt_62d_comm_df
)

output_dfs <- lapply(dfs, function(df) {
  df %>%
    mutate(`Std as pts` = mapply(get_cwt_targets_as_patients, Standard, Total),
      `Diff from std` = Within - `Std as pts`
      ) %>%
    select(-c("Standard"))
})

cwt_fds_comm_df <- output_dfs[["cwt_fds_comm_df"]]
cwt_62d_comm_df <- output_dfs[["cwt_62d_comm_df"]]

# Cleanup ----
rm(filename,
   emca_systems,
   wmca_systems,
   systems,
   dfs,
   output_dfs
   )
