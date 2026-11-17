### Living With and Beyond Cancer Data ########################################

# Includes HNAs, PCSPs,Physical Activity and EOTs.
# Data downloaded sheet-by-sheet from CancerStats.
# BEWARE - Not public!

# Set up ------------------------------------------------------------------

# Read in data
lwbc_eots_df <- read_excel("data/lwbc/Midlands_EOTS.xlsx")
lwbc_hna_status_df <- read_excel("data/lwbc/Midlands_HNA_Status.xlsx")
lwbc_pcsp_status_df <- read_excel("data/lwbc/Midlands_PCSP_Status.xlsx")
lwbc_pa_status_df <- read_excel("data/lwbc/Midlands_PA_Status.xlsx")

# Format data -------------------------------------------------------------

# Filter data to remove Total values and replace NA's with 0 for EOTS

# West Midlands

lwbc_eots_df <- lwbc_eots_df %>%
  filter(!is.na(`Cancer Alliance`)) %>% # Remove rows showing total for each time period
  mutate(
    Count = if_else(is.na(Count), 0, Count), # Replace NA values with 0
    Month = month(Month), # Take just the month of the date in this column as the rest of it is wrong
    Year = year(Year), # Take just the year of the date in this column
    Date = as.yearmon(paste(Year, Month), "%Y %m")
  ) %>% # Concatenate year and month column to get date in right format for processing
  select(-c(Year, Month, `%`)) %>% # De-select columns not needed
  rename(`Trust Name` = `TRUST NAME`)

lwbc_eots_df <- lwbc_eots_df %>%
  filter(!is.na(Date)) %>%
  mutate(Date = my(Date)) %>%
  rename(`Tumour Group` = `Cancer Group`)

# Filter data to remove Total values and replace NA's with 0 for PA

# West Midlands

lwbc_pa_status_df <- lwbc_pa_status_df %>%
  filter(!is.na(`Cancer Alliance`)) %>%
  mutate(
    # Replace missing counts with 0
    Count = if_else(is.na(`Total Count`), 0, `Total Count`),

    # Ensure Year is numeric
    Year_num = as.numeric(format(as.Date(Year), "%Y")),

    # Clean Month column: remove extra spaces and standardize capitalization
    Month_clean = str_trim(str_to_title(Month)),

    # Create Date column as Year + Month
    Date = if_else(
      is.na(Month_clean) | is.na(Year_num),
      NA,
      as.yearmon(paste(Month_clean, Year_num), "%B %Y")
    )
  ) %>%
  select(-c(Year, Month, Month_clean, Year_num, `Total Count`)) %>% # remove old columns
  rename_with(~ str_to_title(.), everything()) # capitalize column names

lwbc_pa_status_df <- lwbc_pa_status_df %>%
  filter(!is.na(Date)) %>% # Remove rows where Date is NA
  mutate(Date = my(Date)) # Convert 'mm/yyyy' or 'yyyy-mm' format to Date (first day of month)


# Clean all HNA and PCSP dataframes using helper function
lwbc_hna_status_df <- clean_lwbc_monthly_df(lwbc_hna_status_df)
lwbc_pcsp_status_df <- clean_lwbc_monthly_df(lwbc_pcsp_status_df)

# Recode trust names to canonical display names via org_lookup
lwbc_name_map <- get_source_name_map("lwbc_geography_map")

lwbc_pa_status_df <- lwbc_pa_status_df %>%
  mutate(`Trust Name` = recode(`Trust Name`, !!!lwbc_name_map))

lwbc_eots_df <- lwbc_eots_df %>%
  mutate(`Trust Name` = recode(`Trust Name`, !!!lwbc_name_map))

lwbc_hna_status_df <- lwbc_hna_status_df %>%
  mutate(`Trust Name` = recode(`Trust Name`, !!!lwbc_name_map))

lwbc_pcsp_status_df <- lwbc_pcsp_status_df %>%
  mutate(`Trust Name` = recode(`Trust Name`, !!!lwbc_name_map))

rm(lwbc_name_map)


# Create quarterly versions for selected dataframes
lwbc_hna_status_q_df <- lwbc_hna_status_df %>%
  mutate(Date = date_to_fiscal_q(Date))

lwbc_pcsp_status_q_df <- lwbc_pcsp_status_df %>%
  mutate(Date = date_to_fiscal_q(Date))

lwbc_pa_status_q_df <- lwbc_pa_status_df %>%
  mutate(Date = date_to_fiscal_q(Date))

lwbc_eots_q_df <- lwbc_eots_df %>%
  mutate(Date = date_to_fiscal_q(Date))

# Remove last 3 months from data
lwbc_hna_status_df <- lwbc_hna_status_df %>%
  filter(Date < (max(lwbc_hna_status_df$Date) %m-% months(2)))

lwbc_pcsp_status_df <- lwbc_pcsp_status_df %>%
  filter(Date < (max(lwbc_pcsp_status_df$Date) %m-% months(2)))

lwbc_pa_status_df <- lwbc_pa_status_df %>%
  filter(Date < (max(lwbc_pa_status_df$Date) %m-% months(2)))

lwbc_eots_df <- lwbc_eots_df %>%
  filter(Date < (max(lwbc_eots_df$Date) %m-% months(2)))

# Identify latest quarter with data for the summary table, and the months of it
# that have survived the trim
# The four files normally end in the same month. If one runs ahead, it alone sets
# the quarter, and files with no data for that quarter show blank in the table.
lwbc_latest_month <- max(
  lwbc_hna_status_df$Date, lwbc_pcsp_status_df$Date,
  lwbc_pa_status_df$Date, lwbc_eots_df$Date
)
lwbc_latest_q <- date_to_fiscal_q(lwbc_latest_month)
lwbc_latest_q_months <- fiscal_q_months_to_date(lwbc_latest_month)

# Tumour Pack Data -------------------------------------------------------------

# Calculate totals per tumour site
# This will allow us to calculate proportions

# for HNA
tumour_pack_lwbc_hna_status_df <- lwbc_hna_status_df %>%
  group_by(Date, `Cancer Alliance`, `Trust Name`, `Tumour Group`) %>%
  mutate(Total = sum(`Total Count`)) %>%
  rename(Count = `Total Count`) %>%
  ungroup()

tumour_pack_lwbc_q_hna_status_df <- tumour_pack_lwbc_hna_status_df %>%
  select(-Total) %>%
  filter(Group != "04 - Not Offered" &
    Group != "Not Known") %>%
  mutate(Date = date_to_fiscal_q(Date)) %>%
  group_by(Date, `Cancer Alliance`, `Trust Name`, `Tumour Group`, Group) %>%
  summarise_at(.vars = vars(Count), .funs = sum) %>%
  mutate(`Total Offered` = sum(Count))

tumour_pack_trust_lwbc_hna_status_df <- tumour_pack_lwbc_hna_status_df %>%
  filter(Group != "04 - Not Offered" &
    Group != "Not Known") %>%
  select(-Total) %>%
  group_by(Date, `Cancer Alliance`, `Tumour Group`, `Trust Name`, Group) %>%
  summarise_at(.vars = vars(Count), .funs = sum)

tumour_pack_trust_lwbc_hna_status_df <- tumour_pack_trust_lwbc_hna_status_df %>%
  group_by(Date, `Cancer Alliance`, `Trust Name`, `Tumour Group`) %>%
  mutate(`Total Offered` = sum(Count))

tumour_pack_trust_lwbc_hna_accepted_df <- tumour_pack_trust_lwbc_hna_status_df %>%
  filter(Group == "03 - Offered and Accepted") %>%
  mutate(`% HNA's Accepted` = if_else(`Total Offered` == 0, 0, round((Count / `Total Offered`) * 100, 2))) %>%
  select(-c(Group, Count, `Total Offered`))

tumour_pack_lwbc_hna_status_df <- tumour_pack_lwbc_hna_status_df %>%
  filter(Group != "04 - Not Offered" &
    Group != "Not Known") %>%
  select(-Total) %>%
  group_by(Date, `Cancer Alliance`, `Tumour Group`, Group) %>%
  summarise_at(.vars = vars(Count), .funs = sum)

tumour_pack_lwbc_hna_status_df <- tumour_pack_lwbc_hna_status_df %>%
  group_by(Date, `Cancer Alliance`, `Tumour Group`) %>%
  mutate(`Total Offered` = sum(Count))

tumour_pack_lwbc_hna_accepted_df <- tumour_pack_lwbc_hna_status_df %>%
  filter(Group == "03 - Offered and Accepted") %>%
  mutate(`% HNA's Accepted` = if_else(`Total Offered` == 0, 0, round((Count / `Total Offered`) * 100, 2))) %>%
  select(-c(Group, Count, `Total Offered`))

# for Physical Activity

tumour_pack_lwbc_pa_status_df <- lwbc_pa_status_df %>%
  group_by(Date, `Cancer Alliance`, `Trust Name`, `Tumour Group`) %>%
  mutate(Total = sum(Count)) %>%
  ungroup()

tumour_pack_lwbc_q_pa_status_df <- tumour_pack_lwbc_pa_status_df %>%
  select(-Total) %>%
  filter(`Physical Activity Assessments` != "09 - Not Known (Not recorded)") %>%
  mutate(Date = date_to_fiscal_q(Date)) %>%
  group_by(Date, `Cancer Alliance`, `Trust Name`, `Tumour Group`, `Physical Activity Assessments`) %>%
  summarise_at(.vars = vars(Count), .funs = sum) %>%
  mutate(`Total Assessed` = sum(Count))

tumour_pack_trust_lwbc_pa_status_df <- tumour_pack_lwbc_pa_status_df %>%
  filter(`Physical Activity Assessments` != "09 - Not Known (Not recorded)") %>%
  select(-Total) %>%
  group_by(Date, `Cancer Alliance`, `Tumour Group`, `Trust Name`, `Physical Activity Assessments`) %>%
  summarise_at(.vars = vars(Count), .funs = sum)

tumour_pack_trust_lwbc_pa_status_df <- tumour_pack_trust_lwbc_pa_status_df %>%
  group_by(Date, `Cancer Alliance`, `Trust Name`, `Tumour Group`) %>%
  mutate(`Total Assessed` = sum(Count))

tumour_pack_trust_lwbc_pa_achieved_df <- tumour_pack_trust_lwbc_pa_status_df %>%
  group_by(Date, `Cancer Alliance`, `Trust Name`, `Tumour Group`) %>%
  summarise(
    `Total Assessed` = first(`Total Assessed`),
    achieved = sum(if_else(
      `Physical Activity Assessments` == "01 - Achieves guidance level of physical activity",
      Count, 0
    )),
    `% PA's Achieved` = if_else(
      `Total Assessed` == 0, 0, round((achieved / `Total Assessed`) * 100, 2)
    ),
    .groups = "drop"
  )

tumour_pack_lwbc_pa_status_df <- tumour_pack_lwbc_pa_status_df %>%
  filter(`Physical Activity Assessments` != "09 - Not Known (Not recorded)") %>%
  select(-Total) %>%
  group_by(Date, `Cancer Alliance`, `Tumour Group`, `Physical Activity Assessments`) %>%
  summarise_at(.vars = vars(Count), .funs = sum)

tumour_pack_lwbc_pa_status_df <- tumour_pack_lwbc_pa_status_df %>%
  group_by(Date, `Cancer Alliance`, `Tumour Group`) %>%
  mutate(`Total Assessed` = sum(Count))

tumour_pack_lwbc_pa_achieved_df <- tumour_pack_lwbc_pa_status_df %>%
  group_by(Date, `Cancer Alliance`, `Tumour Group`) %>%
  summarise(
    `Total Assessed` = first(`Total Assessed`),
    achieved = sum(if_else(`Physical Activity Assessments` == "01 - Achieves guidance level of physical activity",
      Count, 0
    )), `% PA's Achieved` = if_else(
      `Total Assessed` == 0, 0, round((achieved / `Total Assessed`) * 100, 2)
    ),
    .groups = "drop"
  )

# For PCSP

tumour_pack_lwbc_pcsp_status_df <- lwbc_pcsp_status_df %>%
  group_by(Date, `Cancer Alliance`, `Trust Name`, `Tumour Group`) %>%
  mutate(Total = sum(`Total Count`)) %>%
  rename(Count = `Total Count`) %>%
  ungroup()

tumour_pack_lwbc_q_pcsp_status_df <- tumour_pack_lwbc_pcsp_status_df %>%
  select(-Total) %>%
  mutate(Date = date_to_fiscal_q(Date)) %>%
  group_by(Date, `Cancer Alliance`, `Trust Name`, `Tumour Group`, Group) %>%
  summarise_at(.vars = vars(Count), .funs = sum) %>%
  mutate(`Total` = sum(Count))

tumour_pack_trust_lwbc_pcsp_status_df <- tumour_pack_lwbc_pcsp_status_df %>%
  group_by(Date, `Cancer Alliance`, `Trust Name`, `Tumour Group`) %>%
  mutate(Total = sum(Count))

tumour_pack_trust_lwbc_pcsp_completed_df <- tumour_pack_trust_lwbc_pcsp_status_df %>%
  filter(Group == "03 - Assessment completed and care plan completed") %>%
  mutate(`% PCSP's Completed` = if_else(`Total` == 0, 0, round((Count / `Total`) * 100, 2))) %>%
  select(-c(Group, Count, `Total`))

tumour_pack_lwbc_pcsp_status_df <- tumour_pack_lwbc_pcsp_status_df %>%
  select(-c(Total, `Trust Name`)) %>%
  group_by(Date, `Cancer Alliance`, `Tumour Group`, Group) %>%
  summarise_at(.vars = vars(Count), .funs = sum)

tumour_pack_lwbc_pcsp_status_df <- tumour_pack_lwbc_pcsp_status_df %>%
  group_by(Date, `Cancer Alliance`, `Tumour Group`) %>%
  mutate(Total = sum(Count))

tumour_pack_lwbc_pcsp_completed_df <- tumour_pack_lwbc_pcsp_status_df %>%
  filter(Group == "03 - Assessment completed and care plan completed") %>%
  mutate(`% PCSP's Completed` = if_else(`Total` == 0, 0, round((Count / `Total`) * 100, 2))) %>%
  select(-c(Group, Count, `Total`))

# For EOTS

tumour_pack_eots_df <- lwbc_eots_df %>%
  filter(Date >= min(tumour_pack_lwbc_hna_accepted_df$Date)) %>%
  select(-c(`Trust Name`)) %>%
  group_by(Date, `Cancer Alliance`, `Tumour Group`) %>%
  summarise_at(.vars = vars(Count), .funs = sum) %>%
  rename("No. EOTS's Completed" = Count)

tumour_pack_trust_eots_df <- lwbc_eots_df %>%
  filter(Date >= min(tumour_pack_lwbc_hna_accepted_df$Date)) %>%
  group_by(Date, `Cancer Alliance`, `Tumour Group`, `Trust Name`) %>%
  summarise_at(.vars = vars(Count), .funs = sum) %>%
  rename("No. EOTS's Completed" = Count)


tumour_pack_trust_lwbc_plot_df <- full_join(tumour_pack_trust_lwbc_hna_accepted_df, tumour_pack_trust_lwbc_pcsp_completed_df, by = c("Date", "Cancer Alliance", "Trust Name", "Tumour Group"))
tumour_pack_trust_lwbc_plot_df <- full_join(tumour_pack_trust_lwbc_plot_df, tumour_pack_trust_eots_df, by = c("Date", "Cancer Alliance", "Trust Name", "Tumour Group"))
tumour_pack_trust_lwbc_pa_plot_df <- tumour_pack_trust_lwbc_pa_achieved_df

tumour_pack_trust_lwbc_plot_df <- tumour_pack_trust_lwbc_plot_df %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

tumour_pack_trust_lwbc_pa_plot_df <- tumour_pack_trust_lwbc_pa_plot_df %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

tumour_pack_trust_lwbc_plot_df <- tumour_pack_trust_lwbc_plot_df %>%
  filter(Date >= "2024-11-01")
tumour_pack_trust_lwbc_pa_plot_df <- tumour_pack_trust_lwbc_pa_plot_df %>%
  filter(Date >= "2024-11-01")

tumour_pack_lwbc_plot_df <- full_join(tumour_pack_lwbc_hna_accepted_df, tumour_pack_lwbc_pcsp_completed_df, by = c("Date", "Cancer Alliance", "Tumour Group"))
tumour_pack_lwbc_plot_df <- full_join(tumour_pack_lwbc_plot_df, tumour_pack_eots_df, by = c("Date", "Cancer Alliance", "Tumour Group"))
tumour_pack_lwbc_pa_plot_df <- tumour_pack_lwbc_pa_achieved_df

tumour_pack_lwbc_plot_df <- tumour_pack_lwbc_plot_df %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

tumour_pack_lwbc_pa_plot_df <- tumour_pack_lwbc_pa_plot_df %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

tumour_pack_lwbc_plot_df <- tumour_pack_lwbc_plot_df %>%
  filter(Date >= "2024-11-01")

tumour_pack_lwbc_pa_plot_df <- tumour_pack_lwbc_pa_plot_df %>%
  filter(Date >= "2024-11-01")
#################################################################################################################################################################

# Table Data

########################################## for trust###################################################################

# Create a dataframe showing total HNAs per Trust per Tumour per Date to join to accepted dataframe
# This is to ensure the HNA Total column is correct for Trusts who submitted no data for Group == "03 - Offered and Accepted"

tumour_pack_hna_total_df <- tumour_pack_lwbc_q_hna_status_df %>%
  select(-c(Group, Count)) %>%
  distinct(.keep_all = TRUE)

# Filter by only those that were accepted/completed

tumour_pack_hna_accepted_df <- tumour_pack_lwbc_q_hna_status_df %>%
  filter(Group == "03 - Offered and Accepted") %>%
  mutate("% HNA's Accepted" = if_else(`Total Offered` == 0, 0, (Count / `Total Offered`) * 100)) %>%
  select(-c(Count, Group)) %>%
  rename("Total HNA's Offered" = `Total Offered`)

# Join the two dataframes together, remove old Total column that excludes Trust who had no HNAs accepted
tumour_pack_hna_accepted_total_df <- full_join(tumour_pack_hna_accepted_df, tumour_pack_hna_total_df, by = c("Date", "Cancer Alliance", "Trust Name", "Tumour Group")) %>%
  select(-`Total HNA's Offered`) %>%
  rename("Total HNA's Offered" = `Total Offered`) %>%
  relocate("Total HNA's Offered", .before = `% HNA's Accepted`)

# Repeat for PCSPs

tumour_pack_pcsp_total_df <- tumour_pack_lwbc_q_pcsp_status_df %>%
  select(-c(Group, Count)) %>%
  distinct(.keep_all = TRUE)

# Filter by only those that were completed
tumour_pack_pcsp_completed_df <- tumour_pack_lwbc_q_pcsp_status_df %>%
  filter(Group == "03 - Assessment completed and care plan completed") %>%
  mutate("% PCSP's Completed" = if_else(Total == 0, 0, (Count / Total) * 100)) %>%
  select(-c(Count, Group)) %>%
  rename("PCSP Total" = Total)

# Join the two dataframes together, remove old Total column that excludes Trust who had no PCSP completed
tumour_pack_pcsp_completed_total_df <- full_join(tumour_pack_pcsp_completed_df, tumour_pack_pcsp_total_df, by = c("Date", "Cancer Alliance", "Trust Name", "Tumour Group")) %>%
  select(-`PCSP Total`) %>%
  rename("PCSP Total" = Total) %>%
  relocate("PCSP Total", .before = `% PCSP's Completed`)

# Repeat for PA
tumour_pack_pa_total_df <- tumour_pack_lwbc_q_pa_status_df %>%
  select(-c(`Physical Activity Assessments`, Count)) %>%
  distinct(.keep_all = TRUE)

# Filter by only those that were accepted

tumour_pack_pa_achieved_df <- tumour_pack_lwbc_q_pa_status_df %>%
  filter(`Physical Activity Assessments` == "01 - Achieves guidance level of physical activity") %>%
  mutate("% PA's Achieved" = if_else(`Total Assessed` == 0, 0, (Count / `Total Assessed`) * 100)) %>%
  select(-c(Count, `Physical Activity Assessments`)) %>%
  rename("Total PA's Assessed" = `Total Assessed`)

# Join the two dataframes together, remove old Total column that excludes Trust who had no PAs ahieved
tumour_pack_pa_achieved_total_df <- full_join(tumour_pack_pa_achieved_df, tumour_pack_pa_total_df, by = c("Date", "Cancer Alliance", "Trust Name", "Tumour Group")) %>%
  select(-`Total PA's Assessed`) %>%
  rename("Total PA's Assessed" = `Total Assessed`) %>%
  relocate("Total PA's Assessed", .before = `% PA's Achieved`)

# Create an EOTS dataframe for table
tumour_pack_eots_table_df <- lwbc_eots_df %>%
  mutate(Date = date_to_fiscal_q(Date)) %>%
  group_by(Date, `Cancer Alliance`, `Trust Name`, `Tumour Group`) %>%
  summarise_at(.vars = vars(Count), .funs = sum) %>%
  rename(`No. End of Treatment Summaries` = Count)

# Join data frames together for table production
tumour_pack_table_data <- full_join(tumour_pack_hna_accepted_total_df, tumour_pack_pcsp_completed_total_df, by = c("Date", "Cancer Alliance", "Trust Name", "Tumour Group")) %>%
  full_join(tumour_pack_pa_achieved_total_df, by = c("Date", "Cancer Alliance", "Trust Name", "Tumour Group"))

tumour_pack_lwbc_table_df <- full_join(tumour_pack_table_data, tumour_pack_eots_table_df, by = c("Date", "Cancer Alliance", "Trust Name", "Tumour Group"))

########################################## for alliance#######################################################
# Filter by only those that were accepted/completed

tumour_pack_alliance_hna_total_df <- tumour_pack_lwbc_q_hna_status_df %>%
  select(-c(Group, Count)) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(`Trust Name` = `Cancer Alliance`) %>%
  group_by(Date, `Trust Name`, `Cancer Alliance`, `Tumour Group`) %>%
  summarise_at(.vars = vars(`Total Offered`), .funs = sum)

tumour_pack_alliance_hna_accepted_df <- tumour_pack_lwbc_q_hna_status_df %>%
  ungroup() %>%
  filter(Group == "03 - Offered and Accepted") %>%
  select(-c(Group, `Total Offered`)) %>%
  mutate(`Trust Name` = `Cancer Alliance`)

tumour_pack_alliance_hna_accepted_df <- tumour_pack_alliance_hna_accepted_df %>%
  group_by(Date, `Cancer Alliance`, `Trust Name`, `Tumour Group`) %>%
  summarise_at(.vars = vars(Count), .funs = sum)

tumour_pack_alliance_hna_accepted_df <- full_join(tumour_pack_alliance_hna_accepted_df, tumour_pack_alliance_hna_total_df, by = c("Date", "Cancer Alliance", "Trust Name", "Tumour Group")) %>%
  rename("Total HNA's Offered" = `Total Offered`) %>%
  mutate("% HNA's Accepted" = if_else(`Total HNA's Offered` == 0, 0, (Count / `Total HNA's Offered`) * 100)) %>%
  select(-Count)

# Repeat for PCSPs

tumour_pack_alliance_pcsp_total_df <- tumour_pack_lwbc_q_pcsp_status_df %>%
  select(-c(Group, Count)) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(`Trust Name` = `Cancer Alliance`) %>%
  group_by(Date, `Trust Name`, `Cancer Alliance`, `Tumour Group`) %>%
  summarise_at(.vars = vars(Total), .funs = sum)

tumour_pack_alliance_pcsp_completed_df <- tumour_pack_lwbc_q_pcsp_status_df %>%
  ungroup() %>%
  filter(Group == "03 - Assessment completed and care plan completed") %>%
  select(-c(Group, Total)) %>%
  mutate(`Trust Name` = `Cancer Alliance`)

tumour_pack_alliance_pcsp_completed_df <- tumour_pack_alliance_pcsp_completed_df %>%
  group_by(Date, `Cancer Alliance`, `Trust Name`, `Tumour Group`) %>%
  summarise_at(.vars = vars(Count), .funs = sum)

tumour_pack_alliance_pcsp_completed_df <- full_join(tumour_pack_alliance_pcsp_completed_df, tumour_pack_alliance_pcsp_total_df, by = c("Date", "Cancer Alliance", "Trust Name", "Tumour Group")) %>%
  rename("PCSP Total" = Total) %>%
  mutate("% PCSP's Completed" = if_else(`PCSP Total` == 0, 0, (Count / `PCSP Total`) * 100)) %>%
  select(-Count)

# Repeat for PA's

tumour_pack_alliance_pa_total_df <- tumour_pack_lwbc_q_pa_status_df %>%
  select(-c(`Physical Activity Assessments`, Count)) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(`Trust Name` = `Cancer Alliance`) %>%
  group_by(Date, `Trust Name`, `Cancer Alliance`, `Tumour Group`) %>%
  summarise_at(.vars = vars(`Total Assessed`), .funs = sum)

tumour_pack_alliance_pa_achieved_df <- tumour_pack_lwbc_q_pa_status_df %>%
  ungroup() %>%
  filter(`Physical Activity Assessments` == "01 - Achieves guidance level of physical activity") %>%
  select(-c(`Physical Activity Assessments`, `Total Assessed`)) %>%
  mutate(`Trust Name` = `Cancer Alliance`)

tumour_pack_alliance_pa_achieved_df <- tumour_pack_alliance_pa_achieved_df %>%
  group_by(Date, `Cancer Alliance`, `Trust Name`, `Tumour Group`) %>%
  summarise_at(.vars = vars(Count), .funs = sum)

tumour_pack_alliance_pa_achieved_df <- full_join(tumour_pack_alliance_pa_achieved_df, tumour_pack_alliance_pa_total_df, by = c("Date", "Cancer Alliance", "Trust Name", "Tumour Group")) %>%
  rename("Total PA's Assessed" = `Total Assessed`) %>%
  mutate("% PA's Achieved" = if_else(`Total PA's Assessed` == 0, 0, (Count / `Total PA's Assessed`) * 100)) %>%
  select(-Count)

# Create an EOTS dataframe for table
tumour_pack_alliance_eots_table_df <- lwbc_eots_df %>%
  select(-`Trust Name`) %>%
  mutate(Date = date_to_fiscal_q(Date)) %>%
  group_by(Date, `Cancer Alliance`, `Tumour Group`) %>%
  summarise_at(.vars = vars(Count), .funs = sum) %>%
  rename(`No. End of Treatment Summaries` = Count) %>%
  mutate(`Trust Name` = `Cancer Alliance`)

# Join data frames together for table production
tumour_pack_alliance_table_data <- full_join(tumour_pack_alliance_hna_accepted_df, tumour_pack_alliance_pcsp_completed_df, by = c("Date", "Cancer Alliance", "Trust Name", "Tumour Group")) %>%
  full_join(tumour_pack_alliance_pa_achieved_df, by = c("Date", "Cancer Alliance", "Trust Name", "Tumour Group"))

tumour_pack_alliance_lwbc_table_df <- full_join(tumour_pack_alliance_table_data, tumour_pack_alliance_eots_table_df, by = c("Date", "Cancer Alliance", "Trust Name", "Tumour Group"))

tumour_pack_alliance_trust_lwbc_table_df <- bind_rows(tumour_pack_lwbc_table_df, tumour_pack_alliance_lwbc_table_df)


rm(tumour_pack_eots_table_df, tumour_pack_hna_accepted_df, tumour_pack_hna_accepted_total_df, tumour_pack_hna_total_df, tumour_pack_pcsp_completed_df, tumour_pack_pcsp_completed_total_df, tumour_pack_pcsp_total_df, tumour_pack_pa_achieved_df, tumour_pack_pa_achieved_total_df, tumour_pack_pa_total_df, tumour_pack_table_data)
