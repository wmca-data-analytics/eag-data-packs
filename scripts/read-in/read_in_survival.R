survival_raw_df <- read_excel("cancer_survival_2018_2022.xlsx", sheet = "Table_4", skip = 2)
notes_lookup <- read_excel("cancer_survival_2018_2022.xlsx", sheet = "Notes", skip = 1)

notes_lookup <- notes_lookup |>
  select(-`Fields affected`)

survival_df <- survival_raw_df |>
  select(-c(`Overall survival (%)`, `Overall survival, lower 95% CI`, `Overall survival, upper 95% CI`)) |>
  filter(
    str_detect(`NDRS Detailed Group`, "^All"),
    `Geography name` %in% c("West Midlands", "England", "NHS Birmingham and Solihull Integrated Care Board", "NHS Coventry and Warwickshire Integrated Care Board", "NHS Shropshire, Telford and Wrekin Integrated Care Board", "NHS Staffordshire and Stoke-on-Trent Integrated Care Board", "NHS Herefordshire and Worcestershire Integrated Care Board", "NHS Black Country Integrated Care Board"),
    `Geography type` != "Government office region",
    `Standardisation type` != "Non-standardised (All ages)",
    `Net survival (%)` != "[u]"
  )

survival_england_df <- subset(survival_graph_df, `Geography name` == "England")



# Graph examples ------------------------------------------------------------------------------

# Graph showing gender gap in survival faceted by years since diagnosis

survival_years_df <- survival_df |>
  filter(
    `NDRS Detailed Group` == tumour_site,
    Gender != "Persons",
    `Net survival (%)` != "[u]"
  ) |>
  mutate(`Net survival (%)` = as.numeric(`Net survival (%)`))

ggplot(survival_years_df, aes(`Net survival (%)`, `Geography name`)) +
  geom_point(aes(color = factor(`Gender`), shape = factor(`Gender`)), size = 3) +
  facet_wrap(~`Years since diagnosis`) +
  theme_minimal()

# Graph showing gap between 1 and 5 year survival
survival_year_gap_df <- survival_df |>
  filter(
    `NDRS Detailed Group` == tumour_site,
    Gender == "Persons",
    `Years since diagnosis` %in% c(1, 5),
    `Net survival (%)` != "[u]"
  ) |>
  mutate(`Net survival (%)` = as.numeric(`Net survival (%)`))

ggplot(survival_year_gap_df, aes(`Net survival (%)`, `Geography name`)) +
  geom_point(aes(color = factor(`Years since diagnosis`), shape = factor(`Years since diagnosis`)), size = 3) +
  theme_minimal()

# Graph showing survival by geography with England benchmark line

survival_graph_df <- survival_df |>
  filter(
    `NDRS Detailed Group` == tumour_site,
    Gender == "Persons",
    `Net survival (%)` != "[u]"
  ) |>
  mutate(`Net survival (%)` = as.numeric(`Net survival (%)`))


ggplot(survival_graph_df, aes(`Years since diagnosis`, `Net survival (%)`)) +
  geom_point() +
  geom_line() +
  geom_point(
    data = survival_england_df[, c("Years since diagnosis", "Net survival (%)")],
    aes(`Years since diagnosis`, `Net survival (%)`),
    colour = "red"
  ) +
  geom_line(
    data = survival_england_df[, c("Years since diagnosis", "Net survival (%)")],
    aes(`Years since diagnosis`, `Net survival (%)`),
    colour = "red"
  ) +
  facet_wrap(~`Geography name`)

# Graph showing survival by geography with gender break down

survival_gender_df <- survival_df |>
  filter(
    `NDRS Detailed Group` == tumour_site,
    Gender != "Persons",
    `Net survival (%)` != "[u]"
  ) |>
  mutate(`Net survival (%)` = as.numeric(`Net survival (%)`))

ggplot(survival_gender_df, aes(`Years since diagnosis`, `Net survival (%)`, group = Gender)) +
  geom_point(aes(color = Gender)) +
  geom_line(aes(color = Gender), linewidth = 1) +
  facet_wrap(~`Geography name`)
