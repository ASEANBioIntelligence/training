# WHO Influenza Surveillance Training: FluNet and FluID
# ======================================================
#
# This beginner-friendly script demonstrates how to:
# 1. Download recent weekly influenza surveillance data from WHO.
# 2. Clean and inspect FluNet and FluID data.
# 3. Analyse laboratory detections and influenza positivity from FluNet.
# 4. Analyse ILI and SARI indicators from FluID.
# 5. Combine both systems carefully for an integrated view.
# 6. Create charts and export results.
#
# FluNet and FluID measure different parts of influenza surveillance:
# - FluNet contains virological or laboratory surveillance data.
# - FluID contains epidemiological or syndromic surveillance data.
#
# Run this script one section at a time. Read the comments before running
# each section and inspect the new objects in RStudio's Environment pane.


# Step 1: Install and load packages --------------------------------------

# pacman installs missing packages and then loads them.
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  janitor
)

# This base R helper displays 10000 as 10,000 on chart axes.
comma_labels <- function(numbers) {
  format(numbers, big.mark = ",", scientific = FALSE, trim = TRUE)
}

# This helper adds reported values but keeps the result missing when every
# value is missing. Missing surveillance data should not become zero.
sum_reported <- function(values) {
  if (all(is.na(values))) {
    return(NA_real_)
  }

  sum(values, na.rm = TRUE)
}


# Step 2: Choose the analysis settings ----------------------------------

# Download the current year and the three previous years.
current_year <- as.integer(format(Sys.Date(), "%Y"))
start_year <- current_year - 3

# Choose one WHO region for the training charts.
# Available codes: AFR, AMR, EMR, EUR, SEAR, and WPR.
selected_region <- "SEAR"

# FluNet separates SENTINEL, NONSENTINEL, and NOTDEFINED data.
# This lesson uses sentinel surveillance for a more consistent comparison.
selected_source <- "SENTINEL"


# Step 3: Build the WHO download links ----------------------------------

# The OData filter limits the download to recent ISO years. %20 represents
# a space inside a web address.
flunet_url <- paste0(
  "https://xmart-api-public.who.int/FLUMART/VIW_FNT?",
  "$filter=ISO_YEAR%20ge%20", start_year,
  "&$format=csv"
)

fluid_url <- paste0(
  "https://xmart-api-public.who.int/FLUMART/VIW_FID_EPI?",
  "$filter=ISO_YEAR%20ge%20", start_year,
  "&$format=csv"
)

metadata_url <- paste0(
  "https://xmart-api-public.who.int/FLUMART/",
  "VIW_FLU_METADATA?$format=csv"
)


# Step 4: Download the datasets -----------------------------------------

# FluNet can be large. The filtered link keeps only the recent years selected
# above. Download time depends on the internet connection and current API load.
flunet_raw <- read_csv(flunet_url, show_col_types = FALSE)
fluid_raw <- read_csv(fluid_url, show_col_types = FALSE)
influenza_dictionary <- read_csv(metadata_url, show_col_types = FALSE)

# Inspect the original column names and data types.
glimpse(flunet_raw)
glimpse(fluid_raw)


# Step 5: Clean column names and dates ----------------------------------

# clean_names() changes names such as ISO_WEEKSTARTDATE into
# iso_weekstartdate. as.Date() creates proper date values for plotting.
flunet <- flunet_raw %>%
  clean_names() %>%
  mutate(
    iso_weekstartdate = as.Date(iso_weekstartdate),
    mmwr_weekstartdate = as.Date(mmwr_weekstartdate)
  )

fluid <- fluid_raw %>%
  clean_names() %>%
  mutate(
    iso_weekstartdate = as.Date(iso_weekstartdate),
    mmwr_weekstartdate = as.Date(mmwr_weekstartdate),
    agegroup_code = str_to_upper(agegroup_code)
  )

influenza_dictionary <- influenza_dictionary %>%
  clean_names()

# View definitions for important training variables.
dictionary_training_fields <- influenza_dictionary %>%
  filter(
    field_name %in% c(
      "SPEC_PROCESSED_NB", "INF_A", "INF_B", "INF_ALL",
      "AGEGROUP_CODE", "ILI_CASE", "ILI_OUTPATIENTS",
      "SARI_CASE", "SARI_INPATIENTS", "SARI_DEATHS"
    )
  ) %>%
  select(dataset_name, field_name, description, comments)

print(dictionary_training_fields)


# Step 6: Check data quality ---------------------------------------------

# Count rows, countries, and reporting weeks in each dataset.
dataset_overview <- bind_rows(
  flunet %>%
    summarise(
      dataset = "FluNet",
      rows = n(),
      countries = n_distinct(country_code),
      first_week = min(iso_weekstartdate, na.rm = TRUE),
      latest_week = max(iso_weekstartdate, na.rm = TRUE)
    ),
  fluid %>%
    summarise(
      dataset = "FluID",
      rows = n(),
      countries = n_distinct(country_code),
      first_week = min(iso_weekstartdate, na.rm = TRUE),
      latest_week = max(iso_weekstartdate, na.rm = TRUE)
    )
)

print(dataset_overview)

# Check which reporting sources are available in FluNet.
flunet_sources <- flunet %>%
  count(origin_source, sort = TRUE, name = "rows")

print(flunet_sources)

# Check which age groups are available in FluID.
fluid_age_groups <- fluid %>%
  count(agegroup_code, sort = TRUE, name = "rows")

print(fluid_age_groups)


# Step 7: Prepare FluNet laboratory data --------------------------------

# A country may report separate sentinel and non-sentinel records in the same
# week. Filter to one source before adding counts to reduce double counting.
flunet_analysis <- flunet %>%
  filter(
    whoregion == selected_region,
    origin_source == selected_source
  )

if (nrow(flunet_analysis) == 0) {
  stop("No FluNet rows match selected_region and selected_source.")
}

# Add country reports into one weekly regional series.
# Use inf_all as WHO's total influenza detections. Do not add the subtype
# columns together because subtype categories can overlap with totals.
flunet_weekly <- flunet_analysis %>%
  group_by(iso_weekstartdate, iso_year, iso_week) %>%
  summarise(
    specimens_tested = sum_reported(spec_processed_nb),
    influenza_positive = sum_reported(inf_all),
    influenza_a = sum_reported(inf_a),
    influenza_b = sum_reported(inf_b),
    countries_reporting = n_distinct(
      country_code[!is.na(spec_processed_nb) | !is.na(inf_all)]
    ),
    .groups = "drop"
  ) %>%
  mutate(
    positivity_percent = if_else(
      specimens_tested > 0,
      100 * influenza_positive / specimens_tested,
      NA_real_
    )
  )

print(tail(flunet_weekly))

# Summarise laboratory reporting by country for the selected period.
flunet_country_summary <- flunet_analysis %>%
  group_by(country_code, country_area_territory) %>%
  summarise(
    reporting_weeks = n_distinct(iso_weekstartdate),
    specimens_tested = sum_reported(spec_processed_nb),
    influenza_positive = sum_reported(inf_all),
    .groups = "drop"
  ) %>%
  mutate(
    positivity_percent = if_else(
      specimens_tested > 0,
      100 * influenza_positive / specimens_tested,
      NA_real_
    )
  ) %>%
  arrange(desc(influenza_positive))

print(flunet_country_summary)


# Step 8: Visualize FluNet data ------------------------------------------

influenza_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# Chart 1: Weekly specimens tested and influenza-positive specimens.
flunet_volume_long <- flunet_weekly %>%
  select(iso_weekstartdate, specimens_tested, influenza_positive) %>%
  pivot_longer(
    cols = c(specimens_tested, influenza_positive),
    names_to = "measure",
    values_to = "count"
  ) %>%
  mutate(
    measure = recode(
      measure,
      specimens_tested = "Specimens tested",
      influenza_positive = "Influenza positive"
    )
  )

plot_flunet_volume <- flunet_volume_long %>%
  ggplot(aes(x = iso_weekstartdate, y = count, color = measure)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(vars(measure), ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = comma_labels) +
  labs(
    title = "Weekly influenza laboratory surveillance",
    subtitle = str_c(selected_region, " region, ", selected_source, " reports"),
    x = NULL,
    y = "Number reported",
    color = NULL,
    caption = "Source: WHO FluNet"
  ) +
  influenza_theme +
  theme(legend.position = "none")

print(plot_flunet_volume)

# Chart 2: Weekly influenza test positivity.
plot_flunet_positivity <- flunet_weekly %>%
  ggplot(aes(x = iso_weekstartdate, y = positivity_percent)) +
  geom_line(color = "#D95F02", linewidth = 0.9, na.rm = TRUE) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = function(numbers) str_c(round(numbers, 1), "%")
  ) +
  labs(
    title = "Influenza test positivity",
    subtitle = str_c(selected_region, " region, ", selected_source, " reports"),
    x = NULL,
    y = "Positive specimens / specimens tested",
    caption = "Source: WHO FluNet"
  ) +
  influenza_theme

print(plot_flunet_positivity)

# Chart 3: Weekly influenza A and B detections.
flunet_types_long <- flunet_weekly %>%
  select(iso_weekstartdate, influenza_a, influenza_b) %>%
  pivot_longer(
    cols = c(influenza_a, influenza_b),
    names_to = "influenza_type",
    values_to = "detections"
  ) %>%
  mutate(
    influenza_type = recode(
      influenza_type,
      influenza_a = "Influenza A",
      influenza_b = "Influenza B"
    )
  )

plot_flunet_types <- flunet_types_long %>%
  ggplot(aes(x = iso_weekstartdate, y = detections, fill = influenza_type)) +
  geom_col(na.rm = TRUE) +
  scale_y_continuous(labels = comma_labels) +
  scale_fill_manual(values = c("Influenza A" = "#1B9E77", "Influenza B" = "#7570B3")) +
  labs(
    title = "Influenza A and B detections by week",
    subtitle = str_c(selected_region, " region, ", selected_source, " reports"),
    x = NULL,
    y = "Detections",
    fill = NULL,
    caption = "Source: WHO FluNet"
  ) +
  influenza_theme

print(plot_flunet_types)


# Step 9: Prepare FluID ILI and SARI data --------------------------------

# FluID can contain both all-age totals and age-specific rows. Summing both
# would double count cases. This beginner lesson keeps only all-age records.
# Some countries that report only age-specific data will therefore be absent.
fluid_all_ages <- fluid %>%
  filter(
    whoregion == selected_region,
    agegroup_code == "ALL"
  )

if (nrow(fluid_all_ages) == 0) {
  stop("No all-age FluID rows match selected_region.")
}

# Calculate weighted regional indicators by adding numerators and
# denominators first. Do not average country percentages.
fluid_weekly <- fluid_all_ages %>%
  group_by(iso_weekstartdate, iso_year, iso_week) %>%
  summarise(
    ili_cases = sum_reported(ili_case),
    outpatient_visits = sum_reported(ili_outpatients),
    sari_cases = sum_reported(sari_case),
    inpatient_admissions = sum_reported(sari_inpatients),
    sari_deaths = sum_reported(sari_deaths),
    countries_reporting_ili = n_distinct(
      country_code[!is.na(ili_case) & !is.na(ili_outpatients)]
    ),
    countries_reporting_sari = n_distinct(
      country_code[!is.na(sari_case) & !is.na(sari_inpatients)]
    ),
    .groups = "drop"
  ) %>%
  mutate(
    ili_per_1000_outpatients = if_else(
      outpatient_visits > 0,
      1000 * ili_cases / outpatient_visits,
      NA_real_
    ),
    sari_per_100_inpatients = if_else(
      inpatient_admissions > 0,
      100 * sari_cases / inpatient_admissions,
      NA_real_
    )
  )

print(tail(fluid_weekly))

# Show reporting completeness by country. High case counts can reflect more
# reporting weeks or larger surveillance coverage, not only more disease.
fluid_country_summary <- fluid_all_ages %>%
  group_by(country_code, country_area_territory) %>%
  summarise(
    reporting_weeks = n_distinct(iso_weekstartdate),
    ili_cases = sum_reported(ili_case),
    outpatient_visits = sum_reported(ili_outpatients),
    sari_cases = sum_reported(sari_case),
    inpatient_admissions = sum_reported(sari_inpatients),
    sari_deaths = sum_reported(sari_deaths),
    .groups = "drop"
  ) %>%
  arrange(desc(reporting_weeks))

print(fluid_country_summary)


# Step 10: Visualize FluID data ------------------------------------------

# Chart 4: ILI cases per 1,000 outpatient visits.
plot_fluid_ili <- fluid_weekly %>%
  ggplot(aes(x = iso_weekstartdate, y = ili_per_1000_outpatients)) +
  geom_line(color = "#E7298A", linewidth = 0.9, na.rm = TRUE) +
  labs(
    title = "Influenza-like illness among outpatients",
    subtitle = str_c(selected_region, " region; all-age reports"),
    x = NULL,
    y = "ILI cases per 1,000 outpatient visits",
    caption = "Source: WHO FluID"
  ) +
  influenza_theme

print(plot_fluid_ili)

# Chart 5: SARI cases per 100 inpatient admissions.
plot_fluid_sari <- fluid_weekly %>%
  ggplot(aes(x = iso_weekstartdate, y = sari_per_100_inpatients)) +
  geom_line(color = "#66A61E", linewidth = 0.9, na.rm = TRUE) +
  labs(
    title = "Severe acute respiratory infection among inpatients",
    subtitle = str_c(selected_region, " region; all-age reports"),
    x = NULL,
    y = "SARI cases per 100 inpatient admissions",
    caption = "Source: WHO FluID"
  ) +
  influenza_theme

print(plot_fluid_sari)


# Step 11: Create an integrated surveillance view -----------------------

# Join the two weekly summaries by ISO week start date. A full_join() keeps
# weeks reported by only one system and makes data gaps visible.
integrated_weekly <- flunet_weekly %>%
  select(
    iso_weekstartdate,
    influenza_positivity_percent = positivity_percent,
    flunet_countries = countries_reporting
  ) %>%
  full_join(
    fluid_weekly %>%
      select(
        iso_weekstartdate,
        ili_per_1000_outpatients,
        sari_per_100_inpatients,
        fluid_countries_ili = countries_reporting_ili,
        fluid_countries_sari = countries_reporting_sari
      ),
    by = "iso_weekstartdate"
  ) %>%
  arrange(iso_weekstartdate)

print(tail(integrated_weekly))

# Chart 6: Compare influenza positivity with the ILI consultation rate.
# This is an exploratory comparison, not proof that one causes the other.
plot_integrated <- integrated_weekly %>%
  filter(
    !is.na(influenza_positivity_percent),
    !is.na(ili_per_1000_outpatients)
  ) %>%
  ggplot(
    aes(
      x = ili_per_1000_outpatients,
      y = influenza_positivity_percent,
      color = iso_weekstartdate
    )
  ) +
  geom_point(alpha = 0.75, size = 2) +
  scale_color_viridis_c(option = "C", end = 0.9) +
  labs(
    title = "ILI activity and influenza positivity",
    subtitle = str_c(selected_region, " region; each point is one reporting week"),
    x = "ILI cases per 1,000 outpatient visits",
    y = "Influenza positivity (%)",
    color = "Week",
    caption = "Sources: WHO FluNet and FluID"
  ) +
  influenza_theme

print(plot_integrated)


# Step 12: Save tables and charts ----------------------------------------

dir.create("output", showWarnings = FALSE)

write_csv(flunet_weekly, "output/flunet_weekly_summary.csv")
write_csv(flunet_country_summary, "output/flunet_country_summary.csv")
write_csv(fluid_weekly, "output/fluid_weekly_summary.csv")
write_csv(fluid_country_summary, "output/fluid_country_summary.csv")
write_csv(integrated_weekly, "output/influenza_integrated_weekly.csv")

ggsave(
  "output/flunet_weekly_volume.png",
  plot_flunet_volume,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  "output/flunet_positivity.png",
  plot_flunet_positivity,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  "output/flunet_influenza_types.png",
  plot_flunet_types,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  "output/fluid_ili_rate.png",
  plot_fluid_ili,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  "output/fluid_sari_rate.png",
  plot_fluid_sari,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  "output/influenza_integrated.png",
  plot_integrated,
  width = 8,
  height = 6,
  dpi = 300
)


# Practice exercises -----------------------------------------------------

# Exercise 1
# Change selected_region to another WHO region and rerun the analysis.

# Exercise 2
# Compare SENTINEL and NONSENTINEL FluNet reports. Do not add the two
# percentages together.

# Exercise 3
# Use FluNet subtype fields to plot A(H1N1)pdm09 and A(H3) detections.

# Exercise 4
# Filter FluID to one country and compare its ILI and SARI indicators.

# Exercise 5
# Create a chart showing the number of countries reporting each week.

# Exercise 6
# Explore age-specific FluID records. Keep them separate from ALL records to
# avoid double counting.


# Interpretation notes ---------------------------------------------------

# - FluNet detections depend on who is tested and which laboratories report.
# - FluID cases depend on surveillance definitions, sites, and coverage.
# - A missing value means not reported or unavailable; it does not mean zero.
# - Recent weeks may be incomplete because reporting can be delayed.
# - Countries may use different surveillance sources and age categories.
# - Regional rates can change when the countries reporting that week change.
# - ILI and SARI are syndromes and are not all laboratory-confirmed influenza.
# - The integrated chart shows association only and does not establish cause.
