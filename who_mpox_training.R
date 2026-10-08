# WHO Global Mpox Data: Beginner Analysis and Visualization
# ==========================================================
#
# Learning goals
# 1. Import data from a public API.
# 2. Inspect and clean a dataset.
# 3. Create summaries with dplyr.
# 4. Reshape data with tidyr.
# 5. Build charts with ggplot2.
# 6. Save tables and charts.
#
# pacman checks whether packages are installed. Missing packages are installed
# automatically, and then all packages are loaded.
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  janitor
)

# A small base R helper for readable axis labels such as 10,000.
comma_labels <- function(numbers) {
  format(numbers, big.mark = ",", scientific = FALSE, trim = TRUE)
}


# 1. Import the data ------------------------------------------------------

# The WHO endpoint normally returns JSON. Adding ?$format=csv lets read_csv()
# read it directly without needing an additional JSON package.
api_url <- paste0(
  "https://xmart-api-public.who.int/MPX/",
  "V_MPX_VALIDATED_DAILY?$format=csv"
)

mpox_raw <- read_csv(api_url, show_col_types = FALSE)

# Look at the first rows, column names, and column types.
glimpse(mpox_raw)
print(head(mpox_raw))


# 2. Clean the data -------------------------------------------------------

# clean_names() changes names such as COUNTRY and TOTAL_CONF_CASES into
# country and total_conf_cases. The date is changed from text to a proper Date
# value so R can sort and plot it correctly.
mpox <- mpox_raw %>%
  clean_names() %>%
  mutate(date = as.Date(date)) %>%
  arrange(date, country)

# Check the cleaned dataset.
glimpse(mpox)

# Count missing values in every column.
missing_values <- mpox %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "column",
    values_to = "missing_count"
  )

print(missing_values)

# Check whether a country has more than one row on the same date.
duplicate_country_dates <- mpox %>%
  count(iso3, date, name = "row_count") %>%
  filter(row_count > 1)

print(duplicate_country_dates)


# 3. Create a latest snapshot --------------------------------------------

# Countries do not always report on the same day. For each country, keep its
# most recent available row before comparing totals.
latest_country <- mpox %>%
  filter(!is.na(iso3)) %>%
  group_by(iso3) %>%
  arrange(date, .by_group = TRUE) %>%
  slice_tail(n = 1) %>%
  ungroup()

# A small global summary using each country's latest report.
global_summary <- latest_country %>%
  summarise(
    countries_reporting = n_distinct(iso3),
    confirmed_cases = sum(total_conf_cases, na.rm = TRUE),
    confirmed_deaths = sum(total_conf_deaths, na.rm = TRUE)
  )

print(global_summary)

# Latest totals by WHO region.
region_summary <- latest_country %>%
  group_by(who_region) %>%
  summarise(
    countries_reporting = n_distinct(iso3),
    confirmed_cases = sum(total_conf_cases, na.rm = TRUE),
    confirmed_deaths = sum(total_conf_deaths, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(confirmed_cases))

print(region_summary)

# Ten countries with the highest latest confirmed case totals.
top_10_countries <- latest_country %>%
  select(country, who_region, confirmed_cases = total_conf_cases) %>%
  arrange(desc(confirmed_cases)) %>%
  slice_head(n = 10)

print(top_10_countries)


# 4. Prepare complete daily totals ---------------------------------------

# Reporting dates are not complete for every country. Here we create every
# country-date combination, carry the latest total forward, and use zero for
# dates before a country's first report. This makes global trend totals more
# meaningful than simply adding the rows that happen to exist on each date.
all_dates <- seq(min(mpox$date), max(mpox$date), by = "day")

mpox_daily <- mpox %>%
  select(
    country,
    iso3,
    who_region,
    date,
    total_conf_cases,
    total_conf_deaths
  ) %>%
  filter(!is.na(iso3)) %>%
  distinct(iso3, date, .keep_all = TRUE) %>%
  complete(iso3, date = all_dates) %>%
  group_by(iso3) %>%
  arrange(date, .by_group = TRUE) %>%
  fill(country, who_region, .direction = "downup") %>%
  fill(total_conf_cases, total_conf_deaths, .direction = "down") %>%
  mutate(
    total_conf_cases = replace_na(total_conf_cases, 0),
    total_conf_deaths = replace_na(total_conf_deaths, 0)
  ) %>%
  ungroup()

global_daily <- mpox_daily %>%
  group_by(date) %>%
  summarise(
    confirmed_cases = sum(total_conf_cases),
    confirmed_deaths = sum(total_conf_deaths),
    .groups = "drop"
  )


# 5. Visualize the data ---------------------------------------------------

# A simple theme used by every chart.
mpox_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    panel.grid.minor = element_blank()
  )

# Chart 1: Global cumulative confirmed cases over time.
plot_global_trend <- global_daily %>%
  ggplot(aes(x = date, y = confirmed_cases)) +
  geom_line(color = "#2C7FB8", linewidth = 0.9) +
  scale_y_continuous(labels = comma_labels) +
  labs(
    title = "Reported mpox cases worldwide",
    subtitle = "Cumulative confirmed cases, using the latest available country totals",
    x = NULL,
    y = "Confirmed cases",
    caption = "Source: WHO Global Mpox Trends"
  ) +
  mpox_theme

print(plot_global_trend)

# Chart 2: Latest confirmed cases by WHO region.
plot_regions <- region_summary %>%
  mutate(who_region = fct_reorder(who_region, confirmed_cases)) %>%
  ggplot(aes(x = confirmed_cases, y = who_region)) +
  geom_col(fill = "#41AB5D", width = 0.7) +
  scale_x_continuous(labels = comma_labels) +
  labs(
    title = "Reported mpox cases by WHO region",
    subtitle = "Sum of each country's latest confirmed total",
    x = "Confirmed cases",
    y = NULL,
    caption = "Source: WHO Global Mpox Trends"
  ) +
  mpox_theme

print(plot_regions)

# Chart 3: Countries with the highest confirmed case totals.
plot_top_10 <- top_10_countries %>%
  mutate(country = fct_reorder(country, confirmed_cases)) %>%
  ggplot(aes(x = confirmed_cases, y = country, fill = who_region)) +
  geom_col(width = 0.7, show.legend = TRUE) +
  scale_x_continuous(labels = comma_labels) +
  labs(
    title = "Countries with the most reported mpox cases",
    subtitle = "Latest confirmed total available for each country",
    x = "Confirmed cases",
    y = NULL,
    fill = "WHO region",
    caption = "Source: WHO Global Mpox Trends"
  ) +
  mpox_theme

print(plot_top_10)

# Chart 4: Monthly newly reported confirmed cases.
# as.Date(format(...)) changes every date to the first day of its month.
monthly_cases <- mpox %>%
  mutate(month = as.Date(format(date, "%Y-%m-01"))) %>%
  group_by(month) %>%
  summarise(
    new_confirmed_cases = sum(new_conf_cases, na.rm = TRUE),
    .groups = "drop"
  )

plot_monthly_cases <- monthly_cases %>%
  ggplot(aes(x = month, y = new_confirmed_cases)) +
  geom_col(fill = "#756BB1", width = 25) +
  scale_y_continuous(labels = comma_labels) +
  labs(
    title = "Newly reported mpox cases by month",
    subtitle = "Monthly sum of the WHO daily new-case field",
    x = NULL,
    y = "New confirmed cases",
    caption = "Negative values can occur when earlier reports are corrected. Source: WHO"
  ) +
  mpox_theme

print(plot_monthly_cases)

# Chart 5: Compare selected countries.
# Change the ISO3 codes below to explore other countries.
selected_iso3 <- c("BRA", "COD", "ESP", "GBR", "USA")

selected_countries <- mpox_daily %>%
  filter(iso3 %in% selected_iso3)

plot_selected_countries <- selected_countries %>%
  ggplot(aes(x = date, y = total_conf_cases, color = country)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = comma_labels) +
  labs(
    title = "Reported mpox cases in selected countries",
    subtitle = "Cumulative confirmed cases",
    x = NULL,
    y = "Confirmed cases",
    color = "Country",
    caption = "Source: WHO Global Mpox Trends"
  ) +
  mpox_theme

print(plot_selected_countries)


# 6. Save results ---------------------------------------------------------

# Create an output folder if it does not already exist.
dir.create("output", showWarnings = FALSE)

write_csv(region_summary, "output/mpox_region_summary.csv")
write_csv(top_10_countries, "output/mpox_top_10_countries.csv")

ggsave(
  filename = "output/mpox_global_trend.png",
  plot = plot_global_trend,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "output/mpox_cases_by_region.png",
  plot = plot_regions,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "output/mpox_top_10_countries.png",
  plot = plot_top_10,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "output/mpox_monthly_cases.png",
  plot = plot_monthly_cases,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "output/mpox_selected_countries.png",
  plot = plot_selected_countries,
  width = 9,
  height = 5,
  dpi = 300
)


# 7. Practice exercises ---------------------------------------------------

# Exercise 1
# Filter the data to one WHO region and calculate its latest total cases.

# Exercise 2
# Change selected_iso3 and compare five countries of your choice.

# Exercise 3
# Make a bar chart of confirmed deaths by WHO region.

# Exercise 4
# Find the five country reports with the largest value of new_conf_cases.

# Exercise 5
# Change one chart's colors, title, and theme.


# Important interpretation notes -----------------------------------------

# - These are reported surveillance data, not estimates of all infections.
# - A zero may mean no newly reported cases, not necessarily no transmission.
# - Countries report on different schedules.
# - Historical totals may change when countries correct earlier reports.
# - Compare counts carefully because country population sizes are different.
