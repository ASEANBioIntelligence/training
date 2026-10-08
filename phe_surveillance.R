# Event-Based Surveillance Training: BlueDot API
# ================================================================
#
# This beginner-friendly script demonstrates how to:
# 1. Store an API key safely.
# 2. Define a surveillance period and diseases of interest.
# 3. Send a request to the BlueDot Case Counts API.
# 4. Check whether the request succeeded.
# 5. Convert the JSON response into a tidy table.
# 6. Keep events reported for ASEAN Member States.
# 7. Expand source information and export the result.
#
# Run this script one section at a time. Read the comments before running
# each section, and inspect the new object created by that section.


# Step 1: Install and load packages --------------------------------------

# pacman installs missing packages and loads all listed packages.
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  httr,
  jsonlite,
  tidyverse,
  janitor
)


# Step 2: Store the API key safely ---------------------------------------

# Never type an API key directly in a script that may be shared.
#
# For one R session, run the following command in the Console and replace
# the example text with your real key:
# Sys.setenv(BLUEDOT_API_KEY = "your-key-here")
#
# For a permanent setup, run usethis::edit_r_environ() in the Console, add
# BLUEDOT_API_KEY=your-key-here to the file, save it, and restart RStudio.
# The usethis package is optional and is not needed by this script.

api_key <- Sys.getenv("BLUEDOT_API_KEY")

if (api_key == "") {
  stop(
    paste(
      "BlueDot API key not found.",
      "Set BLUEDOT_API_KEY before running this script."
    )
  )
}


# Step 3: Define the surveillance period --------------------------------

# Enter dates in YYYY-MM-DD format.
start_date <- as.Date("2026-09-26")
end_date <- as.Date("2026-10-03")

# Stop early when the date range is not valid.
if (start_date > end_date) {
  stop("start_date must be before or equal to end_date.")
}

# Optional daily-surveillance dates:
# end_date <- Sys.Date() - 1
# start_date <- end_date - 7


# Step 4: Define diseases and locations ---------------------------------

# BlueDot identifies diseases and locations with numeric IDs.
# Keeping them as character vectors makes the lists easier to read and edit.
disease_ids <- c(
  "15", "141", "83", "164", "84", "166", "55", "53", "88", "38",
  "63", "13", "4", "205", "10", "43", "26", "153", "156", "162",
  "6", "110", "113", "136", "212", "215", "216", "224", "264",
  "200", "41", "40", "39", "37", "64", "239", "47"
)

# BlueDot country IDs for the 11 ASEAN Member States used in this workflow.
asean_country_ids <- c(
  "1880251", "1605651", "1694008", "1820814", "1733045", "1643084",
  "1831722", "1327865", "1655842", "1562822", "1966436"
)

# The API expects disease IDs as one comma-separated value.
disease_ids_parameter <- str_c(disease_ids, collapse = ",")


# Step 5: Build the API request ------------------------------------------

api_url <- "https://developer.bluedot.global/casecounts/"

# Keeping query parameters in a named list makes the request easier to read.
query_parameters <- list(
  diseaseIds = disease_ids_parameter,
  subLocationTypes = 6,
  startDate = format(start_date, "%Y-%m-%d"),
  endDate = format(end_date, "%Y-%m-%d"),
  isAggregated = "false",
  includeSources = "true",
  includeCsv = "false",
  `api-version` = "v1"
)


# Step 6: Send the request -----------------------------------------------

response <- GET(
  url = api_url,
  query = query_parameters,
  add_headers(
    "Ocp-Apim-Subscription-Key" = api_key,
    "Cache-Control" = "no-cache"
  )
)

# Print the HTTP status. A successful request normally returns 200.
status_code(response)

# Stop with a useful message when BlueDot returns an error.
stop_for_status(response, task = "download BlueDot surveillance data")


# Step 7: Read the JSON response -----------------------------------------

# content() converts the JSON response into an R list.
api_result <- content(
  response,
  as = "parsed",
  type = "application/json",
  encoding = "UTF-8"
)

# The event records are stored in the response's data element.
event_records <- pluck(api_result, "data", .default = list())

if (length(event_records) == 0) {
  stop("The request succeeded, but no event records were returned.")
}


# Step 8: Convert records into a tidy table ------------------------------

# Each item in event_records is one event. unnest_wider() changes the fields
# inside each event into table columns. clean_names() creates consistent
# lowercase column names such as country_id and disease_id.
all_national_events <- tibble(event = event_records) %>%
  unnest_wider(event) %>%
  clean_names()

glimpse(all_national_events)


# Step 9: Keep ASEAN national events -------------------------------------

# as.character() ensures that country IDs can be compared even when the API
# returns them as numbers rather than text.
asean_national_events <- all_national_events %>%
  mutate(country_id = as.character(country_id)) %>%
  filter(country_id %in% asean_country_ids)

# Count the records returned for each country.
events_by_country <- asean_national_events %>%
  count(country_id, sort = TRUE, name = "event_count")

print(events_by_country)


# Step 10: Expand event source details -----------------------------------

# min_sources is a nested list because one event can have multiple sources.
# unnest_longer() creates one row per source. unnest_wider() then creates one
# column for each source field. keep_empty = TRUE keeps events without a source.
disease_event_national <- asean_national_events %>%
  select(-any_of("max_sources")) %>%
  unnest_longer(min_sources, keep_empty = TRUE) %>%
  unnest_wider(min_sources, names_sep = "_") %>%
  clean_names()

glimpse(disease_event_national)
print(head(disease_event_national))

# View() opens the final table when the script is run interactively in RStudio.
if (interactive()) {
  View(disease_event_national)
}


# Step 11: Export the training results -----------------------------------

dir.create("output", showWarnings = FALSE)

output_file <- str_c(
  "output/bluedot_asean_events_",
  format(start_date, "%Y%m%d"),
  "_to_",
  format(end_date, "%Y%m%d"),
  ".csv"
)

write_csv(disease_event_national, output_file)

message("Saved surveillance data to: ", output_file)


# Practice exercises -----------------------------------------------------

# Exercise 1
# Change start_date and end_date, then compare the number of records.

# Exercise 2
# Use count() to summarize records by disease ID.

# Exercise 3
# Filter the final table to one ASEAN country.

# Exercise 4
# Find the five diseases with the largest number of event records.

# Exercise 5
# Create a ggplot2 bar chart of event counts by country.


# Interpretation and security notes -------------------------------------

# - BlueDot event records support situational awareness and require review.
# - One event may contain multiple sources and therefore produce multiple rows.
# - No returned records does not necessarily mean no disease activity.
# - Disease and location IDs should be checked against your BlueDot account.
# - Never share API keys in scripts, screenshots, email, or version control.
