# WHO Global Mpox Data Training in R

This beginner lesson imports current WHO Global Mpox data, cleans it, creates
summary tables, builds five charts, and saves selected results.

## Requirements

- R and RStudio
- The `pacman` package manager
- The `tidyverse` and `janitor` packages
- An internet connection while importing the WHO data

The script uses `pacman` to install and load `tidyverse` and `janitor`. If
`pacman` is missing, the script installs it automatically. You can also install
it yourself once in the R console:

```r
install.packages("pacman")
```

Open `who_mpox_training.R` in RStudio. Run one section at a time from top to
bottom. Read the comments before running each section, and inspect each object
in the Environment pane.

## Lesson sequence

1. Import live CSV data from the WHO API.
2. Clean column names with `janitor` and convert the date.
3. Check missing and duplicate values.
4. Calculate global, regional, and country summaries.
5. Prepare a complete daily country panel.
6. Create five `ggplot2` visualizations.
7. Export summary tables and PNG charts.
8. Complete five practice exercises.

## Main WHO fields

| Field | Meaning |
|---|---|
| `country` | Country or reporting area |
| `date` | Reporting date |
| `iso3` | Three-letter country code |
| `who_region` | WHO region code |
| `total_conf_cases` | Cumulative confirmed cases |
| `total_prob_cases` | Cumulative probable cases |
| `total_conf_deaths` | Cumulative confirmed deaths |
| `new_conf_cases` | Newly reported confirmed cases |
| `new_prob_cases` | Newly reported probable cases |
| `new_conf_deaths` | Newly reported confirmed deaths |

## Important note

The API is live, so results can change when WHO adds or corrects reports.
Reported counts are surveillance data and should not be interpreted as the
true number of all infections. Countries also report on different schedules.

Data source: [WHO Global Mpox Trends API](https://xmart-api-public.who.int/MPX/V_MPX_VALIDATED_DAILY)
