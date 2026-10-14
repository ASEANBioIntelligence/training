# WHO Influenza Surveillance Training in R

This lesson uses two public datasets from the WHO Global Influenza
Surveillance and Response System (GISRS):

- **FluNet** for weekly virological and laboratory surveillance.
- **FluID** for weekly epidemiological surveillance of influenza-like illness
  (ILI) and severe acute respiratory infection (SARI).

## Requirements

- R and RStudio
- An internet connection
- The `pacman` package manager

The script uses `pacman` to install and load `tidyverse` and `janitor`. If
`pacman` is missing, the script installs it automatically.

## How to use the lesson

1. Open `who_influenza_training.R` in RStudio.
2. Run one numbered step at a time from top to bottom.
3. Inspect printed tables, charts, and objects in the Environment pane.
4. Change `selected_region`, `selected_source`, or `start_year` and rerun.
5. Complete the exercises at the end of the script.

The default analysis uses recent data for the WHO South-East Asia Region and
sentinel FluNet reports. Results are written to the `output` folder.

## Indicators used

| Dataset | Indicator | Calculation |
|---|---|---|
| FluNet | Influenza positivity | Influenza-positive specimens / specimens tested × 100 |
| FluNet | Influenza type | Weekly WHO totals for influenza A and B detections |
| FluID | ILI outpatient rate | ILI cases / outpatient visits × 1,000 |
| FluID | SARI inpatient rate | SARI cases / inpatient admissions × 100 |

## Important analysis choices

- FluNet is filtered to one surveillance source before aggregation.
- FluID uses `ALL` age rows so age-specific and all-age counts are not added
  together.
- Numerators and denominators are added before calculating regional rates.
- Missing values remain missing when no country reported a measure.
- FluNet and FluID are joined by ISO week only for exploratory comparison.

## WHO sources

- [WHO FluNet](https://www.who.int/tools/flunet/)
- [WHO FluID](https://www.who.int/teams/global-influenza-programme/surveillance-and-monitoring/fluid)
- [WHO GISRS](https://www.who.int/initiatives/global-influenza-surveillance-and-response-system)
