# calcHourlyLaborCosts

calculates dataset of hourly labor costs per employee in agriculture

## Usage

``` r
calcHourlyLaborCosts(
  datasource = "USDA_FAO",
  dataVersionILO = "Aug24",
  sector = "agriculture",
  fillWithRegression = TRUE,
  calibYear = 2010,
  cutAfterCalibYear = TRUE,
  projection = FALSE
)
```

## Arguments

- datasource:

  either raw data from "ILO" (agriculture+forestry+fishery) or data
  calculated based on total labor costs from "USDA_FAO" (crop+livestock
  production).

- dataVersionILO:

  Which version of ILO data to use (for hourly labor costs if source is
  ILO, for ag empl. if source is USDA_FAO). "" for the oldest version,
  or "monthYear" (e.g. "Aug24") for a newer version)

- sector:

  should average hourly labor costs be reported ("agriculture"), or
  hourly labor costs specific to either "crops" or "livestock"
  production. For ILO only the aggregate hourly labor costs are
  available.

- fillWithRegression:

  boolean: should missing values be filled based on a regression between
  ILO hourly labor costs and GDPpcMER (calibrated to countries)

- calibYear:

  in case of fillWithRegression being TRUE, data after this year will be
  ignored and calculated using the regression (calibrated for each year
  to calibYear, or the most recent year with data before calibYear).
  NULL if all data should be used for calibration

- cutAfterCalibYear:

  boolean, only relevant if fillWithRegression is TRUE. If
  cutAfterCalibYear is TRUE, raw data after the calib year is
  overwritten by regression results (necessary for consistency with
  calculation within MAgPIE). If FALSE, raw data is kept and only gaps
  are filled with regression

- projection:

  either FALSE or SSP on which projections should be based. Only
  relevant if fillWithRegression is TRUE.

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("HourlyLaborCosts")
} # }
```
