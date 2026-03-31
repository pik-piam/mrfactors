# readILOSTAT

Read in ILOSTAT data that has been downloaded from the ILOSTAT website

## Usage

``` r
readILOSTAT(subtype)
```

## Arguments

- subtype:

  Type of ILOSTAT data that should be downloaded, version specified by
  suffic "\_MonthYear" (month and year of download)

  - \`EmplByActivityModelled\`: "Employment by sex and economic activity
    – ILO modelled estimates, Nov. 2020 (thousands)"

  - \`WeeklyHoursByActivity\`: "Mean weekly hours actually worked per
    employed person by sex and economic activity"

  - \`HourlyLaborCostsByActivity\`: "Mean nominal hourly labour cost per
    employee by economic activity"

  - \`EmplByISIC2\`: "Employment by sex and economic activity - ISIC
    level 2 (thousands) \| Annual"

  - \`EmplByActivityMonthly\`: "Employment by sex and economic activity
    (thousands) \| Monthly"

  - \`EmplByActivityMonthlyAdj\`: "Employment by sex and economic
    activity, seasonally adjusted series (thousands) \| Monthly"

  - \`EmplByActivityAndStatus\`: "Employment by sex, status in
    employment and economic activity (thousands) \| Annual"

  - \`WeeklyHoursByActivityMonthly\`: "Mean weekly hours actually worked
    per employee by sex and economic activity \| Monthly"

  - \`WeeklyHoursByISIC2\`: "Mean weekly hours actually worked per
    employed person by sex and economic activity - ISIC level 2 \|
    Annual"

  - \`WeeklyHoursEmployeesByISIC2\`: "Mean weekly hours actually worked
    per employee by sex and economic activity - ISIC level 2 \| Annual"

  - \`LaborIncomeShareGDPModelled\`: "Labour income share as a percent
    of GDP – ILO modelled estimates, July 2019 (

  - \`OutputPerWorkerModelled\`: "Output per worker (GDP constant 2010
    US \$) – ILO modelled estimates, Nov. 2020 \| Annual"

## Value

ILOSTAT data as MAgPIE object

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
  readSource("ILOSTAT", "EmplByActivityModelled")
} # }
```
