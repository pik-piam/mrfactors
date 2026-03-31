# readRegressionsILO

Read regression coefficients which are used to fill missing values of
ILO datasets

## Usage

``` r
readRegressionsILO(subtype = "AgEmplShare")
```

## Arguments

- subtype:

  Type of ILOSTAT data for which regression coefficients should be read

  - \`AgEmplShare\`: regression coefficients for sqr(ag empl share) ~
    log(GDP pc PPP)

  - \`HourlyLaborCosts\`: regression coefficients for hourly labor costs
    ~ GDP pc MER (old version) or log(hourly labor costs) ~ log(GDP pc
    MER) (new version)

  The version of regression and underlying data can be chosen by adding
  a suffix to the subtype, "" for the oldest version, or "\_monthYear"
  (e.g. "\_Aug24") for newer version

## Value

regression coefficients as MAgPIE object

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
  readSource("RegressionsILO", "AgEmpl")
} # }
```
