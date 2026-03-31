# calcWeeklyHoursILO

calculates complete dataset of mean weekly hours worked by people
employed in agriculture, forestry and fishery based on ILO dataset

## Usage

``` r
calcWeeklyHoursILO(projections = FALSE, dataVersionILO = "Aug24")
```

## Arguments

- projections:

  boolean, should weekly hours be projected (by keeping constant) up to
  2150?

- dataVersionILO:

  which version of the ILO input data to use. "" for the oldest version
  and old regression, or "monthYear" (e.g. "Aug24") for newer data

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("WeeklyHoursILO")
} # }
```
