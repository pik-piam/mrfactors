# calcAgEmplILO

calculates complete dataset of number of people employed in agriculture,
forestry and fishery based on ILO modelles dataset and GDPpcPPP05 for
regression

## Usage

``` r
calcAgEmplILO(
  subsectors = TRUE,
  inclFish = FALSE,
  inclForest = FALSE,
  dataVersionILO = "Aug24"
)
```

## Arguments

- subsectors:

  boolean: should overall values be split into the sub-sectors
  agriculture, forestry and fishery based on their relative share of
  people employed, and agriculture further split into crops and
  livestock based on VoP

- inclFish:

  boolean: should employment in fisheries be included?

- inclForest:

  boolean: should emplyoment in forestry be included?

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
calcOutput("AgEmplILO")
} # }
```
