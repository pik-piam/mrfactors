# calcFactorCostsCrops

calculates factor costs for crop production in mio. US\$MER05

## Usage

``` r
calcFactorCostsCrops(datasource = "USDA", unit = "constant 2017 US$MER")
```

## Arguments

- datasource:

  only source available is "USDA" (calculates factor costs by applying
  factor cost share from USDA to VoP from FAO)

- unit:

  output currency unit based on the toolConvertGDP function from the
  GDPuc library

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FactorCostsCrops")
} # }
```
