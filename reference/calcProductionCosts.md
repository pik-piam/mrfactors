# calcProductionCosts

calculates agricultural production costs (split into different cost
categories)

## Usage

``` r
calcProductionCosts(datasource = "Vittis", unit = "constant 2017 US$MER")
```

## Arguments

- datasource:

  Datasource of production costs, currently only "Vittis"

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
calcOutput("ProductionCosts", source = "Vittis")
} # }
```
