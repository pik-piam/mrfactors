# calcFactorCostsLivst

calculates factor costs for livestock production in mio. US\$MER05

## Usage

``` r
calcFactorCostsLivst(
  datasource = "USDA",
  otherLivst = FALSE,
  inclFish = FALSE,
  unit = "constant 2017 US$MER"
)
```

## Arguments

- datasource:

  only source available is "USDA" (calculates factor costs by applying
  factor cost share from USDA to VoP from FAO)

- otherLivst:

  boolean: should FAO livestock categories that can't be matched to
  MAgPIE categories (i.e. beeswax, wool, silkworms, and honey) be
  reported as "livst_other"?

- inclFish:

  boolean: should fish factor costs be included?

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
calcOutput("FactorCostsLivst")
} # }
```
