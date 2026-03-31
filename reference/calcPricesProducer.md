# calcPricesProducer

producer prices for agricultural products. 05USDMER/tDM prices from FAO,
no currency conversion

## Usage

``` r
calcPricesProducer(
  products = "kcr",
  calculation = "VoP",
  weighting = "production"
)
```

## Arguments

- products:

  either "kcr" or "kcl"

- calculation:

  type of calculation "FAO" (directly reads the data), "VoP" calculates
  as VoP/Production, only "FAO" available for "kli" products.

- weighting:

  either "production" (default) or "consumption" based weighting

## Value

magpie object. prices in year specific annual

## See also

\[calcOutput()\]

## Author

Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
a <- calcOutput("PricesProducer")
} # }
```
