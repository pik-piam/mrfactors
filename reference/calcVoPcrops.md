# calcVoPcrops

Calculates the value of production of individual production items or its
fraction compared to overall Value of Production (Agriculture, Fish,
Forestry).

## Usage

``` r
calcVoPcrops(fillGaps = TRUE, unit = "constant 2017 US$MER")
```

## Arguments

- fillGaps:

  boolean: should gaps be filled using production \* prices (where
  production data is available)?

- unit:

  output currency unit based on the toolConvertGDP function from the
  GDPuc library

## Value

magpie object. in mio. USD or fraction

## See also

\[calcOutput()\]

## Author

Edna J. Molina Bacca, Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
a <- calcOutput("VoPcrops")
} # }
```
