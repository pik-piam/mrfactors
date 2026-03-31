# calcVoPAFF

Calculates the overall value of production of the agriculture, forestry
and fisheries sectors. Forestry and Fisheries are calculated from
exports values.

## Usage

``` r
calcVoPAFF(unit = "constant 2017 US$MER")
```

## Arguments

- unit:

  output currency unit based on the toolConvertGDP function from the
  GDPuc library

## Value

magpie object. in mio. USD

## See also

\[calcOutput()\]

## Author

Edna J. Molina Bacca, Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
a <- calcOutput("VoPAFF")
} # }
```
