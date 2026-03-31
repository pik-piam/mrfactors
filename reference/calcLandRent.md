# calcLandRent

Calculates factor intensity in crop production for ladn rent from USDA
(Inputs share) and FAO (Value of Production) and productivity in
constant 2017 US\$MER per ha.

## Usage

``` r
calcLandRent(unit = "constant 2017 US$MER", rent = "cropland")
```

## Arguments

- unit:

  output currency unit based on the toolConvertGDP function from the
  GDPuc library

- rent:

  rent type cropland average "cropland" or per crop type "perCrop"

## Value

magpie object of the land factor requirements in USD/ha per crop

## See also

\[calcOutput()\]

## Author

Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
a <- calcOutput("LandRent")
} # }
```
