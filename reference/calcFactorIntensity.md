# calcFactorIntensity

Calculates factor intensity in crop production for labour and/or capital
from USDA (Inputs share) and FAO (Value of Production)in constant 2017
US\$MER per ton. Capital intensity and requirements can also be
calculated from FAO's CapitalStock database.

## Usage

``` r
calcFactorIntensity(
  output = "intensities",
  method = "USDA",
  unit = "constant 2017 US$MER"
)
```

## Arguments

- output:

  needed outputs. It can be either "intensities" (Capital/Labour factor
  intensities), "requirements" (Capital Stock requirements per ton), and
  "CapitalShare" for "USDA" method. For the "CapitalStock" method only
  "intensities" and "requirements" outputs supported.

- method:

  "USDA" or "CapitalStock"

- unit:

  output currency unit based on the toolConvertGDP function from the
  GDPuc library

## Value

magpie object of the factor requirements intensity or factor intensity
in USD/tDM per crop, or capital share fraction.

## See also

\[calcOutput()\]

## Author

Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
a <- calcOutput("FactorIntensity")
} # }
```
