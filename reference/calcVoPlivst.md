# calcVoPlivst

Calculates the value of production of individual livestock categories

## Usage

``` r
calcVoPlivst(other = FALSE, fillGaps = TRUE, unit = "constant 2017 US$MER")
```

## Arguments

- other:

  boolean: should FAO livestock categories that can't be matched to
  MAgPIE categories (i.e. beeswax, wool, silkworms, and honey) be
  reported as "livst_other"?

- fillGaps:

  boolean: should gaps be filled using production \* prices (where
  production data is available)?

- unit:

  output currency unit based on the toolConvertGDP function from the
  GDPuc library

## Value

magpie object

## See also

\[calcOutput()\]

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
a <- calcOutput("VoPlivst")
} # }
```
