# convertILOSTAT

transforms currencies where applicable, and fills missing countries in
ILOSTAT data with 0

## Usage

``` r
convertILOSTAT(x)
```

## Arguments

- x:

  unconverted magpie object from read-script

## Value

Data as MAgPIE object with common country list

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("ILOSTAT", subtype = "EmplByActivityModelled", convert = TRUE)
} # }
```
