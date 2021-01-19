
<!-- README.md is generated from README.Rmd. Please edit that file -->

# JABmisc

<!-- badges: start -->

<!-- badges: end -->

The goal of JABmisc is to improve data analysis workflows by:

1.  simplifying initial data exploration
2.  providing functions to fill gaps in data
    manipulation/munging/wrangling

## Installation

JABmisc currently only exists in a development version. You can install
this version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("allenbaron/JABmisc")
```

## Examples

To get a quick look at a tabular dataset (after loading) use
`describe_cols()` or the full-functioning `audit_df()`. Here’s an
example with the `airquality` dataset:

``` r
library(JABmisc)
describe_cols(airquality)
#> # A tibble: 6 x 6
#>   var    type  missing_n missing_percent unique_n unique_as_string              
#>   <chr>  <chr>     <int>           <dbl>    <int> <chr>                         
#> 1 Ozone  int          37            24.2       67 41 | 36 | 12 | 18 | 28 | 23 |~
#> 2 Solar~ int           7             4.6      117 190 | 118 | 149 | 313 | 299 |~
#> 3 Wind   dbl           0             0         31 7.4 | 8 | 12.6 | 11.5 | 14.3 ~
#> 4 Temp   int           0             0         40 67 | 72 | 74 | 62 | 56 | 66 |~
#> 5 Month  int           0             0          5 5 | 6 | 7 | 8 | 9             
#> 6 Day    int           0             0         31 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8~
```

Other available functions in this package:

  - filter lists by name or value (`filter_list()`)
  - perform numeric formatting (`round_up()`, `format_num()`)
  - or, are *TOTALLY* miscellaneous and provide functionality to help in
    random, odd use-cases where I’ve needed them
    (e.g. `find_consensus_path()`)
