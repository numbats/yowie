
<!-- README.md is generated from README.Rmd. Please edit that file -->

# yowie

<!-- badges: start -->

[![R-CMD-check](https://github.com/numbats/yowie/workflows/R-CMD-check/badge.svg)](https://github.com/numbats/yowie/actions)
<!-- badges: end -->

The goal of yowie is to provide longitudinal wages data sets along with
several demographic variables from The National Longitudinal Survey of
Youth (NLSY) from survey year 1979 to 2018. There are three data sets in
this package:

  - `wages_hs2020`: The wages data from the cohort whose highest grade
    completed is up to 12th grade.
  - `wages_hs_dropout`: The wages data of the high school dropouts.
  - `demographic_nlsy79`: The demographic data of the NLSY79 cohort.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("numbats/yowie")
```

## Example

Here is the example of plotting some samples of subjects in
`wages_hs2020` using `brolgar` (Tierney, Cook, and Prvan, 2020).

``` r
library(yowie)
library(brolgar)
library(ggplot2)

set.seed(20210217)

wages_hs2020 <- as_tsibble(x = wages_hs2020,
                    key = id,
                    index = year,
                    regular = FALSE)

ggplot(wages_hs2020, 
       aes(x = year,
                y = mean_hourly_wage,
                group = id)) +
  geom_line(alpha = 0.7) +
  facet_sample() +
  ylab("mean hourly wage") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
