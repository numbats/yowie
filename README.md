
<!-- README.md is generated from README.Rmd. Please edit that file -->

# yowie <a href='https://numbats.github.io/yowie/'><img src='man/figures/logo.png' align="right" height="138.5" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/numbats/yowie/workflows/R-CMD-check/badge.svg)](https://github.com/numbats/yowie/actions)
<!-- badges: end -->

The goal of `yowie` is to provide longitudinal wages data sets along
with several demographic variables of Americans from [the National
Longitudinal Survey of Youth
(NLSY79)](https://www.nlsinfo.org/content/cohorts/nlsy79) from Round 1
(the survey year 1979) to Round 28 (the survey year 2018). The NLSY79 is
a longitudinal project held by the U.S. Bureau of Labor Statistics that
follows the lives of a sample of American youth born between 1957-64.
The cohort originally included 12,686 respondents ages 14-22 when first
interviewed in 1979. There are three data sets provided in this package:

  - `wages`: The wages data from the cohort whose highest grade
    completed is up to 12th grade.
  - `wages_hs_do`: The wages data of the high school dropouts.
  - `demog_nlsy79`: The demographic data of the NLSY79 cohort.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("numbats/yowie")
```

## Example

Here is the example of plotting some samples of subjects in `wages_hs`
using `brolgar` (Tierney, Cook, and Prvan, 2020).

``` r
library(yowie)
library(brolgar)
library(ggplot2)

set.seed(20210217)

ggplot(yowie::wages, 
       aes(x = year,
           y = wage,
           group = id)) +
  geom_line(alpha = 0.7) +
  facet_sample() +
  ylab("mean hourly wage") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

## About the package name

A [yowie](https://en.wikipedia.org/wiki/Yowie) is a mythical creature in
Australian foklore akin to Big Foot or Sasquatch in north America. In
terms of the package it can be considered an acronym: Years Of Wages to
Investigate and Explore.
