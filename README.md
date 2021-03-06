
<!-- README.md is generated from README.Rmd. Please edit that file -->

# binsmlr

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/rrunner/binsmlr.svg?branch=master)](https://travis-ci.com/rrunner/binsmlr)
[![Codecov test
coverage](https://codecov.io/gh/rrunner/binsmlr/branch/master/graph/badge.svg)](https://codecov.io/gh/rrunner/binsmlr?branch=master)
<!-- badges: end -->

## Overview

binsmlr, as in *bin similar*, is a package to create bins that are
assessed to be heterogenous (in terms of default risk) across a range of
ordinal values. The binning can be performed automatically,
interactively and manually.

The objective is to create groups of data (bins) across a range of
ordinal values. The bins produced should be homogenous within and
heterogenous across bins in terms of the default risk. A binning
approach, in the context of data transformation, is common in different
credit risk applications and in particular to credit risk portfolios
having a relatively large number of defaults. A prerequisite is that
each observation is assigned an ordinal value such as score or rating
etc., that indicates the relative default risk as proposed by the rank
ordering model. Also, a default indicator is required.

The main process is the following:

1.  Create a relatively large set of initial bins.

2.  Perform homogeneity tests between all adjacent bins

3.  Merge the pair of bins that are the most similar in terms of
    proportions of state of non-default and state of default.

4.  Repeat step 2 and 3 until all bins are heterogeneous or if a minimum
    number of bins are obtained.

The process above should result in bins that are statistically different
in terms of default risk across a range of ordinal data.

## Installation

Install the development version from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("rrunner/binsmlr")
```

## Usage

``` r
library(binsmlr)
help("binsmlr")

# example of interactive binning

# create initial bins
bins <- create_initial_bins(bin_data, 30, "score", "default")
length(bins)
is_monotonic(bins)

# reduce bins by performing repeated homogeneity tests
new_bins <- reduce_bins(bins, min_required_bins = 7, confidence_level = 0.01)
length(new_bins)
is_monotonic(new_bins)

# plot initial and reduced bins
bins_df <- dplyr::bind_rows(bins)
plot(x = bins_df$mid_score, y = log(bins_df$odds), type = "p",
     col = "lightblue", cex = 1.5, pch = 20, ylab = "log(odds)",
     xlab = "score")

new_bins_df <- dplyr::bind_rows(new_bins)
points(x = new_bins_df$mid_score, y = log(new_bins_df$odds),
       col = "darkblue", cex = 1.5, pch = 20)
legend(x = "topright", legend = c("Initial bins", "Reduced bins"),
       col = c("lightblue", "darkblue"), pch = 20, pt.cex = 1.5, bty = "n")
```

## Getting help and report issues

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/rrunner/binsmlr/issues).
