# This script is run ONCE, by you, to turn your existing
# AQALY_estim_combined_rep_data_UK.RDS file into the package's internal
# data object. It is not run by users who install the package - only the
# resulting data/estimatesAQALY.rda file (created below) ships with the
# package.
#
# 1. Put AQALY_estim_combined_rep_data_UK.RDS in this data-raw/ folder
#    (or adjust the path below).
# 2. Run this script from the package root, e.g. with:
#      source("data-raw/prepare_data.R")
#    or, if you use usethis/devtools interactively, just run the lines
#    below from the R console with your working directory set to the
#    package root.

estimatesAQALY <- readRDS("data-raw/AQALY_estim_combined_rep_data_UK.RDS")

# Sanity check: the fields InstantAQALYscore() relies on must be present
stopifnot(
  !is.null(estimatesAQALY$coefficients),
  !is.null(estimatesAQALY$vcov)
)

# Saves estimatesAQALY as data/estimatesAQALY.rda and registers it in
# DESCRIPTION's LazyData field. Requires the usethis package
# (install.packages("usethis") if needed).
usethis::use_data(estimatesAQALY, overwrite = TRUE)
