library(testthat)
library(courserafars)

expect_that(make_filename(2012), equals("accident_2012.csv.bz2"))

