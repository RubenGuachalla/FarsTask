#necessary libraries
library(testthat)
library(FarsTask)
library(dplyr)
library(readr)
library(tidyr)
library(magrittr)
library(graphics)
library(maps)

# test make_filename
devttest_that("make_filename function testing on year 2013", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")})

test_that("make_filename function testing on year 2014", {
  expect_equal(make_filename(2014), "accident_2014.csv.bz2")})

test_that("make_filename function testing on year 2015", {
  expect_equal(make_filename(2015), "accident_2015.csv.bz2")})
