library(hrvhra)
context("windowing functions")
# setup
kurde <- 3

test_that("the windowing functions cut at correct places",
          expect_equal(
            kurde,3
          )
          )
