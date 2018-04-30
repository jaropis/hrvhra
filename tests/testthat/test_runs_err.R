library(hrvhra)
context("Runs input data")
test_that("Behavior with good and bad data", {
  # first expect a good dataset to run through silently
  expect_silent(countruns(RR$RR, RR$flags))
  expect_silent(countruns(c(1, 2, 3, 4, 5), c(0, 0, 0, 0, 0)))

  # now for some errors
  expect_error(countruns("1, 2, 3, 4, 5", c(0, 0, 0, 0, 0)), "the rr vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  expect_error(countruns(c("1", "2", "3", "3", "1"), c(0, 0, 0, 0, 0)),
               "the rr vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  expect_error(countruns(c(1, 2, NA, 4, 5), c(0, 0, 0, 0, 0)),"the rr vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  expect_error(countruns(c(1, 2), c(0, 0)), "RR vector too short to define a 'run'")
  expect_error(countruns(c(1, 2, 3, 4, 5), c("0", "0", "0", "0", "0")), "the annotations vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  expect_error(countruns(c(1, 2, 3, 4, 5), "0, 0, 0, 0, 0"), "the annotations vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  expect_error(countruns(c(1, 2, 3, 4, 5), c(0, 0, NA, 0, 0)), "the annotations vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  expect_error(countruns(c(1, 2, 3), c(1)), "annotations and RR vectors need to be of the same length")

  expect_error(countruns(c(1, 2, 3, 4, 5), c(1, 2, 1, 2, 1)), "no segments of continuous RR intervals in this dataset")
  expect_error(countruns(c(1, 2, 1, 2, 1), c(0, 2, 1, 1, 1)), "no full runs in this dataset")
})
