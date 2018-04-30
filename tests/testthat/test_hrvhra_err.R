library(hrvhra)
context("HRV and HRA input data")
test_that("HRV and HRA behavior is as expected on good and bad input data", {
  # first some good data
  expect_silent(hrvhra(RR$RR, RR$flags))
  expect_silent(hrvhra(c(992.500, 756.250, 798.750, 821.875, 853.125, 875.000, 858.750, 796.250),
                      c(0, 0, 0, 0, 0, 0, 0, 0)))
  expect_silent(drawpp(RR$RR, RR$flags))
  expect_silent(drawpp(c(992.500, 756.250, 798.750, 821.875, 853.125, 875.000, 858.750, 796.250),
                       c(0, 0, 0, 0, 0, 0, 0, 0)))
  expect_silent(describerr(RR$flags))
  expect_silent(describerr(c(0, 0, 0, 0, 0, 0, 0, 0)))


  expect_error(hrvhra("992.500, 756.250, 798.750, 821.875, 853.125, 875.000, 858.750, 796.250",
                      c(0, 0, 0, 0, 0, 0, 0, 0)),
               "the rr vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  expect_error(hrvhra(c("992.500", "756.250", "798.750", "821.875", "853.125", "875.000", "858.750", "796.250"),
                      c(0, 0, 0, 0, 0, 0, 0, 0)),
               "the rr vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  expect_error(hrvhra(c(992.500, 756.250, 798.750, 821.875, NA, 875.000, 858.750, 796.250),
                      c(0, 0, 0, 0, 0, 0, 0, 0)),
               "the rr vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")

  expect_error(hrvhra(c(992.500, 756.250, 798.750, 821.875, 853.125, 875.000, 858.750, 796.250),
                       "0, 0, 0, 0, 0, 0, 0, 0"),
                "the annotations vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  expect_error(hrvhra(c(992.500, 756.250, 798.750, 821.875, 853.125, 875.000, 858.750, 796.250),
                      c("0", "0", "0", "0", "0", "0", "0", "0")),
               "the annotations vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  expect_error(hrvhra(c(992.500, 756.250, 798.750, 821.875, 853.125, 875.000, 858.750, 796.250),
                      c(0, 0, 0, 0, 0, NA, 0, 0)),
               "the annotations vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  expect_error(hrvhra(c(992.500, 756.250, 798.750, 821.875, 853.125, 875.000, 858.750, 796.250),
                      c(0, 0, 0, 0, 0, 0, 0)),
               "annotations and RR vectors need to be of the same length")
  expect_error(hrvhra(c(992.500, 756.250, 798.750, 821.875, 853.125, 875.000, 858.750, 796.250),
                      c(1, 0, 1, 0, 1, 0, 0, 1)),
               "too many non-sinus beats to proceed")

  expect_warning(hrvhra(rep(992.5, 10), rep(0, 10)), "There is no variability - is this a pacemaker?")
  expect_warning(hrvhra(c(992.5, 756.205, 992.5, 756.205, 992.5, 756.205, 992.5, 756.205, 992.5, 756.205), rep(0, 10)),
                 "There is no long-term variability - is this a bigeminy?")

  expect_error(drawpp(RR$RR, RR$flags, vname=3), "vname needs to be a string")
  expect_error(drawpp(c(992.500, 756.250, 798.750, 821.875, 853.125, 875.000, 858.750, 796.250),
                      "0, 0, 0, 0, 0, 0, 0, 0"),
               "the annotations vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  expect_error(drawpp(c(992.500, 756.250, 798.750, 821.875, 853.125, 875.000, 858.750, 796.250),
                      c("0", "0", "0", "0", "0", "0", "0", "0")),
               "the annotations vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  expect_error(drawpp(c(992.500, 756.250, 798.750, 821.875, 853.125, 875.000, 858.750, 796.250),
                      c(0, 0, 0, 0, 0, NA, 0, 0)),
               "the annotations vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  # this should pass a value to a parameter which is already defined in the function
  expect_silent(drawpp(RR$RR, RR$flags, vname="P", bg="red"))
  # this should pass a value to a parameter which is not present in the function
  expect_silent(drawpp(RR$RR, RR$flags, vname="P", main="A test Poincare plot"))

  expect_error(describerr("0, 0, 0, 0, 0, 0, 0, 0"),
               "the annotations vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  expect_error(describerr(c("0", "0", "0", "0", "0", "0", "0", "0")),
               "the annotations vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  expect_error(describerr(c(0, 0, 0, 0, 0, NA, 0, 0)),
               "the annotations vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")


})
