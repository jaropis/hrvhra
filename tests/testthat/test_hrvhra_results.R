library(hrvhra)
context("HRV and HRA results correctness")
test_that("HRV and HRA results agree with known or easily computable results",
          {
            # the RR dataset is well known and the results have been obtained in many ways
            expect_equal(
              hrvhra(RR$RR, RR$flags),
              c(
                SDNN = 66.31092,
                SD1 = 36.80717,
                SD2 = 86.25258,
                SD1I = 36.80717,
                SDNNd = 45.94416,
                SDNNa = 47.81497,
                SD1d = 26.41755,
                SD1a = 25.62968,
                SD2d = 59.36199,
                SD2a = 62.57526
              ),
              tolerance = 1e-5
            )
            options(warn = -1)
            expect_equal(suppressWarnings(hrvhra(
              c(
                992.5,
                756.205,
                992.5,
                756.205,
                992.5,
                756.205,
                992.5,
                756.205,
                992.5,
                756.205
              ),
              rep(0, 10)
            ))[c("SD2", "SD2d", "SD2a")],  c(SD2 = 0, SD2d = 0, SD2a = 0))

            expect_equal(
              suppressWarnings(hrvhra(rep(990, 10), rep(0, 10))),
              c(
                SDNN = 0,
                SD1 = 0,
                SD2 = 0,
                SD1I = 0,
                SDNNd = 0,
                SDNNa = 0,
                SD1d = 0,
                SD1a = 0,
                SD2d = 0,
                SD2a = 0
              )
            )
            # again, this is a well known dataset
            expect_equal(describerr(RR$flags),
                         c(
                           all = 1943,
                           N = 1943,
                           V = 0,
                           S = 0,
                           X = 0,
                           U = 0
                         ))
            expect_equal(describerr(c(1, 2, 3, 1, 2, 3, 1, 2, 3)),
                         c(
                           all = 9,
                           N = 0,
                           V = 3,
                           S = 3,
                           X = 3,
                           U = 0
                         ))
            expect_equal(describerr(c(0, 1, 2, 3, 4, 0, 1,  2, 3, 4)),
                         c(
                           all = 10,
                           N = 2,
                           V = 2,
                           S = 2,
                           X = 2,
                           U = 2
                         ))
})
