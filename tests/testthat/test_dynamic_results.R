library(hrvhra)
context("windowing functions")
# setup
## for testing time_based_slide
window_rnd <- sample(5:10, 1)
time_slide <- time_based_slide(RR, window = window_rnd)
cum_suma_front <- cumsum(RR$RR) / 60 / 1000
N_5 <- which(cum_suma_front <= window_rnd)
N_5 <- N_5[length(N_5)]

cum_suma_back <- cumsum(hrvhra::reverse_df(RR)[["RR"]]) / 60 / 1000
N_m_5 <- which(cum_suma_back <= window_rnd)
N_m_5 <- N_m_5[length(N_m_5)]

test_that("the time_based_slide functions cuts at correct places", {
          expect_equal(
            sum(RR$RR[1:(N_5 +  1)]) / 60 / 1000,
            sum(time_slide[[1]]$RR) / 60 / 1000  # the time in the first window is exactly equal to that calculated above
          )
          expect_equal(
            sum(RR$RR[(nrow(RR) - N_m_5):nrow(RR)]) / 60 / 1000,
            sum(time_slide[[length(time_slide)]]$RR) / 60 / 1000 # the time in the last window is exactly equal to that calculated above
          )
})


test_that("the index_based_slide functions cuts at correct places", {
  expect_equal(
    sum(RR$RR[1:(N_5 +  1)]) / 60 / 1000,
    sum(time_slide[[1]]$RR) / 60 / 1000  # the time in the first window is exactly equal to that calculated above
  )
  expect_equal(
    sum(RR$RR[(nrow(RR) - N_m_5):nrow(RR)]) / 60 / 1000,
    sum(time_slide[[length(time_slide)]]$RR) / 60 / 1000 # the time in the last window is exactly equal to that calculated above
  )
})
