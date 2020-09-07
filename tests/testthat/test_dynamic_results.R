library(hrvhra)
context("windowing functions")
# setup
## for testing time_based_slide
window_rnd_time_slide <- sample(5:10, 1)
time_slide <- time_based_slide(RR, window = window_rnd_time_slide)
cum_suma_front <- cumsum(RR$RR) / 60 / 1000
N_5 <- which(cum_suma_front <= window_rnd_time_slide)
N_5 <- N_5[length(N_5)]

cum_suma_back <- cumsum(hrvhra::reverse_df(RR)[["RR"]]) / 60 / 1000
N_m_5 <- which(cum_suma_back <= window_rnd_time_slide)
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

## setup for testing index_based_slide
window_rnd_index_slide <- sample(300:400, 1)
index_slide <- index_based_slide(RR, window = window_rnd_index_slide)

test_that("the index_based_slide functions cuts at correct places", {
  expect_equal(
    sum(RR$RR[1:(window_rnd_index_slide)]) / 60 / 1000,
    sum(index_slide[[1]]$RR) / 60 / 1000  # the time in the first window is exactly equal to that calculated above
  )
  expect_equal(
    sum(RR$RR[(nrow(RR) - window_rnd_index_slide + 1):nrow(RR)]) / 60 / 1000,
    sum(index_slide[[length(index_slide)]]$RR) / 60 / 1000 # the time in the last window is exactly equal to that calculated above
  )
})

## setup for testing time_based_jump
window_rnd_time_jump <- sample(5:10, 1)
time_jump_cut_end_false <- time_based_jump(RR, window = window_rnd_time_jump, cut_end = FALSE)
suma_front <- sum(RR$RR[1:nrow(time_jump_cut_end_false[[1]])]) / 60 / 1000

time_jump_cut_end_true <- time_based_jump(RR, window = window_rnd_time_jump, cut_end = TRUE)
suma_back <- sum((RR$RR[(nrow(RR)-nrow(time_jump_cut_end_true[[length(time_jump_cut_end_true)]]) + 1):nrow(RR)])) / 60 / 1000

test_that("the time_based_jump functions cuts at correct places", {
  expect_equal(
    suma_front,
    sum(time_jump_cut_end_false[[1]]$RR) / 60 / 1000  # the time in the first window is exactly equal to that calculated above
  )
  expect_equal(
    suma_back,
    sum(time_jump_cut_end_true[[length(time_jump_cut_end_true)]]$RR) / 60 / 1000 # the time in the last window is exactly equal to that calculated above
  )
})

## setup for testing index_based_jump - cut_end_false
window_rnd_index_jump <- sample(300:500, 1)
index_jump_cut_end_false <- index_based_jump(RR, window = window_rnd_index_jump, cut_end = FALSE)
N_300_full <- floor(nrow(RR) / window_rnd_index_jump) * window_rnd_index_jump
last_window_first_elem_cut_end_false <- RR$RR[N_300_full + 1]
last_but_one_window_last_elem_cut_end_false <- RR$RR[N_300_full]
first_window_last_elem_cut_end_false <- RR$RR[[window_rnd_index_jump]]
second_window_first_elem_cut_end_false <- RR$RR[[window_rnd_index_jump + 1]]

test_that("the index_based_jump functions cuts at correct places cut_out_end false", {
  expect_equal(
    last_window_first_elem_cut_end_false,
    index_jump_cut_end_false[[length(index_jump_cut_end_false)]]$RR[[1]]# the time in the first window is exactly equal to that calculated above
  )
  expect_equal(
    last_but_one_window_last_elem_cut_end_false,
    index_jump_cut_end_false[[length(index_jump_cut_end_false) - 1]]$RR[[window_rnd_index_jump]]# the time in the last window is exactly equal to that calculated above
  )
  expect_equal(
    first_window_last_elem_cut_end_false,
    index_jump_cut_end_false[[1]]$RR[[window_rnd_index_jump]]
  )
  expect_equal(
    second_window_first_elem_cut_end_false,
    index_jump_cut_end_false[[2]]$RR[[1]]
  )
})

## setup for testing index_based_jump - cut_end_true
index_jump_cut_end_true <- index_based_jump(RR, window = window_rnd_index_jump, cut_end = TRUE)
N_300_full <- floor(nrow(RR) / window_rnd_index_jump) * window_rnd_index_jump
N_end_first_window <- nrow(RR) - N_300_full
last_window_first_elem_cut_end_true <- RR$RR[nrow(RR) - window_rnd_index_jump + 1]
last_but_one_window_last_elem_cut_end_true <- RR$RR[N_300_full - window_rnd_index_jump + N_end_first_window]
first_window_last_elem_cut_end_true <- RR$RR[[N_end_first_window]]
second_window_first_elem_cut_end_true <- RR$RR[[N_end_first_window + 1]]

test_that("the index_based_jump functions cuts at correct places cut_out_end true", {
  expect_equal(
    last_window_first_elem_cut_end_true,
    index_jump_cut_end_true[[length(index_jump_cut_end_true)]]$RR[[1]]# the time in the first window is exactly equal to that calculated above
  )
  expect_equal(
    last_but_one_window_last_elem_cut_end_true,
    index_jump_cut_end_true[[length(index_jump_cut_end_true) - 1]]$RR[[window_rnd_index_jump]]# the time in the last window is exactly equal to that calculated above
  )
  expect_equal(
    first_window_last_elem_cut_end_true,
    index_jump_cut_end_true[[1]]$RR[[nrow(index_jump_cut_end_true[[1]])]]
  )
  expect_equal(
    second_window_first_elem_cut_end_true,
    index_jump_cut_end_true[[2]]$RR[[1]]
  )
})
