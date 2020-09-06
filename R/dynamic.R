library(slider)

#' Helper function reversing the row order of a data frame
#' @param df_elem the data frame to be reversed
#' @return reversed dataframe
reverse_df <- function(df_elem) {
  df_elem[dim(df_elem)[1]:1, ]
}

#' Function adding a time track as a time object, so that it is easier to manipulate
#' @param RR RR object
#' @param now when does the recording begin? default is "2020-09-05 12:11:00"
#' @param reverse whether the RR intervals time series should be reversed (necessary for jumping window)
#' @return data frame with time_track as the first vector
add_time_track <- function(RR, now = "2020-09-05 12:11:00", reverse = FALSE) {
  print(reverse)
  if (reverse) {
    RR <- reverse_df(RR)
  }
  beginning <- as.POSIXct(now)
  time_track <- beginning + cumsum(RR[["RR"]] / 1000) # this will give us a time track in seconds
  cbind(time_track, RR)
}

#' Function reversing time track when it is in a list of reversed RR intervals time series
reverse_time_track <- function(RR_time_track, cut_end = FALSE) {
  if (cut_end) {
    return(RR_time_track)
  }
  beginning <- RR_time_track[[1]][["time_track"]][[1]]
  lengths <- sapply(RR_time_track, nrow)
  N <- sum(lengths)
  # first we kill the full time track
  reversed_RR_dfs <- lapply(RR_time_track, function(elem) select(elem, RR, flags)) %>% # first we kill the full time track
    lapply(function(elem) { # now reverse the elements within the list
        reverse_df(elem)
      }
    ) %>%
    rev() # reverse the list
  temp_ttrack <- c(beginning)
  for(idx in seq_along(reversed_RR_dfs)) {
    # adding the correct time track to individual RR dfs - may come in handy to keep them
    temp_ttrack <- beginning + cumsum(reversed_RR_dfs[[idx]]$RR / 1000)
    reversed_RR_dfs[[idx]] <- cbind(temp_ttrack, reversed_RR_dfs[[idx]])
    names(reversed_RR_dfs[[idx]]) <- c("time_track", "RR", "flags")
    rownames(reversed_RR_dfs[[idx]]) <- 1:nrow(reversed_RR_dfs[[idx]])
    beginning <- temp_ttrack[length(temp_ttrack)]
  }
  reversed_RR_dfs
}

#' Function dividing the dataframe into SLIDING windows of `window` length
#' test with lapply(ala, function(elem) sum(elem$RR) / 60 / 1000)
#' @param RR RR data
#' @param window the length of the window in minutes
#' @export
time_based_slide <- function(RR, window = 5, now = "2020-09-05 12:11:00") {
  RR_time_track <- add_time_track(RR, now)
  slide_index(RR_time_track,
              RR_time_track$time_track,
              ~.x,
              .complete = TRUE,
              .before = minutes(window))
}

#' Function dividing the dataframe into SLIDING windows of `window` length
#' test with lapply(ala, function(elem) nrow(elem$RR))
#' @param RR RR data
#' @param window the length of the window in beats (samples)
#' @export
index_based_slide <- function(RR, window = 300) {
  slide(RR, ~.x, .before = window - 1, .complete= TRUE) %>%
    Filter(function(elem) !is.null(elem), .)
}
#' Function dividing the dataframe into JUMPING windows of `window` length based on time
#' test with lapply(ala, function(elem) sum(elem$RR) / 60 / 1000)
#' @param RR RR data
#' @param window the length of the window in minutes
#' @param cut_end if the window does not fit the entire recording perfectly, should I cut_end = TRUE or the beginning (FALSE) of the recording
#' @param now when does the recording begin? default is "2020-09-05 12:11:00"
#' @export
time_based_jump <- function(RR, window = 5, cut_end = FALSE, now = "2020-09-05 12:11:00") {
    RR_time_track <- add_time_track(RR, now, reverse = ifelse(cut_end, FALSE, TRUE))
    slide_period(RR_time_track,
                 RR_time_track$time_track,
                 "minute", .every = window,
                 ~.x,
                 .complete = TRUE) %>%
      reverse_time_track(cut_end)
}

#' Function dividing the dataframe into JUMPING windows of `window` length based on index
#' @param RR RR data
#' @param window the length of the window in beats
#' @param cut_end if the window does not fit the entire recording perfectly, should I cut_end = TRUE or the beginning (FALSE) of the recording
#' @param now when does the recording begin? default is "2020-09-05 12:11:00"
#' @export
index_based_jump <- function(RR, window = 300, cut_end = FALSE) {
  if (cut_end) {
    split <- slide(c(0, RR$RR), ~.x, .before = window - 1, .step = window ) # ATTENTION! prepending a 0 because of the way the slider works (the first window would be just the first element)
    split[[1]] <- NULL
  } else {
    split <- slide(RR$RR, ~.x, .after = window - 1, .step = window)
  }
  split %>% Filter(function(elem) !is.null(elem), .)
}
