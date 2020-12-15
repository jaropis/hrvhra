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
  if (reverse) {
    RR <- reverse_df(RR)
  }
  RR <- rbind(c(0, 0), RR) # we need a point which really corresponds to 0 - it will be left out by slider (remember that RR are already INTERVALS)
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
    lapply(function(elem) { # and reverse the elements within the list
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
#' @param time_unit the unit of time in which the window is expressed - can be either minute or second
#' @export
time_based_slide <- function(RR, window = 5, now = "2020-09-05 12:11:00", time_unit = "minute") {
  stopifnot(time_unit %in% c("second", "minute"))
  RR_time_track <- add_time_track(RR, now)
  slide_index(RR_time_track,
              RR_time_track$time_track,
              ~.x,
              .complete = TRUE,
              .before = 'if'(time_unit == "minute", minutes(window), seconds(window))) %>%
    Filter(function(elem) !is.null(elem), .)
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
#' @param tolerance what departure from the window length do we tolerate
#' @param time_unit the unit of time in which the window is expressed - can be either minute or second
#' @export
time_based_jump <- function(RR, window = 5, cut_end = FALSE, now = "2020-09-05 12:11:00", tolerance = 0.1, time_unit = "minute") {
    stopifnot(time_unit %in% c("second", "minute"))
    tempate_length <- 'if'(time_unit == "minute", window * 60 * 1000, window)
    RR_time_track <- add_time_track(RR, now, reverse = ifelse(cut_end, FALSE, TRUE))
    # TODO check what cut end means, because it is all messed up here
    resulting_windows <- slide_period(RR_time_track,
                                      RR_time_track$time_track,
                                      time_unit, .every = window,
                                      ~.x,
                                      .complete = TRUE,
                                      .origin = RR_time_track$time_track[[1]]) %>%
      reverse_time_track(cut_end)
    if(length(resulting_windows) == 1) {
      # early return if there is only one window
      return(remove_zeros(resulting_windows))
    }
    hanging_window <- 'if' (!cut_end, resulting_windows[[1]], resulting_windows[[length(resulting_windows)]])
    resulting_windows <- if (abs(sum(hanging_window$RR) - (template_length))/(template_length) >= tolerance) { # if the hanging window is not within 2% of window length
      'if' (cut_end, resulting_windows[1:(length(resulting_windows) - 1)], 
            resulting_windows[2:length(resulting_windows)])
    } else {
      resulting_windows
    }
    
    # now removing 0-length RR introduced by using the slider package
    remove_zeros(resulting_windows)
}

#' Helper function removing zeros resulting from using the slider package
#' @param resulting_windows  list of windows
remove_zeros <- function(resulting_windows) {
  resulting_windows %>% lapply(function(elem) {
    where_zero <- which(elem$RR == 0 & elem$flags ==0)
    if(length(where_zero) > 0) {
      elem <- elem[-c(where_zero), ]  
    }
    elem
  })
}
#' Function dividing the data.frame into JUMPING windows of `window` length based on index
#' @param RR RR data
#' @param window the length of the window in beats
#' @param cut_end if the window does not fit the entire recording perfectly, should I cut_end = TRUE or the beginning (FALSE) of the recording
#' @param tolerance what departure from the window length do we tolerate
#' @export
index_based_jump <- function(RR, window = 300, cut_end = FALSE, tolerance = 0.1) {
  if (!cut_end) {
    N <- length(RR$RR)
    window_multiples <- floor(N / window)
    if (window_multiples < 1) {
      stop("The lenght of the time series is too short for the selected window")
    }
    full_multiples <- window_multiples * window
    remainder <- N - full_multiples
    split <- c(list(RR[1:remainder, ]),
               slide(RR[(remainder + 1):N, ], ~.x, .after = window - 1, .step = window ))
  } else {
    split <- slide(RR, ~.x, .after = window - 1, .step = window)
  }
  split %<>% Filter(function(elem) !is.null(elem), .)
  hanging_window <- 'if' (!cut_end, split[[1]], split[[length(split)]])
  if (abs(nrow(hanging_window) - (window))/(window) >= tolerance) { # if the hanging window is not within 2% of window length
    'if' (cut_end, split[1:(length(split) - 1)], 
          split[2:length(split)])
  } else {
    split
  } 
}
