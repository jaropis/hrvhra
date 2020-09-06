#' Function adding a time track as a time object, so that it is easier to manipulate
#' @param RR RR object
#' @param now when does the recording begin? default is "2020-09-05 12:11:00"
#' @return data frame with time_track as the first vector
#' @export
add_time_track <- function(RR, now = "2020-09-05 12:11:00") {
  beginning <- as.POSIXct(now)
  time_track <- beginning + cumsum(RR[["RR"]] / 1000) # this will give us a time track in seconds
  cbind(time_track, RR)
}
