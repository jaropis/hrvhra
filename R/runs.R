#' Spliting the rr time series into disjoint subseries, breaking on annotations
#'
#' This function returns a list of disjoing segments - mini RR-time series which
#' only contain beats of sinus origin. The function breaks the original RR time
#' series on annotations which are not 0
#'
#' @param rr vector containing RR intervals time series
#' @param annotations vector containing annotations for the RR intervals
#' @return a list of vectors corresponding to disjoint RR time-series of sinus origin

split_on_annot <- function(rr, annotations) {
  bad_idx <- which(annotations != 0)
  if (length(bad_idx) == 0)
    return(list(rr))
  start <- 1
  rr_segments <- list()
  for (idx in bad_idx) {
    end <- idx
    if (start <= end - 1)
      rr_segments <- c(rr_segments, list(rr[start:end - 1]))
    start <- idx + 1
  }
  if (annotations[length(annotations)] == 0)
    rr_segments <- c(rr_segments, list(rr[start:length(rr)]))
  assert_that(length(rr_segments) > 0,
              msg = "no segments of continuous RR intervals in this dataset")
  return(rr_segments)
}

#' Counting runs in a segment of RR intervals of sinus origin
#'
#' This is a helper function used to count runs in a single segment, resulting
#' from splitting the whole RR time series on non-zero annotations
#'
#' @param rr vector containing RR intervals of only sinus origin
#' @param all_runs list that keeps all the runs in order
#' @param directions vector that keeps the designation - whether the run in the all_runs list is a deceleration or aceleration or noChange
#'
#' @return a two-element list, the first element is the list of runs, the second is the vector holding the directions of the runs

get_runs <- function(rr,
                     all_runs = list(),
                     directions = c()) {
  last <- ifelse(rr[1] == rr[2], 0, ifelse(rr[1] < rr[2], 1,-1))
  begin <- 1
  for (index in seq(length = (length(rr) - 2)) + 1) {
    current <-
      ifelse(rr[index] == rr[index + 1], 0, ifelse(rr[index] < rr[index + 1], 1, -1))
    if (current != last) {
      if (last == -1) {
        all_runs <- c(all_runs, list(rr[(begin):index]))
        directions <- c(directions, "Down")
      }
      if (last == 0) {
        all_runs <- c(all_runs, list(rr[(begin):index]))
        directions <- c(directions, "no_Change")
      }
      if (last == 1) {
        all_runs <- c(all_runs, list(rr[(begin):index]))
        directions <- c(directions, "Up")
      }
      begin <- index + 1
      last <- current
    }
  }

  # now check the last run - checked separately so that the previous loop is
  # simpler (a bit redundant, though) -

  if (last == -1) {
    all_runs <- c(all_runs, list(rr[(begin):length(rr)]))
    directions <- c(directions, "Down")
  }
  if (last == 0) {
    all_runs <- c(all_runs, list(rr[(begin):length(rr)]))
    directions <- c(directions, "no_Change")
  }
  if (last == 1) {
    all_runs <- c(all_runs, list(rr[(begin):length(rr)]))
    directions <- c(directions, "Up")
  }
  return(list(all_runs = all_runs, directions = directions))
}

#' Spliting the RR time series into monotoinc runs and assigning directions
#'
#' This function splits the whole recording into disjoint monotonic runs of RR
#' intervals of sinus origin and assigns a direction (Up, Down, no_Change)
#' to each of them
#'
#' @inheritParams split_on_annot

#' @return a two-element list, the first element is the list of runs, the second is the vector holding the directions of the runs

split_all_into_runs <- function(rr, annotations) {
  list_of_separate_segments <- split_on_annot(rr, annotations)
  # initialize the list keeping separate runs in consecutive segments
  separate_runs_and_directions <-
    list(all_runs = list(), directions = c())
  for (segment in list_of_separate_segments) {
    if (length(segment) > 1) {
      temp <- get_runs(segment)
      separate_runs_and_directions$all_runs <-
        c(separate_runs_and_directions$all_runs, temp$all_runs)
      separate_runs_and_directions$directions <-
        c(separate_runs_and_directions$directions,
          temp$directions)
    }
  }
  assert_that(length(separate_runs_and_directions[[1]]) > 0,
              msg = "no full runs in this dataset")

  return(separate_runs_and_directions)
}

#' Counting monotonic runs in an RR intervals time series.
#'
#' Counts the number of monotonic runs in an RR interval times series, for which
#' the annotations vector may or may not be available. If the annotations vector
#' is not available all RRs are assumed to be of sinus origin. This functon
#' counts all the runs of a specific type (decelerations, accelerations, no
#' change), up to the maximum values present in the RR intervals time series.
#' For example, if there is only one deceleration run of the type 1 2 3 4 5, the
#' result will be direction_up = c(0,0,0,0,1),
#' direction_down = NULL, no_change = NULL
#'
#' @inheritParams split_on_annot
#' @return a list with keys dicection_up, direction_down, no_change whose values are vectors containing corresponding runs types statistics
#' @import assertthat
#' @export
#'
#' @examples
#' countruns(RR$RR, RR$flags)
#' countruns(RR$RR)

countruns <- function(rr, annotations=c()) {
  # checking if RR vector the correct type and is long enough to proceed
  assert_that(is.vector(rr), is.numeric(rr), noNA(rr),
              msg = "the rr vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  assert_that(length(rr) > 2, msg = "RR vector too short to define a 'run'")

  if (length(annotations) == 0)
    annotations <- rr * 0

  # checking if annotations vector the coannotationsect type and is long enough to proceed
  assert_that(is.vector(annotations),
              is.numeric(annotations),
              noNA(annotations),
              msg = "the annotations vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  assert_that(length(annotations) == length(rr), msg = "annotations and RR vectors need to be of the same length")

  splitrr <- split_all_into_runs(rr, annotations)
  directions <- splitrr$directions
  up_runs <- splitrr$all_runs[directions == "Up"]
  down_runs <- splitrr$all_runs[directions == "Down"]
  no_change_runs <- splitrr$all_runs[directions == "no_Change"]
  up_runs_counts <- unlist(lapply(up_runs, length))
  down_runs_counts <- unlist(lapply(down_runs, length))
  no_change_runs_counts <- unlist(lapply(no_change_runs, length))

  ## getting maximum lengths of the respective runs
  max_up <- ifelse(length(up_runs_counts > 0), max(up_runs_counts), 0)
  max_down <-
    ifelse(length(down_runs_counts > 0), max(down_runs_counts), 0)
  max_no_change <-
    ifelse(length(no_change_runs_counts) > 0,
           max(no_change_runs_counts),
           0)

  ## now counting
  up_runs_accumulator <- c()
  down_runs_accumulator <- c()
  zero_change_runs_accumulator <- c()

  #directionup
  for (idx_up in seq(length = max_up)) {
    up_runs_accumulator <-
      c(up_runs_accumulator, sum(up_runs_counts == idx_up))
  }
  for (idx_down in seq(length = max_down)) {
    down_runs_accumulator <-
      c(down_runs_accumulator, sum(down_runs_counts == idx_down))
  }
  for (idx_no_change in seq(length = max_no_change)) {
    zero_change_runs_accumulator <-
      c(zero_change_runs_accumulator,
        sum(no_change_runs_counts == idx_no_change))
  }

  # now assigning meaningful names to the resulting vectors
  if (length(up_runs_accumulator) > 0)
    names(up_runs_accumulator) <-
      paste(rep("up", length(up_runs_accumulator)),
            seq_len(length(up_runs_accumulator)), sep = "")
  if (length(down_runs_accumulator) > 0)
    names(down_runs_accumulator) <-
      paste(rep("down", length(down_runs_accumulator)),
            seq_len(length(down_runs_accumulator)), sep = "")
  if (length(zero_change_runs_accumulator) > 0)
    names(zero_change_runs_accumulator) <-
      paste(rep("no_change", length(zero_change_runs_accumulator)),
            seq_len(length(zero_change_runs_accumulator)), sep = "")

  return(
    list(
      direction_up = up_runs_accumulator,
      direction_down = down_runs_accumulator,
      no_change = zero_change_runs_accumulator
    )
  )
}
