#' Spliting the rr time series into disjoint subseries, breaking on annotations
#'
#' This function returns a list of disjoing segments - mini RR-time series which
#' only contain beats of sinus origin. The function breaks the original RR time
#' series on annotations which are not 0
#'
#' @param rr vector containing RR intervals time series
#' @param annotations vector containing annotations for the RR intervals
#' @param throwError whether error should return NULL or throw an error
#' @return a list of vectors corresponding to disjoint RR time-series of sinus origin
split_on_annot <- function(rr, annotations, throwError = FALSE) {
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
  # what happens for bad RR ts
  if (length(rr_segments) == 0) {
    if (!throwError) {
      return(NULL)
    } else {
      stop("no segments of continuous RR intervals in this dataset")
    } 
  }
  return(rr_segments)
}

#' Counting runs in a segment of RR intervals of sinus origin
#'
#' This is a helper function used to count runs in a single segment, resulting
#' from splitting the whole RR time series on non-zero annotations
#'
#' @param rr vector containing RR intervals of only sinus origin of length at least 2
#' @param all_runs list that keeps all the runs in order
#' @param directions vector that keeps the designation - whether the run in the all_runs list is a deceleration or aceleration or noChange
#'
#' @return a two-element list, the first element is the list of runs, the second is the vector holding the directions of the runs
get_runs <- function(rr,
                     all_runs = list(),
                     directions = c()) {
  last <- ifelse(rr[1] == rr[2], 0, ifelse(rr[1] < rr[2], 1, -1))
  begin <- 1
  for (index in seq(2, length(rr))) { # length rr must be >= 2, so no problem here
    current <-
      ifelse(rr[index] == rr[index + 1], 0, ifelse(rr[index] < rr[index + 1], 1, -1))

    if (index == length(rr))
      current <- 2 # this is a signal that the loop has reached the final RR which MUST be a reference beat, so it MUST be different than the last beat, hence 2

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
  all_runs[[1]] <- all_runs[[1]][-c(1)] # knocking off the first element of the segment, because it MUST be reference
  return(list(all_runs = all_runs, directions = directions))
}

#' Splitting the RR time series into monotonic runs and assigning directions
#'
#' This function splits the whole recording into disjoint monotonic runs of RR
#' intervals of sinus origin and assigns a direction (Up, Down, no_Change)
#' to each of them
#'
#' @inheritParams split_on_annot
#' @return a two-element list, the first element is a list of runs, the second is a vector holding the directions of the runs

split_all_into_runs <- function(rr, annotations, throwError = FALSE) {
  list_of_separate_segments <- split_on_annot(rr, annotations, throwError)
  if (is.null(list_of_separate_segments)) {
    return(NULL)
  }
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

#' Function to get runs sequence in an RR intervals time series, e.g. c("DR3", "AR4", "N1", "AR3", "DR2")
#' @param rr vector containing RR intervals time series
#' @param annotations vector containing annotations for the RR intervals
#' @param throwError whether error should return NULL or throw an error
#' @return vector of runs sequence, like c("DR3", "AR4", "N1", "AR3", "DR2")
#' @export
get_runs_sequence <- function(rr, annotations, throwError = FALSE) {
  runs <- split_all_into_runs(rr, annotations, throwError)
  paste0(str_replace(runs[[2]], "Up", "AR") %>%
           str_replace("Down", "DR") %>%
           str_replace("no_Change", "N"),
         sapply(runs[[1]], length))
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
#'
#' @references J Piskorski, P Guzik, The structure of heart rate asymmetry: deceleration and acceleration runs, Physiological measurement 32 (8), (2011)

countruns <- function(rr, annotations=c(), throwError = FALSE) {
  # checking if RR vector the correct type and is long enough to proceed
  assert_that(is.vector(rr), is.numeric(rr), noNA(rr),
              msg = "the rr vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  "RR vector too short to define a 'run'"
  if (length(rr) <= 2) {
    if (!throwError) {
      return(list(
        direction_up = c(up1 = -1),
        direction_down = c(down1 = -1),
        no_change = c(N1 = -1)
      ))
    } else {
      stop("RR vector too short to define a 'run'")
    } 
  }

  if (length(annotations) == 0)
    annotations <- rr * 0

  # checking if annotations vector the coannotationsect type and is long enough to proceed
  assert_that(is.vector(annotations),
              is.numeric(annotations),
              noNA(annotations),
              msg = "the annotations vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  assert_that(length(annotations) == length(rr), msg = "annotations and RR vectors need to be of the same length")

  splitrr <- split_all_into_runs(rr, annotations, throwError)
  
  # this below happens if there are only non-sinus beats
  if (is.null(splitrr)) {
    return(list(
      direction_up = c(up1 = -1),
      direction_down = c(down1 = -1),
      no_change = c(N1 = -1)
    ))
  }
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

#' Function binding the results of runs calculation for different datasets (files)
#' into a single table
#'
#' Sometimes many files are calculated and many results objects are collected (one for each dataset / file)
#' in this situation it may be helpful to present them as a table. This function collects all the results
#' as rows of a table, padding with zeros when necessary
#' @param results list of results from the `countruns` function
#' @param rownames rownames for the table
#' @return data.table
#' @export
bind_runs_as_table <- function(results, rownames = NULL) {
  len_up <- 0
  len_down <- 0
  len_no_change <- 0
  ## in this loop I am getting the maximum run length of a specific type for the analyzed group of recordings
  for (result in results){
    if (length(result$direction_up) > len_up) len_up <- length(result$direction_up)
    if (length(result$direction_down) > len_down) len_down <- length(result$direction_down)
    if (length(result$no_change) > len_no_change) len_no_change <- length(result$no_change)
  }

  final_results <- data.frame()
  for (result in results) {
    final_results <- rbind(final_results,
                           # theoretically, we could have only  accelerations, only decelerations or no no-change
                           c('if'(is.null(result$direction_up),
                                  rep(NA, len_up),
                                  result$direction_up[1:len_up]),
                             'if'(is.null(result$direction_down),
                                  rep(NA, len_down),
                                  result$direction_down[1:len_down]),
                             'if'(is.null(result$no_change),
                                  rep(NA, len_no_change),
                                  result$no_change[1:len_no_change]),
                             'if'(is.null(result$direction_up),
                                  0,
                                  get_longest_run(result$direction_up, "up")),
                             'if'(is.null(result$direction_down),
                                  0,
                                  get_longest_run(result$direction_down, "down")),
                             'if'(is.null(result$no_change),
                                  0,
                                  get_longest_run(result$no_change, "no_change"))
                           )
    )
  }
  
  # adding passed rownames or numbers
  if(!is.null(rownames)) {
    final_results <- cbind(rownames, final_results)
  } else {
    final_results <- cbind(seq(nrow(final_results)), final_results)
  }

  computed_names <- c("file",
                      paste("DR", 1:len_up, sep = ""),
                      paste("AR", 1:len_down, sep = ""))

  if(len_no_change > 0) {
    computed_names <- c(computed_names, paste("N", seq_len(len_no_change), sep = ""))
  }
  computed_names <- c(computed_names, "DR_MAX", "AR_MAX", "N_MAX")
  colnames(final_results) <- computed_names
  # and finally replacing NA's by zeros, so that it is easier to process
  # (in fact, count 0 is obviously no NA, as 0 is a valid number of runs)
  final_results[is.na(final_results)] <- as.integer(0)
  
  # if a row has a -1 (indicative of a failed calculation) then all values are replaced with NAs
  for (idx in seq(nrow(final_results))) {
    if (-1 %in% final_results[idx, ]) {
      final_results[idx, 2:ncol(final_results)] <- NA # saving filename
    }
  }
  final_results
}

#' Function extracting the longest run from list
#' @param runs_result list with runs results
#' @param type type of run (up or down)
#' @return int
#' 
get_longest_run <- function(runs_results, type) {
  number <- names(runs_results)[length(runs_results)] %>% 
    strsplit(type)
  as.numeric(number[[1]][2])
}

#' Function extracting runs entropy
#' @param nb_decelerations vector with the numbers of respective decelerations
#' @param nb_decelerations vector with the numbers of respective acceleratons
#' @param nb_noChanges vector with the numbers of respective neutral_runs
#' @export
#' @return vector of three folats
#'
entropies <- function(nb_decelerations, nb_accelerations, nb_noChanges) {
  individual_entropy <- function(counts, n){
    full <- 0
    partial <- 0
    for (i in seq_along(counts)){
      if (counts[i] > 0){
        partial <- - i * counts[i]/n * log(i * counts[i]/n)
      }
      full <- full + partial
    }
    return(full)
  }
  individual_entropy_2 <- function(counts, n){
    full <- 0
    partial <- 0
    for (i in seq_along(counts)){
      if (counts[i] > 0){
        partial <- - counts[i]/n * log(counts[i]/n)
      }
      full <- full + partial
    }
    return(full)
  }
  n <- sum(c(nb_decelerations * 1:length(nb_decelerations),
             nb_accelerations * 1:length(nb_accelerations), 
             nb_noChanges * 1:length(nb_noChanges)))
  HDR <- individual_entropy(nb_decelerations, n)
  HAR <- individual_entropy(nb_accelerations, n)
  HNO <- individual_entropy(nb_noChanges, n)
  
  n_2 <- sum(c(nb_decelerations,
               nb_accelerations, 
               nb_noChanges))
  HDR2 <- individual_entropy_2(nb_decelerations, n_2)
  HAR2 <- individual_entropy_2(nb_accelerations, n_2)
  HNO2 <- individual_entropy_2(nb_noChanges, n_2)
  result <- c(HDR, HAR, HNO, HDR2, HAR2, HNO2)
  names(result) <- c("HDR", "HAR", "HNO", "HDR2", "HAR2", "HNO2")
  return(result)
}
