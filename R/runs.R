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

countruns <- function(RR, anotacje) {
  # Helper: replicate Python's sign for vectors
  sign_vec <- function(x) {
    ifelse(x > 0, 1, ifelse(x < 0, -1, 0))
  }
  
  # 1) Compute differences and their sign
  diffRR <- diff(RR)
  signRR <- sign_vec(diffRR)
  
  # 2) Identify indices where we "break" runs (anotacje != 0)
  ind_wyrz1 <- which(anotacje != 0)
  ind_wyrz2 <- ind_wyrz1 - 1
  
  # Edge case: if the largest index is beyond the last diff, set it to length(diffRR) - 1
  if (length(ind_wyrz1) >= 1 && max(ind_wyrz1) >= length(signRR)) {
    bad_index <- which(ind_wyrz1 == max(ind_wyrz1))
    ind_wyrz1[bad_index] <- length(signRR) - 1
  }
  
  # Combine and set those indices to 16 (same logic as Python)
  ind_wyrz <- unique(c(ind_wyrz1, ind_wyrz2))
  # Make sure we only mark valid positions
  ind_wyrz <- ind_wyrz[ind_wyrz >= 1 & ind_wyrz <= length(signRR)]
  signRR[ind_wyrz] <- 16
  
  # 3) Count consecutive runs of up and down
  #    By default, we allocate up to 40 runs (as in Python),
  #    but we will later trim to the actual max run length.
  max_runs <- 40
  akumulator_up   <- rep(0, max_runs)
  akumulator_down <- rep(0, max_runs)
  
  # Edge case: If RR has length 1 or less, nothing to diff => return zeros
  if (length(signRR) == 0) {
    return(
      list(
        direction_up   = setNames(numeric(0), character(0)),
        direction_down = setNames(numeric(0), character(0)),
        no_change      = c(no_change1 = 0)
      )
    )
  }
  
  index_up   <- 0
  index_down <- 0
  flaga      <- signRR[1]
  
  # Iterate through signRR from the 2nd element to the end
  for (i in seq(2, length(signRR))) {
    znak <- signRR[i]
    
    # Continue run up
    if (flaga == 1 && znak == 1) {
      index_up <- index_up + 1
    }
    # Continue run down
    if (flaga == -1 && znak == -1) {
      index_down <- index_down + 1
    }
    # Up run ended
    if (flaga == 1 && znak != 1) {
      akumulator_up[index_up + 1] <- akumulator_up[index_up + 1] + 1
      index_up <- 0
    }
    # Down run ended
    if (flaga == -1 && znak != -1) {
      akumulator_down[index_down + 1] <- akumulator_down[index_down + 1] + 1
      index_down <- 0
    }
    
    flaga <- znak
  }
  
  # Edge-case check for the final run
  # (Same logic as in the Python code: we look at the very last 'znak')
  last_znak <- signRR[length(signRR)]
  if (flaga == 1 && last_znak == 1) {
    akumulator_up[index_up + 1] <- akumulator_up[index_up + 1] + 1
  }
  if (flaga == -1 && last_znak == -1) {
    akumulator_down[index_down + 1] <- akumulator_down[index_down + 1] + 1
  }
  
  # 4) Count "no change"
  #    In the Python code, this is simply the total number of diff == 0.
  not_changed <- sum(signRR == 0)
  
  # 5) Trim up/down arrays to the maximum run length actually present
  #    (including zeros for missing lengths up to that max)
  # Find the longest run for each
  max_up_len   <- if (any(akumulator_up != 0))   max(which(akumulator_up != 0))   else 0
  max_down_len <- if (any(akumulator_down != 0)) max(which(akumulator_down != 0)) else 0
  
  # Subset the vectors
  if (max_up_len > 0) {
    up_runs <- akumulator_up[seq_len(max_up_len)]
    names(up_runs) <- paste0("up", seq_len(max_up_len))
  } else {
    # No up-runs at all
    up_runs <- numeric(0)
  }
  
  if (max_down_len > 0) {
    down_runs <- akumulator_down[seq_len(max_down_len)]
    names(down_runs) <- paste0("down", seq_len(max_down_len))
  } else {
    # No down-runs at all
    down_runs <- numeric(0)
  }
  
  # "No change" is just a single count in the Python logic
  # Name it "no_change1" to fit your requested output style
  no_change_runs <- c(not_changed)
  names(no_change_runs) <- "no_change1"
  
  # 6) Return a list with the three vectors
  list(
    direction_up   = up_runs,
    direction_down = down_runs,
    no_change      = no_change_runs
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
