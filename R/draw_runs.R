#' helper function to check if a number is even
#' @param x number to be checked
#' @return TRUE/FALSE
iseven <- function(x) x %% 2 == 0

#' function generating the positions of labels to annotate a runs plot
#' @param labels vector with a list of runs names, e.g. c("DR3", "AR4", "N1", "AR3", "DR2")
#' @param rr rr intervals time series
#' @param d the distance from segment/point
#' @param x_offset offset on the x axis
#' @param y_offset offset on the y axis
#' @return list of labels positions in points
#' @export
calculate_labels_positions <- function(rr, labels, d = 3, x_offset = 0.40, y_offset = 50) {
  position <- 1
  positions_list <- list()
  for (run in labels) {
    run_length <- as.numeric(substr(run, nchar(run), nchar(run)))
    direction <- substr(run, 1, 1)
    # now finding the point which is of d distance from the segment (i1, x1), (i2, x2)
    if (!iseven(run_length)) {
      i1 <- position + (run_length %/% 2)
      i2 <- position + (run_length %/% 2) + 1
      x1 <- rr[i1]
      x2 <- rr[i2]

      if (direction == 'D') {
        d1 <- (i1 + 1/2) - 0.40
        d2 <- (x1 + x2) / 2 + 50
      } else {
        d1 <- (i1 + 1/2) - 0.40
        d2 <- (x1 + x2) / 2 - 50
      }
      if (direction == 'N') {
        d1 <- (i1+i2) / 2
        d2 <- x1 + 50
      }
      positions_list <- c(positions_list, list(c(d1[1], d2[1])))
    }
    if (iseven(run_length)) {
      i <- position + run_length %/% 2
      x <- rr[i]
      if (direction == 'D') {
        d1 <- i - x_offset
        d2 <- x + y_offset
      } else {
        d1 <- i + x_offset
        d2 <- x + y_offset
      }
      if (direction == "N") {
        d1 <- i
        d2 <- rr[i] + 50
      }
      positions_list <- c(positions_list, list(c(d1, d2)))
    }
    position <- position + run_length
  }
  return(positions_list)
}

#' function generating a SIMULATED rr intervals time series
#' @param runs_vec runs_vec vector with a list of runs names, e.g. c("DR3", "AR4", "N1", "AR3", "DR2")
#' @param starting_rr starting rr intervals time series value
#' @return vector
simulate_rr_intervals <- function(runs_vec, starting_rr = c(894)) {
  rr = starting_rr
  for (run in runs_vec) {
    run_length <- as.numeric(substr(run, nchar(run), nchar(run)))
    direction <- substr(run, 1, 1)
    direction <- ifelse(direction == "D", 1, ifelse(direction == "A", -1, 0))
    fatigue <- 0
    for (idx in 1:run_length){
      rr <- c(rr, rr[length(rr)] + direction * (runif(1, min=100, max = 200)) - sign(direction) * fatigue)
      fatigue <- fatigue + 30
    }
  }
  rr
}

# TUTU
#' function drawing a SIMULATED RR runs (for presentation purposes)
#' @param runs_vec vector with a list of runs names, e.g. c("DR3", "AR4", "N1", "AR3", "DR2")
#' @return list of: simulated RR intervals, label positions, labels
#' @export
#' TUTU: ponizej: konflikt - ponizej wywolywana jest ta sama funkcja!!! WTF! jak to wgle dziala
calculate_label_positions <- function(runs_vec) {

  if (is.char(runs_vec[[1]])) {
    # here we simulate the RR intervals
    # now generating label positions
    previous <- 1
    label_positions <- calculate_label_positions(runs_vec, rr_intervals, d=1)
    list(rr_vec = rr_vec, label_positions = label_positions, labels = labels)
  }
}

#' function generating runs plot
#' @param rr_intervals vector of rr intervals, if null, rr intervals will be simulated on the basis of the provided labels
#' @param labels vector of labels like c("DR3", "AR4", "N1", "AR3", "DR2") - if rr_intervals are provided, labels will be ignored
#' @return does not return anything, run for side effects (plotting / exporting plot)
#' @export
draw_runs <- function(rr_intervals = NULL,
                      annotations = NULL,
                      labels = c("DR4", "AR4", "N1", "AR3", "DR2")) {
  if (is.null(rr_intervals) & is.null(labels)) {
    stop("at least one of rr_intervals or labels must be provided")
  }

  if (is.null(rr_intervals)) {
    rr_intervals = simulate_rr_intervals(labels)
  } else {
    if(is.null(annotations)) {
      annotations <- rr_intervals * 0
    }
    labels = get_runs_sequence(rr_intervals, annotations)
  }

  label_positions <- calculate_labels_positions(rr_intervals, labels)

  plot(rr_intervals, ylim=c(min(rr_intervals) - 20, max(rr_intervals) + 20), xlab="beat number", ylab="RR interval [ms]")

  for (beat in 2:length(rr_intervals)) {
    if (rr_intervals[beat]>rr_intervals[beat-1]) {
      segments(beat-1, rr_intervals[beat-1], beat, rr_intervals[beat], lty=1, lwd=3, col="black")
    }
  }

  for (beat in 2:length(rr_intervals)){
    if (rr_intervals[beat]<rr_intervals[beat-1]){
      segments(beat-1, rr_intervals[beat-1], beat, rr_intervals[beat], lty=3, lwd=3, col="black")
    }
  }

  points(rr_intervals, pch=21,col="black", bg="white")

  for (beat in 2:(length(rr_intervals)-1)) {
    if ((rr_intervals[beat]<rr_intervals[beat-1] | rr_intervals[beat]==rr_intervals[beat-1]) & (rr_intervals[beat]<rr_intervals[beat+1])){
      points(beat, rr_intervals[beat], pch=21, col="black", bg="black", cex=1.4)
    }
  }

  for (beat in 2:(length(rr_intervals)-1)) {
    if ((rr_intervals[beat]>rr_intervals[beat-1] | rr_intervals[beat]==rr_intervals[beat-1]) & (rr_intervals[beat]>rr_intervals[beat+1])) {
      points(beat, rr_intervals[beat], pch=21, col="black", bg="gray50", cex=1.4)
    }
  }

  if (rr_intervals[1]>rr_intervals[2]) {
    points(1, rr_intervals[1], pch=21, col="black", bg="gray50", cex=1.4)} else {points(1, rr_intervals[1], pch=21, col="black", bg="black", cex=1.4)}

  if (substr(labels[[length(labels)]], 1, 1) == "D"){
    rr_intervalsLast <- rr_intervals[length(rr_intervals)] - 150
    lty <- 3
  } else {
    rr_intervalsLast <- rr_intervals[length(rr_intervals)]+150
    lty <- 1
  }

  for (label_idx in 1:length(label_positions)) {
    text(label_positions[[label_idx]][1],label_positions[[label_idx]][2], labels[label_idx])
  }

  if (substr(labels[[length(labels)]],1,1)=="D") {
    points(length(rr_intervals), rr_intervals[length(rr_intervals)], pch=21, col="black", bg="gray50", cex=1.4)
  } else {
    points(length(rr_intervals), rr_intervals[length(rr_intervals)], pch=21, col="black", bg="black", cex=1.4)}
}


