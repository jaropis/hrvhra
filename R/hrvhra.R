#' Preparing data for Poincare plot descriptors calculations
#'
#' This function prepares data for Poincare plot calculations,
#' namely it prepares the auxiliary rr_i and rr_ii vectors and
#' filters them appropriately, so that no non-physiological pairs
#' occur in the Poincare plot
#'
#' @param rr vector containing RR intervals time series
#' @param annotations vector containing annotations for the RR intervals
#' @return a n x 2 matrix containing the filtered Poincare plot data
#' @import assertthat
#' @export
preparepp <- function(rr, annotations = c()) {
  # checking if RR vector the correct type and is long enough to proceed
  assert_that(is.vector(rr), is.numeric(rr), noNA(rr),
              msg = "the rr vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  assert_that(length(rr) > 3, msg = "RR vector too short to form a Poincare plot")

  # checking if annotations vector the coannotationsect type and is long enough to proceed
  assert_that(is.vector(annotations),
              is.numeric(annotations),
              noNA(annotations),
              msg = "the annotations vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  assert_that(length(annotations) == length(rr), msg = "annotations and RR vectors need to be of the same length")

  if (length(annotations) == 0)
    # assume all sinus beats if no annotations
    annotations = 0 * rr

  rr_i <- rr[1:(length(rr) - 1)]
  rr_ii <- rr[2:length(rr)]

  annotations_i <- annotations[1:(length(annotations) - 1)]
  annotations_ii <- annotations[2:length(annotations)]

  remove_i <- which(annotations_i != 0)
  remove_ii <- which(annotations_ii != 0)
  if (length(c(remove_i, remove_ii)) != 0) {
    rr_i <- rr_i[-c(remove_i, remove_ii)]
    rr_ii <- rr_ii[-c(remove_i, remove_ii)]
  }

  # check if there are enough beats of sinus origin to proceed
  assert_that(length(rr_i) > 2, msg = "too many non-sinus beats to proceed")

  return(cbind(rr_i, rr_ii))
}

#' Wrapper for preparepp - accepts dataframes (with the intention to make this a constructor)
#' @param rr_df dataframe with RR intervals (first column) and annotations (second column)
#' @return poincare plot object
#' @export
pp <- function(rr_df) {
  assert_that((is.data.frame(rr_df) && ncol(rr_df) %in% c(1, 2)) || (is.vector(rr_df) && is.numeric(rr_df)),
              msg = "rr_df must be a dataframe with the first column containing RR intervals and the optional second column with annotations, or an RR vector")
  {
    if (is.vector(rr_df)) {
      preparepp(rr_df, rr_df  * 0)
    } else if (ncol(rr_df) == 2) {
      preparepp(rr_df[[1]], rr_df[[2]])
    } else {
      preparepp(rr_df[[1]], rr_df[[1]] * 0)
    }
  } %>% as.data.frame()
}

#' Drawing the Poincare plot
#'
#' This function draws the Poincare plot prepared by the \code{preparePP} function
#'
#' @param pp poincare plot object
#' @param vname variable name - this will be used for construction \code{xlab} and \code{ylab} for the Poincare plot
#' @param mode plot mode - either "base" or "plotly"
#' @param \\dots Additional arguments passed on to \code{plot}
#' @export
#'
#' @examples
#' pp(RR) %>% drawpp()

drawpp <- function(pp, vname = "RR", mode = "base", ...) {
  if (mode == "base") {
    draw_pp_base(pp, vname = vname, ...)
  } else {
    draw_pp_plotly(pp, vname = vname, ...)
  }
}

#' Drawing the Poincare plot using base
#'
#' This function draws the Poincare plot prepared by the \code{preparePP} function
#'
#' @param pp poincare plot object
#' @param vname variable name - this will be used for construction \code{xlab} and \code{ylab} for the Poincare plot
#' @param \\dots Additional arguments passed on to \code{plot}
#' @importFrom graphics abline
#'
draw_pp_base <- function(pp, vname = "RR", ...) {
  assert_that(is.character(vname), msg = "vname needs to be a string")
  additional = list(...)
  # default appearance of the Poincare plot
  list_of_parameters <-
    list(
      pp[, 2] ~ pp[, 1],
      xlab = parse(text = paste(vname, "[i]")),
      ylab = parse(text = paste(vname, "[i+1]")),
      pch = 21,
      col = "black",
      bg = "orange"
    )

  # append additional parameters - any error here will be dealt with plot
  for (parameter in names(additional)) {
    if (parameter %in% names(list_of_parameters))
      list_of_parameters[parameter] <- additional[parameter]
    else
      list_of_parameters <-
        c(list_of_parameters, additional[parameter])
  }

  # and plot
  print(list_of_parameters)
  do.call(what = "plot", list_of_parameters, quote = TRUE)
  abline(0, 1, lty = 2, lwd = 2)
}

#' Drawing the Poincare plot using base
#'
#' This function draws the Poincare plot prepared by the \code{preparePP} function
#'
#' @param PP poincare plot object
#' @param vname variable name - this will be used for construction \code{xlab} and \code{ylab} for the Poincare plot
#' @param \\dots Additional arguments passed on to \code{plot}
#' @importFrom graphics abline
#' @import plotly
#'
draw_pp_plotly <- function(PP, vname = "RR", ...) {
  plot_options <- list(...)
  x <- list(
    title = paste0(vname, "<sub>i</sub>")
  )
  y <- list(
    title = paste0(vname, "<sub>i+1</sub>")
  )
  n_lines <- nrow(PP)
  scatter_tooltips <- sapply(seq(length = nrow(PP)),
                             function(idx) {
                               paste0("(", PP[["rr_i"]][idx], ", ", PP[["rr_ii"]][idx], ")<br>",
                                      'if'(PP[["rr_i"]][idx] >= PP[["rr_ii"]][idx],
                                           'if'(PP[["rr_i"]][idx] == PP[["rr_ii"]][idx],
                                                "neutral", "acceleration")
                                           , "deceleration"))
                             })

  PP %>%
    plot_ly(x = ~rr_i, y = ~rr_ii) %>%
    add_trace(
      type = "scatter",
      mode = "markers",
      x = ~rr_i,
      y = ~rr_ii,
      text = scatter_tooltips,
      hoverinfo = "text",
      size = I('if'(is.null(plot_options[["size"]]), 18, plot_options[["size"]])),
      alpha = I('if'(is.null(plot_options[["alpha"]]), 0.2, plot_options[["alpha"]])),
      color = I('if'(is.null(plot_options[["col"]]), "orange", plot_options[["col"]]))
    ) %>%
    add_trace(
      type = "scatter",
      mode = "lines",
      text = rep("line of identity", n_lines),
      x = ~rr_i,
      y = ~rr_i,
      color = I("black"),
      hoverinfo = "text") %>%
    hide_legend() %>%
    layout(xaxis = x, yaxis = y)
}
#' HRV descriptors
#'
#' This function returns the basic HRV descriptors related to the Poincare plot.
#'
#' @inheritParams preparepp
#' @return an 1 x 10 vector containing SDNN, SD1, SD2, SD1I, SDNNd, SDNNa, SD1d, SD1a, SD2d, SD2a  descriptors
#' @importFrom stats var
#' @export
#'
#' @examples
#' hrvhra(RR$RR, RR$flags)
#'
#' @references J Piskorski, P Guzik, Geometry of the Poincare plot of RR intervals and its asymmetry in healthy adults, Physiological measurement 28 (3), 287 (2007)

hrvhra <- function(rr, annotations) {
  pp <- preparepp(rr, annotations)
  rr_i <- pp[, "rr_i"]
  rr_ii <- pp[, "rr_ii"]

  # start with classic HRV parameters

  n <- length(rr_i)
  correct <- (n - 1) / n # correction to use biased variance

  sd1 <- sqrt(var((rr_i - rr_ii) / sqrt(2)) * correct)
  sd2 <- sqrt(var((rr_i + rr_ii) / sqrt(2)) * correct)
  sdnn <- sqrt(1 / 2 * (sd2 ^ 2 + sd1 ^ 2))
  sd1I <- sqrt((1 / n) * sum((rr_i - rr_ii) ^ 2) / 2)

  if (sdnn == 0)
    warning("There is no variability - is this a pacemaker?")
  if (sd2 == 0)
    warning("There is no long-term variability - is this a bigeminy?")

  results_hrv <- c(sdnn, sd1, sd2, sd1I)
  names(results_hrv) <- c("SDNN", "SD1", "SD2", "SD1I")

  # now on to HRA parameters

  xy <- (rr_ii - rr_i) / sqrt(2)
  decelerations <- which(xy > 0)
  accelerations <- which(xy < 0)
  nochange <- which(xy == 0)

  if (length(accelerations) == length(xy))
    warning("There are only accelerations in this recording.")
  if (length(decelerations) == length(xy))
    warning("There are only decelerations in this recording.")

  sd1d <- sqrt(sum(xy[decelerations] ^ 2) / n)
  sd1a <- sqrt(sum(xy[accelerations] ^ 2) / n)

  XY <- (rr_i - mean(rr_i) + rr_ii - mean(rr_ii)) / sqrt(2)

  sd2a <-
    sqrt(1 / n * (sum(XY[accelerations] ^ 2) + 1 / 2 * sum(XY[nochange] ^ 2)))
  sd2d <-
    sqrt(1 / n * (sum(XY[decelerations] ^ 2) + 1 / 2 * sum(XY[nochange] ^ 2)))

  sdnna <- sqrt(1 / 2 * (sd1a ^ 2 + sd2a ^ 2))
  sdnnd <- sqrt(1 / 2 * (sd1d ^ 2 + sd2d ^ 2))
  results_hra <- c(sdnnd, sdnna, sd1d, sd1a, sd2d, sd2a)
  names(results_hra) <-
    c("SDNNd", "SDNNa", "SD1d", "SD1a", "SD2d", "SD2a")
  results <- c(results_hrv, results_hra)

  return(results)
}

#' RR intervals time series statistics
#'
#' This function returns the statistics of the beats in the analyzed RR
#' intervals time series
#'
#' @param annotations The annotations in the analyzed RR intervals time series
#' @return an 1 x 6 vector containing the number of all beats (all), sinus beats (N), ventricular beats (V), supraventricular beats (S), artifacts (X) and unidentifed beats (U)
#' @export
#'
#' @examples

describerr <- function(annotations) {
  assert_that(is.vector(annotations),
              is.numeric(annotations),
              noNA(annotations),
              msg = "the annotations vector is either 1) not a vector, or 2) is not numeric or 3) has missing values")
  all_beats <- length(annotations)
  sinus_beats <- sum(annotations == 0)
  ventricular_beats <- sum(annotations == 1)
  supraventricular_beats <- sum(annotations == 2)
  artifacts <- sum(annotations == 3)
  unidentified_beats <- sum(!(annotations %in% c(0, 1, 2, 3)))
  statistics <- c(
    all_beats,
    sinus_beats,
    ventricular_beats,
    supraventricular_beats,
    artifacts,
    unidentified_beats
  )
  names(statistics) <- c("all", "N", "V", "S", "X", "U")
  return(statistics)
}
