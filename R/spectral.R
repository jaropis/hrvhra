#' Function calculating Lomb-Scargle periodogram and returning power within the pre-defined bands
#' @param RR RR object
lomb_spectrum <- function(RR) {
  x_ts <- data.frame(sampling = cumsum(RR$RR) / 1000, # to get the results in Hz on x and ms^2 on y
                     samples = RR$RR)
  if (sum(RR$flags != 0)) {
    x_ts <- x_ts[-which(RR$flags != 0), ]
  }
  spectrum <- lomb::lsp(x_ts,
                        type = "frequency",
                        plot = FALSE)
  # total power will be sum(spectrum$power) * diff(spectrum$scanned)[1]  * 2 * var(RR$RR)
}
