#' Function calculating Lomb-Scargle periodogram and returning power within the pre-defined bands
#' @param RR RR object
lomb_spectrum <- function(RR) {
  x_ts <- data.frame(sampling = cumsum(RR$RR) / 1000, # to get the results in Hz on x and ms^2 on y
                     samples = RR$RR)
  spectrum <- lomb::lsp(x_ts[-which(RR$flags != 0), ],
                        type = "frequency",
                        plot = FALSE)
  # total power will be sum(spectrum$power) * diff(spectrum$scanned)[1]
}
