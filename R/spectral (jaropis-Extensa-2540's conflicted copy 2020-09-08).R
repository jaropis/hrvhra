#' Function calculating Lomb-Scargle periodogram and returning power within the pre-defined bands
#' power is normalized so that the total power is equal to the variance of the recording
#' @param RR RR object
#' @return list with power and frequency
lomb_spectrum <- function(RR) {
  x_ts <- data.frame(sampling = cumsum(RR$RR) / 1000, # to get the results in Hz on x and ms^2 on y
                     samples = RR$RR)
  if (sum(RR$flags != 0)) {
    x_ts <- x_ts[-which(RR$flags != 0), ]
  }
  spectrum <- lomb::lsp(x_ts,
                        type = "frequency",
                        plot = FALSE)
  # normalizing power spectrum so that we have correspondence between time domain and frequency domain
  power <- spectrum$power * 1/length(spectrum$scanned)  * var(RR$RR)
  return(list(power = power, frequency = spectrum$scanned))
}

#' Function to calculate power in specific bands
#' @param spectrum list with power and frequency
#' @param bands list in the form list(LF=c(0.01, 0.1)) etc.
#' @return list with bands names as names and power in bands as the values
#' @export
calculate_frequency_bands <- function(spectrum, bands) {
  power_in_bands <- lapply(bands, function(band) {
    sum(spectrum$power[spectrum$frequency >= band[1] && spectrum$frequency <= band[2]])
  })
  names(power_in_bands) <- names(bands)
  return(power_in_bands)
}

