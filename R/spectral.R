#' Frequency bands 24 h
#' @export
frequency_bands_24 <- list(ULF = c(0, 0.003), VLF = c(0.003, .04), LF = c(0.04, 0.15), HF = c(0.15, 0.40))

#' Frequency bands < 24 h
#' @export
frequency_bands <- list(VLF = c(0.00, .04), LF = c(0.04, 0.15), HF = c(0.15, 0.40))

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
#' @param bands list in the form list(LF=c(0.00, .04)) etc.
#' @return list with bands names as names and power in bands as the values
calculate_frequency_bands <- function(spectrum, bands = frequency_bands) {
  power_in_bands <- lapply(bands, function(band) {
    sum(spectrum$power[spectrum$frequency >= band[1] & spectrum$frequency <= band[2]])
  })
  names(power_in_bands) <- names(bands)
  power_in_bands["TP"] <- sum(unlist(power_in_bands))
  return(power_in_bands)
}

#' Master function getting the results
#' @param RR RR object
#' @param bands frequency bands
#' @return list with power in bands
#' @export
calculate_RR_spectrum <- function(RR, bands = frequency_bands) {
  lomb_spectrum(RR) %>%
    calculate_frequency_bands(bands) %>%
    unlist()
}
