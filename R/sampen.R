#' Correlation Sums Calculation Using the NCM Algorithm
#'
#' This function calculates correlation sums for a given signal,
#' embedding dimension, and radius of comparison using the NCM algorithm.
#' It is used internally by `ncm_samp_en`.
#'
#' @inheritParams ncm_samp_en
#' @return numeric vector, correlation sums for different embedding dimensions.
ncm_correlation_sums <- function(signal, m, r) {
  tau <- 1
  m_range <- 0:(m-1)
  m_counts <- max(m_range) + 1
  corsum_matrix <- numeric(m_counts)
  
  #size of Norm component matrix
  size_X <- length(signal) - 1
  size_Y <- length(signal) - 1
  
  # triangular NCM matrix
  NCM <- matrix(0, nrow = size_X, ncol = size_Y)
  for (i_row in 0:(size_X-1)) {
    for (j_column in 0:(size_Y-1)) {
      if (i_row + (j_column + 1) * tau <= length(signal) - 1) {
        NCM[i_row + 1, j_column + 1] <- abs(signal[i_row + 1] - signal[i_row + 1 + (j_column + 1) * tau])
      }
    }
  }
  
  for (m_val in m_range) {
    for (current_row_idx in 0:(nrow(NCM) - m_val - 1)) {
      current_row <- NCM[(current_row_idx + 1):(current_row_idx + m_val + 1), 1:(ncol(NCM) - current_row_idx - m_val), drop = FALSE]
      if (nrow(current_row) > 0 && ncol(current_row) > 0) { 
        max_norms <- apply(current_row, 2, max)
        corsum_matrix[m_val + 1] <- corsum_matrix[m_val + 1] + sum(max_norms <= r)
      }
    }
  }
  
  # normalize correlation sum, multiply by 2 due to property of triangular matrix and exclude duplicates
  for (m_val in m_range) {
    factorA <- (length(signal) - (m_val) * tau)
    factorB <- (length(signal) - 1 - (m_val) * tau)
    factor <- factorA * factorB
    corsum_matrix[m_val + 1] <- corsum_matrix[m_val + 1] * 2 * 1 / factor
  }
  return(corsum_matrix)
}

#' Calculate Sample Entropy Using NCM Algorithm
#'
#' This function calculates the sample entropy for a given signal,
#' embedding dimension m, and radius of comparison r,
#' using the NCM algorithm developed by Zurek et al.
#'
#' @param signal numeric vector, the data for which the Sample Entropy is to be calculated.
#' @param m integer, the embedding dimension.
#' @param r numeric, the radius of comparison.
#' @return numeric, the calculated sample entropy.
#' @export
ncm_samp_en <- function(signal, m, r) {
  cm <- ncm_correlation_sums(signal, m, r)
  sampen <- log(cm[1]) - log(cm[2])
  return(sampen)
}