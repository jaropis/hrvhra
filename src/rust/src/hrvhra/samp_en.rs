/// Correlation Sums Calculation Using the NCM Algorithm
///
/// This function calculates correlation sums for a given signal,
/// embedding dimension, and radius of comparison using the NCM algorithm.
/// It is used internally by `ncm_samp_en`.
///
/// # Parameters
/// * `signal`: &[f64] - the data for which correlation sums are calculated
/// * `m`: usize - the embedding dimension
/// * `r`: f64 - the radius of comparison
/// # Returns
/// * Vec<f64> - correlation sums for different embedding dimensions
fn ncm_correlation_sums(signal: &[f64], m: usize, r: f64) -> Vec<f64> {
    let tau: usize = 1;
    let m_range: Vec<usize> = (0..m).collect();
    let m_counts = *m_range.iter().max().unwrap() + 1;
    let mut corsum_matrix = vec![0.0; m_counts];

    // avoiding creating the full NCM matrix in memory
    for &m_val in &m_range {
        let mut count = 0.0;

        // iterating through possible template vectors
        for i in 0..(signal.len() - m_val * tau - 1) {
            // comparing with other template vectors
            for j in (i + 1)..(signal.len() - m_val * tau) {
                // calculating maximum absolute difference between corresponding points
                let mut max_diff = 0.0;
                for k in 0..=m_val {
                    let diff = (signal[i + k * tau] - signal[j + k * tau]).abs();
                    if diff > max_diff {
                        max_diff = diff;
                    }
                }

                // counting templates within radius r
                if max_diff <= r {
                    count += 1.0;
                }
            }
        }

        // storing the normalized correlation sum
        let factor_a = (signal.len() - m_val * tau) as f64;
        let factor_b = (signal.len() - 1 - m_val * tau) as f64;
        corsum_matrix[m_val] = count * 2.0 / (factor_a * factor_b);
    }

    corsum_matrix
}

/// Calculate Sample Entropy Using NCM Algorithm
///
/// This function calculates the sample entropy for a given signal,
/// embedding dimension m, and radius of comparison r,
/// using the NCM algorithm developed by Zurek et al.
///
/// # Parameters
/// * `signal`: &[f64] - the data for which the Sample Entropy is to be calculated
/// * `m`: usize - the embedding dimension
/// * `r`: f64 - the radius of comparison
/// # Returns
/// * f64 - the calculated sample entropy
pub fn calc_samp_en(signal: &[f64], m: usize, r: f64) -> f64 {
    let cm = ncm_correlation_sums(signal, m, r);
    let sampen = cm[0].ln() - cm[1].ln();
    sampen
}
