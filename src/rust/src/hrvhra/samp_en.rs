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

    // size of Norm component matrix
    let size_x = signal.len() - 1;
    let size_y = signal.len() - 1;

    // triangular NCM matrix
    let mut ncm = vec![vec![0.0; size_y]; size_x];
    for i_row in 0..size_x {
        for j_column in 0..size_y {
            if i_row + (j_column + 1) * tau <= signal.len() - 1 {
                ncm[i_row][j_column] = (signal[i_row] - signal[i_row + (j_column + 1) * tau]).abs();
            }
        }
    }

    for &m_val in &m_range {
        for current_row_idx in 0..(ncm.len() - m_val - 1) {
            // extracting the current submatrix
            let mut current_row = Vec::new();
            for i in 0..=m_val {
                let mut row = Vec::new();
                for j in 0..(ncm[0].len() - current_row_idx - m_val) {
                    row.push(ncm[current_row_idx + i][j]);
                }
                current_row.push(row);
            }

            // calculating max norms
            if !current_row.is_empty() && !current_row[0].is_empty() {
                let max_norms: Vec<f64> = (0..current_row[0].len())
                    .map(|j| {
                        let mut max_val: f64 = 0.0;
                        for i in 0..current_row.len() {
                            max_val = max_val.max(current_row[i][j]);
                        }
                        max_val
                    })
                    .collect();

                corsum_matrix[m_val] += max_norms.iter().filter(|&&x| x <= r).count() as f64;
            }
        }
    }

    // normalizing correlation sum, multiply by 2 due to property of triangular matrix and exclude duplicates
    for &m_val in &m_range {
        let factor_a = (signal.len() - m_val * tau) as f64;
        let factor_b = (signal.len() - 1 - m_val * tau) as f64;
        let factor = factor_a * factor_b;
        corsum_matrix[m_val] = corsum_matrix[m_val] * 2.0 * 1.0 / factor;
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
