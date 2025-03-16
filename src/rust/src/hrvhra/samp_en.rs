use ndarray::{Array1, Array2};

pub fn calc_correlation_sums(signal: &[f64], m: usize, r: f64) -> Array2<f64> {
    // getting preparing initial parameters
    let tau = 1;
    let m_counts = m + 1;

    // creating correlation sum matrix
    let mut corsum_matrix = Array2::<f64>::zeros((1, m_counts));

    // calculating sizes for NCM matrix
    let size_x = signal.len() - (1 - 1) * tau - 1;
    let size_y = signal.len() - 1 - (1 - 1) * tau;

    // creating triangular NCM matrix
    let mut ncm = Array2::<f64>::zeros((size_x, size_y));

    // filling NCM matrix
    for i_row in 0..size_x {
        for j_column in 0..size_y {
            if i_row + (j_column + 1) * tau <= signal.len() - 1 {
                ncm[[i_row, j_column]] =
                    (signal[i_row] - signal[i_row + (j_column + 1) * tau]).abs();
            }
        }
    }

    // calculating correlation sum for embedded dimension m
    for m_val in 0..=m {
        for current_row_idx in 0..ncm.nrows() - m_val {
            let mut current_row = Array1::<f64>::zeros(size_y - current_row_idx - m_val);

            // getting maximum values along axis 0 for the current segment
            for j in 0..size_y - current_row_idx - m_val {
                let mut max_val = ncm[[current_row_idx, j]];
                for i in 0..=m_val {
                    if current_row_idx + i < ncm.nrows() && j < ncm.ncols() {
                        max_val = max_val.max(ncm[[current_row_idx + i, j]]);
                    }
                }
                current_row[j] = max_val;
            }

            // counting values less than r
            let count = current_row.iter().filter(|&&x| x <= r).count() as f64;
            corsum_matrix[[0, m_val]] += count;
        }

        // normalizing correlation sum
        let factor_a = (signal.len() - m_val * tau) as f64;
        let factor_b = (signal.len() - 1 - m_val * tau) as f64;
        let factor = factor_a * factor_b;
        corsum_matrix[[0, m_val]] = corsum_matrix[[0, m_val]] * 2.0 / factor;
    }

    corsum_matrix
}

pub fn calc_samp_en(signal: &[f64], r: f64) -> f64 {
    // calculating sample entropy for single r value
    let cm = calc_correlation_sums(signal, 2, r);
    (cm[[0, 0]] / cm[[0, 1]]).ln()
}
