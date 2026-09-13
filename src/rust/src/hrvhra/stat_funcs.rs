pub fn mean(rr: &Vec<f64>) -> f64 {
    if rr.is_empty() {
        return 0.0;
    }

    let mut accumulator = 0.0;
    let length = rr.len();
    for i in 0..length as usize {
        accumulator = accumulator + rr[i];
    }
    return accumulator / (length) as f64;
}

pub fn sd(rr: &Vec<f64>, sample: bool) -> f64 {
    let mut var_accu = 0.0;
    let mut comp = 0.0;
    let n = rr.len();

    if n == 0 || (sample && n < 2) {
        return 0.0;
    }

    let mean = mean(&rr);

    for rr_i in rr {
        (comp, var_accu) = sum_of_squares(*rr_i, mean, comp, var_accu);
    }

    let divisor = if sample { n - 1 } else { n };
    return (var_accu / divisor as f64).sqrt();
}
fn sum_of_squares(rr: f64, mean: f64, comp: f64, var_accu: f64) -> (f64, f64) {
    let diff = rr - mean;
    let term = diff * diff;
    let y = term - comp;
    let t = var_accu + y;
    return ((t - var_accu) - y, t); // this returns the new values of comp and var_accu, in order
}
