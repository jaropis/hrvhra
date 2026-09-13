use crate::asym::PoincarePlot;
use crate::common::Annotations;
use crate::stat_funcs::mean;
// use crate::stat_funcs::sd;

fn runs_form_pp(rr_intervals: &Vec<f64>, annotations: &Vec<Annotations>) -> PoincarePlot {
    let mut xi: Vec<f64> = vec![];
    let mut xii: Vec<f64> = vec![];
    for idx in 0..(rr_intervals.len() - 1) {
        if (annotations[idx] == Annotations::N) & (annotations[idx + 1] == Annotations::N) {
            xi.push(rr_intervals[idx]);
            xii.push(rr_intervals[idx + 1])
        }
    }
    return PoincarePlot { xi: xi, xii: xii };
}

pub fn get_mean_for_sd1(rr_intervals: &Vec<f64>, annotations: &Vec<Annotations>) -> f64 {
    let mut sum = 0.;
    let pp = runs_form_pp(rr_intervals, annotations);
    for i in 0..pp.xi.len() {
        sum += pp.xii[i] - pp.xi[i];
    }
    return sum / pp.xi.len() as f64;
}

pub fn sd_1_2_contribs(
    rr_intervals: &Vec<f64>,
    annotations: &Vec<Annotations>,
) -> (Vec<Option<f64>>, Vec<Option<f64>>, f64) {
    let rr_length = rr_intervals.len();
    let mut point_sd1_i_vars: Vec<Option<f64>> = vec![None; rr_length];
    // sd2 specific part
    let pp = runs_form_pp(rr_intervals, annotations);
    // TODO: the means can also be collected in the loop - for the paper
    let mean_rr_i = mean(&pp.xi);
    let mean_rr_ii = mean(&pp.xii);
    let mut point_sd2_vars: Vec<Option<f64>> = vec![None; rr_length];
    let mut none_counter = 1;
    point_sd1_i_vars[0] = None; // PAPER: this is not really necessary, because of the pre-allocation of the vector,
                                // but the runs can only start from 1 - each point in the PP plot contributes variance
                                // so let's look for the runs in the SECOND vector in the PP - this is the only possibility
    for i in 1..(rr_intervals.len()) {
        if annotations[i] != Annotations::N || annotations[i - 1] != Annotations::N {
            point_sd1_i_vars[i] = None;
            point_sd2_vars[i] = None;
            none_counter += 1;
            continue;
        }
        // sd1_i
        let local_diff = rr_intervals[i] - rr_intervals[i - 1];
        let local_diff_squared = local_diff * local_diff;
        // sd2
        let local_l2_perp_dist = (rr_intervals[i] - mean_rr_i + rr_intervals[i - 1] - mean_rr_ii);
        let local_l2_perp_dist_squared = local_l2_perp_dist * local_l2_perp_dist;
        if local_diff > 0.0 {
            point_sd1_i_vars[i] = Some(local_diff_squared / 2.);
            point_sd2_vars[i] = Some(local_l2_perp_dist_squared / 2.);
        }
        if local_diff < 0.0 {
            point_sd1_i_vars[i] = Some(local_diff_squared / 2.);
            point_sd2_vars[i] = Some(local_l2_perp_dist_squared / 2.);
        }
        if local_diff == 0.0 {
            point_sd1_i_vars[i] = Some(0.0);
            point_sd2_vars[i] = Some(local_l2_perp_dist_squared / 2.);
        }
    }
    let modifier = 1.0 / (rr_length - none_counter) as f64;

    return (point_sd1_i_vars, point_sd2_vars, modifier);
}
