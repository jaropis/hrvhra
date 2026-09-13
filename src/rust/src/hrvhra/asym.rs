use crate::common::Annotations;
use crate::stat_funcs::mean;
use crate::stat_funcs::sd;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct AsymVarDesc {
    rr_intervals: Vec<f64>,
    annotations: Vec<Annotations>,
    pub pp: PoincarePlot,
    length: usize,
    pub quality_stats: QualityStats,
    pub time_length: f64,
    pub mean_rr: f64,
    pub sdnn: f64,
    pub sd1: f64,
    pub sd2: f64,
    pub sd1_i: f64,
    pub sd1a: f64,
    pub sd1d: f64,
    pub sd2a: f64,
    pub sd2d: f64,
    pub sdnn_i: f64,
    pub sdnn_a: f64,
    pub sdnn_d: f64,
    analyzed: bool,
}

#[derive(Debug, Clone, Default)] // the Default trait makes sure the starting values are all 0
pub struct QualityStats {
    n: usize, // normal
    v: usize, // ventricular
    s: usize, // supraventricular
    x: usize, // artifact
}

#[derive(Debug, Clone)]
pub struct PoincarePlot {
    pub xi: Vec<f64>,
    pub xii: Vec<f64>,
}

impl AsymVarDesc {
    pub fn new(rr_intervals: Vec<f64>, annotations: Vec<Annotations>) -> Self {
        let length = rr_intervals.len();
        return AsymVarDesc {
            rr_intervals: rr_intervals,
            annotations: annotations,
            pp: PoincarePlot {
                xi: vec![],
                xii: vec![],
            },
            length: length,
            quality_stats: QualityStats::default(),
            time_length: 1.0,
            mean_rr: 0.0,
            sdnn: 0.0,
            sd1: 0.0,
            sd2: 1.0,
            sd1_i: 1.0,
            sd1a: 1.0,
            sd1d: 1.0,
            sd2a: 1.0,
            sd2d: 1.0,
            sdnn_i: 1.0,
            sdnn_a: 1.0,
            sdnn_d: 1.0,
            analyzed: false,
        };
    }
    pub fn analyze_asym_var(&mut self) {
        self.quality_stats = self.get_quality_stats();
        self.pp = self.form_pp();
        self.mean_rr = self.mean_rr_ordinary();
        self.sdnn = self.sd(false);
        self.analyzed = true;
        self.sd1 = self.sd1();
        (self.sd2, self.sd2d, self.sd2a) = self.sd2();
        (self.sd1_i, self.sd1d, self.sd1a) = self.sd1_i();
        self.sdnn_a = (1. / 2. * (self.sd1a.powi(2) + self.sd2a.powi(2))).sqrt();
        self.sdnn_d = (1. / 2. * (self.sd1d.powi(2) + self.sd2d.powi(2))).sqrt();
        self.sdnn_i = (self.sdnn_a.powi(2) + self.sdnn_d.powi(2)).sqrt();
    }

    fn get_quality_stats(&self) -> QualityStats {
        let mut quality_stats = QualityStats {
            n: 0,
            v: 0,
            s: 0,
            x: 0,
        };
        for i in 0..self.length {
            match self.annotations[i] {
                Annotations::N => quality_stats.n = quality_stats.n + 1,
                Annotations::S => quality_stats.v = quality_stats.v + 1,
                Annotations::V => quality_stats.s = quality_stats.s + 1,
                Annotations::X => quality_stats.x = quality_stats.x + 1,
            }
        }
        quality_stats
    }
    fn form_pp(&mut self) -> PoincarePlot {
        let mut xi: Vec<f64> = vec![];
        let mut xii: Vec<f64> = vec![];
        for idx in 0..self.length - 1 {
            if (self.annotations[idx] == Annotations::N)
                & (self.annotations[idx + 1] == Annotations::N)
            {
                xi.push(self.rr_intervals[idx]);
                xii.push(self.rr_intervals[idx + 1])
            }
        }
        return PoincarePlot { xi: xi, xii: xii };
    }
    /* fn mean_rr_full(&mut self) -> f64 {
        // leaving this as a memento - mean can't be calculated as below
        let mut accumulator = 0.0;
        let length = self.pp.xi.len() - 1;
        for i in 0..self.pp.xi.len() as usize {
            accumulator = accumulator + self.pp.xi[i];
        }
        accumulator = accumulator + self.pp.xii[length];
        self.extra = format!(
            "full_length: {}, {}, {}\n",
            self.extra,
            accumulator,
            self.pp.xi.len() + 1
        );
        return accumulator / (self.pp.xi.len() + 1) as f64;
    } */
    fn mean_rr_ordinary(&mut self) -> f64 {
        let len = self.rr_intervals.len();
        let mut accumulator = 0.0;
        let mut len_accumulator = 0;
        for i in 0..len {
            if self.annotations[i] == Annotations::N {
                accumulator += self.rr_intervals[i];
                len_accumulator += 1;
            }
        }
        /*
        debugging statement
        self.extra = format!(
            "{}, {}, {}, {}, mean: {}",
            self.extra,
            "ordinary:",
            accumulator,
            len_accumulator,
            accumulator / len_accumulator as f64
        ); */
        return accumulator / len_accumulator as f64;
    }
    fn mean_rr_pp(&self) -> f64 {
        // this is calculated from xi only
        let mut accumulator = 0.0;
        for i in 0..self.pp.xi.len() as usize {
            accumulator = accumulator + self.pp.xi[i];
        }
        return accumulator / (self.pp.xi.len() as f64);
    }
    /// Returns the standard deviation
    /// #Arguments
    /// * `sample` - Whether sample sd or sd as an estimator should be estimated
    fn sd(&self, sample: bool) -> f64 {
        let mut rri: Vec<f64> = vec![];
        for i in 0..self.rr_intervals.len() {
            if self.annotations[i] == Annotations::N {
                rri.push(self.rr_intervals[i])
            }
        }

        return sd(&rri, sample);
    }

    fn sd1(&self) -> f64 {
        let pp_len = self.pp.xi.len();
        let mut diff = vec![0.0; pp_len];
        for i in 0..pp_len {
            let local_diff = &self.pp.xii[i] - &self.pp.xi[i];
            diff[i] = local_diff / 2.0_f64.sqrt();
        }
        return sd(&diff, false);
    }

    fn sd2(&self) -> (f64, f64, f64) {
        let pp_len = self.pp.xi.len();
        let mut sum = vec![0.0; pp_len];
        let mut var_2_d = 0.0;
        let mut var_2_a = 0.0;
        let modifier = 1.0 / (pp_len) as f64; // -1 because pp are shorter by 1
        let mean_rr_i = mean(&self.pp.xi);
        let mean_rr_ii = mean(&self.pp.xii);
        for i in 0..pp_len {
            let local_sum = &self.pp.xii[i] + &self.pp.xi[i];
            let local_l2_perp_dist =
                (&self.pp.xi[i] - mean_rr_i + &self.pp.xii[i] - mean_rr_ii) / 2_f64.sqrt();
            let local_diff = &self.pp.xii[i] - &self.pp.xi[i];
            sum[i] = local_sum / 2_f64.sqrt();
            let local_l2_perp_dist_squared = local_l2_perp_dist * local_l2_perp_dist;
            if local_diff > 0.0 {
                var_2_d += local_l2_perp_dist_squared;
            }
            if local_diff < 0. {
                var_2_a += local_l2_perp_dist_squared;
            }
            if local_diff == 0. {
                // spreading the variance on l1 equally between accelerations and decelerations
                var_2_d += 0.5 * local_l2_perp_dist_squared;
                var_2_a += 0.5 * local_l2_perp_dist_squared;
            }
        }
        // note that the way of calculating sd2 is totally different from the way of calculating
        // sd2d and sd2a - this is done to facilitate testing the partitioning
        return (
            sd(&sum, false),
            (modifier * var_2_d).sqrt(),
            (modifier * var_2_a).sqrt(),
        );
    }
    fn sd1_i(&self) -> (f64, f64, f64) {
        let pp_len = self.pp.xi.len();
        let mut var_1_i = 0.0;
        let mut var_1_d = 0.0;
        let mut var_1_a = 0.0;
        let modifier = 1.0 / pp_len as f64;
        for i in 0..pp_len {
            let local_diff = &self.pp.xii[i] - &self.pp.xi[i];
            let local_diff_squared = local_diff * local_diff;
            var_1_i += local_diff_squared / 2.;
            if local_diff > 0.0 {
                var_1_d += local_diff_squared / 2.;
            }
            if local_diff < 0.0 {
                var_1_a += local_diff_squared / 2.;
            }
        }
        return (
            (modifier * var_1_i).sqrt(),
            (modifier * var_1_d).sqrt(),
            (modifier * var_1_a).sqrt(),
        );
    }
    pub fn print_asym_results(self) {
        println!("the results are sdnn: {}, sd1: {}, sd1_i: {}, sd2: {}, sd1d: {}, sd1a: {}, sd2d: {}, sd2a: {}, sdnn_i:{}, sdnn_d: {}, sdnn_a: {}", 
        self.sdnn, 
        self.sd1, 
        self.sd1_i, 
        self.sd2, 
        self.sd1d, 
        self.sd1a, 
        self.sd2d, 
        self.sd2a,
        (self.sdnn_a.powi(2) + self.sdnn_d.powi(2) ).sqrt(),
        self.sdnn_d, 
        self.sdnn_a);
    }
}
