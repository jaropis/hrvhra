use crate::common::Annotations;
use crate::common::VarType;
use crate::runs_asym_helpers::sd_1_2_contribs;
use std::cmp;
use std::collections::HashMap;
use std::hash::Hash;

// defining run types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RunType {
    Dec = 1,  // deceleration run
    Neu = 0,  // neutral run
    Acc = -1, // acceleration run
}

// storing run statistics and addresses
#[derive(Debug, Clone)]
pub struct RunsAccumulator {
    dec: HashMap<usize, i32>,      // storing statistics for deceleration runs
    acc: HashMap<usize, i32>,      // storing statistics for acceleration runs
    neu: HashMap<usize, i32>,      // storing statistics for neutral runs
    runs_addresses: Vec<Vec<i32>>, // storing addresses of runs: [end address, length, type]
}

pub struct RRRuns {
    rr_intervals: Vec<f64>,
    mean_rr: f64,
    rr_length: usize,
    annotations: Vec<Annotations>,
    write_last_run: bool,
    accumulator: RunsAccumulator,
    runs_variances: HashMap<VarType, HashMap<RunType, Vec<f64>>>,
    total_vars: HashMap<VarType, f64>, // these will hold final variances, but calculated from runs, not in the ordinary fashion. Useful for testing
    analyzed: bool,
    max_dec: usize,
    max_acc: usize,
    max_neu: usize,
}

impl RRRuns {
    // creating new instance of RRRuns
    pub fn new(rr: Vec<f64>, annot: Vec<Annotations>, write_last_run: bool) -> Self {
        let size = rr.len();
        let accumulator = RunsAccumulator {
            dec: HashMap::new(),
            acc: HashMap::new(),
            neu: HashMap::new(),
            runs_addresses: Vec::new(),
        };
        let runs_variances: HashMap<VarType, HashMap<RunType, Vec<f64>>> = HashMap::new();
        let total_vars: HashMap<VarType, f64> = HashMap::new(); // this will hold variances calculated directly from runs. They MUST be exactly the same as those calculated by asym.rs
        let mut mean_rr = 0.0;
        for rr_i in &rr {
            mean_rr += rr_i;
        }
        mean_rr = mean_rr / size as f64;
        RRRuns {
            rr_intervals: rr,
            mean_rr: mean_rr,
            rr_length: size,
            annotations: annot,
            runs_variances: runs_variances,
            total_vars: total_vars,
            write_last_run,
            accumulator,
            analyzed: false,
            max_acc: 0,
            max_dec: 0,
            max_neu: 0,
        }
    }
    pub fn get_runs_summary(&mut self) -> Vec<Vec<i32>> {
        if !self.analyzed {
            self.analyze_runs();
        }
        // calculating max length to determine number of rows needed
        let max_length = cmp::max(cmp::max(self.max_acc, self.max_dec), self.max_neu);
        // building summary rows
        let mut summary = Vec::new();
        for i in 1..=max_length {
            let row = vec![
                if i <= self.max_acc {
                    *self.accumulator.acc.get(&i).unwrap_or(&0)
                } else {
                    0
                },
                if i <= self.max_dec {
                    *self.accumulator.dec.get(&i).unwrap_or(&0)
                } else {
                    0
                },
                if i <= self.max_neu {
                    *self.accumulator.neu.get(&i).unwrap_or(&0)
                } else {
                    0
                },
            ];
            summary.push(row);
        }

        // if summary is empty (no runs found), return a single row of zeros
        if summary.is_empty() {
            summary.push(vec![0, 0, 0]);
        }

        summary
    }
    pub fn get_nonzero_length(&self, map: &HashMap<usize, i32>) -> usize {
        let mut max: &usize = &0;
        for k in map.keys() {
            if max < k {
                max = k;
            }
        }
        *max
    }
    // updating runs addresses
    fn update_runs_addresses(&mut self, new_entry: Vec<i32>) {
        self.accumulator.runs_addresses.push(new_entry);
    }

    // analyzing runs
    fn analyze_runs(&mut self) {
        let mut flag_dec = false;
        let mut flag_acc = false;
        let mut flag_neu = false;
        let mut index_dec = 0;
        let mut index_acc = 0;
        let mut index_neu = 0;
        let mut running_rr_number = 0;
        // rewinding to first good flag
        // the `running_rr_number + 1 < len` bound is checked first so the
        // `annotations[running_rr_number + 1]` access below can never go out of bounds
        while running_rr_number + 1 < self.rr_intervals.len()
            && (self.annotations[running_rr_number] != Annotations::N
                || self.annotations[running_rr_number + 1] != Annotations::N)
        {
            running_rr_number += 1;
        }
        // if we scanned the whole recording without finding a viable pair of normal
        // beats (e.g. all-bad input), there are no runs to analyze - return early
        if running_rr_number + 1 >= self.rr_intervals.len()
            || self.annotations[running_rr_number] != Annotations::N
            || self.annotations[running_rr_number + 1] != Annotations::N
        {
            self.set_max();
            self.calculate_runs_variances();
            self.analyzed = true; // have to mark that this has been analyzed
            return; // returning early if we have jumped over all the recording and found no viable runs - this is an edge case
        }
        // initializing flags
        if self.rr_intervals[running_rr_number] < self.rr_intervals[running_rr_number + 1] {
            flag_dec = true;
            index_dec += 1;
        }
        if self.rr_intervals[running_rr_number] > self.rr_intervals[running_rr_number + 1] {
            flag_acc = true;
            index_acc += 1;
        }
        if self.rr_intervals[running_rr_number] == self.rr_intervals[running_rr_number + 1] {
            flag_neu = true;
            index_neu += 1;
        }
        running_rr_number += 1;
        while running_rr_number < (self.rr_intervals.len() - 1) {
            if self.annotations[running_rr_number + 1] != Annotations::N {
                if flag_dec {
                    *self.accumulator.dec.entry(index_dec).or_insert(0) += 1;
                    self.update_runs_addresses(vec![
                        running_rr_number as i32,
                        index_dec as i32,
                        RunType::Dec as i32,
                    ]);
                }
                if flag_acc {
                    *self.accumulator.acc.entry(index_acc).or_insert(0) += 1;
                    self.update_runs_addresses(vec![
                        running_rr_number as i32,
                        index_acc as i32,
                        RunType::Acc as i32,
                    ]);
                }
                if flag_neu {
                    *self.accumulator.neu.entry(index_neu).or_insert(0) += 1;
                    self.update_runs_addresses(vec![
                        running_rr_number as i32,
                        index_neu as i32,
                        RunType::Neu as i32,
                    ]);
                }
                index_dec = 0;
                index_acc = 0;
                index_neu = 0;
                flag_acc = false;
                flag_dec = false;
                flag_neu = false;
                // rewinding to last bad beat
                while self.annotations[running_rr_number] != Annotations::N
                    || self.annotations[running_rr_number + 1] != Annotations::N
                {
                    running_rr_number += 1;
                    if running_rr_number >= self.rr_intervals.len() - 1 {
                        self.set_max();
                        self.calculate_runs_variances();
                        self.analyzed = true; // have to mark that this has been analyzed
                        return;
                    }
                }
                if running_rr_number < self.rr_intervals.len() - 1 {
                    if self.rr_intervals[running_rr_number]
                        < self.rr_intervals[running_rr_number + 1]
                        && self.annotations[running_rr_number + 1] == Annotations::N
                    {
                        flag_dec = true;
                        index_dec += 1;
                    }
                    if self.rr_intervals[running_rr_number]
                        > self.rr_intervals[running_rr_number + 1]
                        && self.annotations[running_rr_number + 1] == Annotations::N
                    {
                        flag_acc = true;
                        index_acc += 1;
                    }
                    if self.rr_intervals[running_rr_number]
                        == self.rr_intervals[running_rr_number + 1]
                        && self.annotations[running_rr_number + 1] == Annotations::N
                    {
                        flag_neu = true;
                        index_neu += 1;
                    }
                }
                running_rr_number += 1; // for the next turn of the loop, because we are continuing
                continue;
            }

            if running_rr_number >= self.rr_intervals.len() - 1 {
                // TODO: Do I need this?
                break;
            }
            // getting the values once at the start
            #[derive(Debug)]
            enum Comparison {
                Greater,
                Smaller,
                Equal,
            }
            let both_normal = self.annotations[running_rr_number] == Annotations::N
                && self.annotations[running_rr_number + 1] == Annotations::N;

            if both_normal {
                let comparison = if self.rr_intervals[running_rr_number + 1]
                    > self.rr_intervals[running_rr_number]
                {
                    Comparison::Greater
                } else if self.rr_intervals[running_rr_number + 1]
                    < self.rr_intervals[running_rr_number]
                {
                    Comparison::Smaller
                } else {
                    Comparison::Equal
                };
                match comparison {
                    Comparison::Greater => {
                        index_dec += 1;
                        if !flag_dec {
                            if flag_acc {
                                *self.accumulator.acc.entry(index_acc).or_insert(0) += 1;
                                self.update_runs_addresses(vec![
                                    running_rr_number as i32,
                                    index_acc as i32,
                                    RunType::Acc as i32,
                                ]);
                                index_acc = 0;
                                flag_acc = false;
                            } else if flag_neu {
                                *self.accumulator.neu.entry(index_neu).or_insert(0) += 1;
                                self.update_runs_addresses(vec![
                                    running_rr_number as i32,
                                    index_neu as i32,
                                    RunType::Neu as i32,
                                ]);
                                index_neu = 0;
                                flag_neu = false;
                            }
                            flag_dec = true;
                        }
                    }
                    Comparison::Smaller => {
                        index_acc += 1;
                        if !flag_acc {
                            if flag_dec {
                                *self.accumulator.dec.entry(index_dec).or_insert(0) += 1;
                                self.update_runs_addresses(vec![
                                    running_rr_number as i32,
                                    index_dec as i32,
                                    RunType::Dec as i32,
                                ]);
                                index_dec = 0;
                                flag_dec = false;
                            } else if flag_neu {
                                *self.accumulator.neu.entry(index_neu).or_insert(0) += 1;
                                self.update_runs_addresses(vec![
                                    running_rr_number as i32,
                                    index_neu as i32,
                                    RunType::Neu as i32,
                                ]);
                                index_neu = 0;
                                flag_neu = false;
                            }
                            flag_acc = true;
                        }
                    }
                    Comparison::Equal => {
                        index_neu += 1;
                        if !flag_neu {
                            if flag_dec {
                                *self.accumulator.dec.entry(index_dec).or_insert(0) += 1;
                                self.update_runs_addresses(vec![
                                    running_rr_number as i32,
                                    index_dec as i32,
                                    RunType::Dec as i32,
                                ]);
                                index_dec = 0;
                                flag_dec = false;
                            } else if flag_acc {
                                *self.accumulator.acc.entry(index_acc).or_insert(0) += 1;
                                self.update_runs_addresses(vec![
                                    running_rr_number as i32,
                                    index_acc as i32,
                                    RunType::Acc as i32,
                                ]);
                                index_acc = 0;
                                flag_acc = false;
                            }
                            flag_neu = true;
                        }
                    }
                }
            }
            running_rr_number += 1;
        }
        // writing last run if needed
        if self.write_last_run {
            if index_acc > 0 {
                *self.accumulator.acc.entry(index_acc).or_insert(0) += 1;
                self.update_runs_addresses(vec![
                    running_rr_number as i32, // +1 i loops from running_rr_number + 1, so the loop ends at running_rr_number - 1
                    index_acc as i32,
                    RunType::Acc as i32,
                ]);
            }
            if index_dec > 0 {
                *self.accumulator.dec.entry(index_dec).or_insert(0) += 1;
                self.update_runs_addresses(vec![
                    running_rr_number as i32,
                    index_dec as i32,
                    RunType::Dec as i32,
                ]);
            }
            if index_neu > 0 {
                *self.accumulator.neu.entry(index_neu).or_insert(0) += 1;
                self.update_runs_addresses(vec![
                    running_rr_number as i32,
                    index_neu as i32,
                    RunType::Neu as i32,
                ]);
            }
        } else {
            println!("the last run not needed");
        }
        self.set_max();
        self.analyzed = true;
        self.calculate_runs_variances();
    }

    // setting maximal runs lengths for future use

    pub fn set_max(&mut self) {
        self.max_dec = self.get_nonzero_length(&self.accumulator.dec);
        self.max_acc = self.get_nonzero_length(&self.accumulator.acc);
        self.max_neu = self.get_nonzero_length(&self.accumulator.neu);
    }
    // getting full runs
    pub fn get_full_runs(
        &mut self,
    ) -> (
        &RunsAccumulator,
        &HashMap<VarType, HashMap<RunType, Vec<f64>>>,
    ) {
        if !self.analyzed {
            self.analyze_runs();
        }
        (&self.accumulator, &self.runs_variances)
    }

    // printing runs
    pub fn print_runs(&mut self) {
        if !self.analyzed {
            self.analyze_runs();
        }
        //println!("ful neu accumulator size: {:?}", self.accumulator.neu);
        let max_length = cmp::max(cmp::max(self.max_acc, self.max_dec), self.max_neu);
        println!("i  Ar - DR - N");
        for i in 1..max_length {
            println!(
                "{} {} - {} - {}",
                i,
                if i < self.max_acc {
                    *self.accumulator.acc.get(&i).unwrap_or(&0)
                } else {
                    0
                },
                if i < self.max_dec {
                    *self.accumulator.dec.get(&i).unwrap_or(&0)
                } else {
                    0
                },
                if i < self.max_neu {
                    *self.accumulator.neu.get(&i).unwrap_or(&0)
                } else {
                    0
                }
            );
        }
    }

    // printing addresses
    pub fn print_addresses(&mut self, run_type: RunType, run_length: i32, reference_beat: bool) {
        println!(
            "run type: {} run length: {}",
            match run_type {
                RunType::Dec => "DEC",
                RunType::Acc => "ACC",
                _ => "NEU",
            },
            run_length
        );

        if !self.analyzed {
            self.analyze_runs();
        }

        let reference_offset = if reference_beat { 1 } else { 0 };
        for run in &self.accumulator.runs_addresses {
            if run[2] == run_type as i32 && run[1] == run_length {
                let end_idx = run[0] as usize + reference_beat as usize;
                let length = (run[1] + reference_offset) as usize;
                if length <= end_idx + 1 {
                    let start_idx = end_idx - length;
                    println!("start_idx: {}, end_idx: {}", start_idx, end_idx);
                    for idx in start_idx..=end_idx {
                        // inclusive range
                        print!("{} ", self.rr_intervals[idx]);
                    }
                    println!();
                }
            }
        }
    }

    pub fn print_runs_addresses(&self) {
        for run in &self.accumulator.runs_addresses {
            println!("{:?}", run)
        }
    }

    pub fn print_runs_accumulator(&self) {
        println!("dec: {:?}", self.accumulator.dec);
        println!("acc: {:?}", self.accumulator.acc);
        println!("neu: {:?}", self.accumulator.neu);
    }
    fn calculate_runs_variances(&mut self) {
        // getting the vector with all sd1_i related variances for each point + the modifier;
        let (point_sd1_i_vars, point_sd2_vars, modifier) =
            sd_1_2_contribs(&self.rr_intervals, &self.annotations);
        for run in &self.accumulator.runs_addresses {
            let rr_index = run[0];
            let length = run[1];
            let run_type = run[2];

            let run_type_enum = match run_type {
                t if t == RunType::Dec as i32 => RunType::Dec,
                t if t == RunType::Acc as i32 => RunType::Acc,
                _ => RunType::Neu,
            };
            let max_len = match run_type_enum {
                RunType::Dec => self.max_dec,
                RunType::Acc => self.max_acc,
                RunType::Neu => self.max_neu,
            };
            // this either accesses an existing vector containing variances of runs of specific lengths, or creates if, if it does not,
            // i.e. it may return a reference to the vector of decelerations runs variances vector, each of the entries contains the variance of
            // a specific length and direction: index 0 - cumulative variance of all deceleration runs of length 1,
            // index 1: - cumulative variance of all deceleration runs of length 2 etc.
            let mut local_run_sd1_variance = 0.;
            let mut local_run_sd2_variance = 0.;
            for i in (rr_index - length + 1)..=rr_index {
                let local_var1 = point_sd1_i_vars[i as usize].expect("THIS CANNOT HAPPEN");
                let local_var2 = point_sd2_vars[i as usize].expect("THIS CANNOT HAPPEN");
                local_run_sd1_variance += local_var1 * modifier;
                local_run_sd2_variance += local_var2 * modifier;
            }
            let length_index = length - 1;
            for (var_type, contribution) in [
                (VarType::Var1i, local_run_sd1_variance),
                (VarType::Var2, local_run_sd2_variance),
            ] {
                let length_bins = self
                    .runs_variances
                    .entry(var_type)
                    .or_default()
                    .entry(run_type_enum)
                    .or_insert_with(|| vec![0.0; max_len]);

                length_bins[length_index as usize] += contribution;
            }
        }
        self.sum_variances()
    }
    pub fn print_runs_variances(&mut self) {
        println!(
            "square root of the sum of all variances is: {:?} , individual are: {:?}",
            self.total_vars, self.runs_variances
        )
    }
    pub fn get_runs_variances(&mut self) -> HashMap<VarType, f64> {
        if !self.analyzed {
            self.analyze_runs();
        }
        return self.total_vars.clone();
    }

    fn sum_variances(&mut self) {
        let mut var_sums_by_run = HashMap::new();
        let mut var_sums_full: HashMap<VarType, f64> = HashMap::new();
        for var_type in [VarType::Var1i, VarType::Var2] {
            let mut run_sums = HashMap::new();

            for run_type in [RunType::Dec, RunType::Acc, RunType::Neu] {
                let sum = self
                    .runs_variances
                    .get(&var_type)
                    .and_then(|stats| stats.get(&run_type))
                    .map_or(0.0, |variances| variances.iter().sum());

                run_sums.insert(run_type, sum);
            }

            var_sums_by_run.insert(var_type, run_sums);
        }

        var_sums_full.insert(
            VarType::Var1i,
            var_sums_by_run[&VarType::Var1i][&RunType::Dec]
                + var_sums_by_run[&VarType::Var1i][&RunType::Acc]
                + var_sums_by_run[&VarType::Var1i][&RunType::Neu],
        );
        var_sums_full.insert(
            VarType::Var2,
            var_sums_by_run[&VarType::Var2][&RunType::Dec]
                + var_sums_by_run[&VarType::Var2][&RunType::Acc]
                + var_sums_by_run[&VarType::Var2][&RunType::Neu],
        );
        var_sums_full.insert(
            VarType::VarNNi,
            0.5 * (var_sums_full[&VarType::Var1i] + var_sums_full[&VarType::Var2]),
        );
        var_sums_full.insert(
            VarType::Var1iD,
            var_sums_by_run[&VarType::Var1i][&RunType::Dec],
        );
        var_sums_full.insert(
            VarType::Var1iA,
            var_sums_by_run[&VarType::Var1i][&RunType::Acc],
        );
        var_sums_full.insert(
            VarType::Var2D,
            var_sums_by_run[&VarType::Var2][&RunType::Dec]
                + 0.5 * var_sums_by_run[&VarType::Var2][&RunType::Neu],
        );
        var_sums_full.insert(
            VarType::Var2A,
            var_sums_by_run[&VarType::Var2][&RunType::Acc]
                + 0.5 * var_sums_by_run[&VarType::Var2][&RunType::Neu],
        );
        var_sums_full.insert(
            VarType::VarNNiD,
            0.5 * (var_sums_full[&VarType::Var1iD] + var_sums_full[&VarType::Var2D]),
        );
        var_sums_full.insert(
            VarType::VarNNiA,
            0.5 * (var_sums_full[&VarType::Var1iA] + var_sums_full[&VarType::Var2A]),
        );
        self.total_vars = var_sums_full;
    }
}
