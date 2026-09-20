use extendr_api::prelude::*;

// import your existing code
mod hrvhra;

use hrvhra::common::Annotations;
use hrvhra::common::VarType;
use hrvhra::runs::RRRuns;
use hrvhra::runs_asym_helpers::sd_1_2_contribs;
use hrvhra::samp_en::calc_samp_en;

/// Return string `"Hello world!"` to R.
/// @export
#[extendr]
fn hello_world() -> &'static str {
    "Hello world!"
}

/// Create a new RRRuns object and return the runs analysis
/// @param rr Vector of RR intervals
/// @param annotations Vector of annotations (0 for normal beats, non-zero for abnormal)
/// @param write_last_run Whether to include the last run in the analysis
/// @return A list containing the runs analysis
/// @export
#[extendr]
fn analyze_rr_runs(rr: &[f64], annotations: &[i32], write_last_run: bool) -> Robj {
    let local_annotations = Annotations::to_vec_of_annot(
        annotations
            .iter()
            .copied()
            .map(u8::try_from)
            .collect::<std::result::Result<Vec<u8>, _>>()
            .expect("annotation codes must be between 0 and 255"),
    );
    // creating a new runs analyzer
    let mut runs = RRRuns::new(rr.to_vec(), local_annotations, write_last_run);

    // getting the runs summary
    let mut summary = runs.get_runs_summary();

    // convert summary to a flattened vector to pass to R
    let mut flat_data = Vec::new();
    let rows = summary.len();
    let cols = if rows > 0 { summary[0].len() } else { 3 };

    for row in &summary {
        for &val in row {
            flat_data.push(val);
        }
    }

    // convert dimensions to R objects
    let r_rows = rows.into_robj();
    let r_cols = cols.into_robj();
    let r_data = flat_data.into_robj();

    // return a list with the raw data and dimensions
    // R will need to reshape this into a matrix
    list!(data = r_data, rows = r_rows, cols = r_cols).into_robj()
}

/// Get a summary of runs analysis
/// @param rr Vector of RR intervals
/// @param annotations Vector of annotations (0 for normal beats, non-zero for abnormal)
/// @param write_last_run Whether to include the last run in the analysis
/// @return A vector of data with rows and columns as attributes
/// @export
#[extendr]
fn get_runs_summary(rr: &[f64], annotations: &[i32], write_last_run: bool) -> Robj {
    let local_annotations = Annotations::to_vec_of_annot(
        annotations
            .iter()
            .copied()
            .map(u8::try_from)
            .collect::<std::result::Result<Vec<u8>, _>>()
            .expect("annotation codes must be between 0 and 255"),
    );
    // creating a new runs analyzer
    let mut runs = RRRuns::new(rr.to_vec(), local_annotations, write_last_run);
    // getting the summary
    let (runs_summary, vars_summary) = runs.get_runs();
    // convert summary to a flattened vector to pass to R
    let runs_counts =
        List::from_values(runs_summary.into_iter().map(|row| row.into_robj())).into_robj();
    let runs_vars =
        List::from_values(vars_summary.into_iter().map(|row| row.into_robj())).into_robj();
    list!(runs_counts = runs_counts, runs_vars = runs_vars).into_robj()
}

/// Get the sample entropy for a signal
/// @param signal signal for which the sample entropy will be calculated
/// @param m embedding dimension - int
/// @param r comparison radius - float
/// @return a float
/// @export
#[extendr]
fn samp_en(signal: &[f64], m: usize, r: f64) -> f64 {
    calc_samp_en(signal, m, r)
}
// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod hrvhra;
    fn hello_world;
    fn analyze_rr_runs;
    fn get_runs_summary;
    fn samp_en;
}
