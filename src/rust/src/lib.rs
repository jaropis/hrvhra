use extendr_api::prelude::*;

// import your existing code
mod hrvhra {
    pub mod data_reader;
    pub mod runs;
    pub mod samp_en;
}

use hrvhra::runs::RRRuns;

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
    // creating a new runs analyzer
    let mut runs = RRRuns::new(rr.to_vec(), annotations.to_vec(), write_last_run);
    
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
    list!(
        data = r_data,
        rows = r_rows,
        cols = r_cols
    ).into_robj()
}

/// Get a summary of runs analysis
/// @param rr Vector of RR intervals
/// @param annotations Vector of annotations (0 for normal beats, non-zero for abnormal)
/// @param write_last_run Whether to include the last run in the analysis
/// @return A vector of data with rows and columns as attributes
/// @export
#[extendr]
fn get_runs_summary(rr: &[f64], annotations: &[i32], write_last_run: bool) -> Robj {
    // creating a new runs analyzer
    let mut runs = RRRuns::new(rr.to_vec(), annotations.to_vec(), write_last_run);
    // getting the summary
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
    list!(
        data = r_data,
        rows = r_rows,
        cols = r_cols
    ).into_robj()
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod hrvhra;
    fn hello_world;
    fn analyze_rr_runs;
    fn get_runs_summary;
}