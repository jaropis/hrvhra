use std::fs::File;
use std::io::{self, BufRead, BufReader};

pub struct RRSeries {
    pub rr: Vec<f64>,
    pub annot: Vec<i32>,
    pub column_names: Vec<String>,
    pub size: usize,
}

impl RRSeries {
    pub fn read_rr(path: &str) -> io::Result<Self> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);
        let mut lines = reader.lines();

        // reading header line
        let header = lines
            .next()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "empty file"))??;
        let column_names: Vec<String> = header.split_whitespace().map(String::from).collect();

        let mut rr = Vec::new();
        let mut annot = Vec::new();

        // processing data rows
        for line in lines {
            let line = line?;
            let values: Vec<&str> = line.split_whitespace().collect();
            if values.len() >= 2 {
                rr.push(values[0].parse::<f64>().unwrap());
                annot.push(values[1].parse::<i32>().unwrap());
            }
        }

        Ok(RRSeries {
            size: rr.len(),
            column_names,
            rr,
            annot,
        })
    }
}
