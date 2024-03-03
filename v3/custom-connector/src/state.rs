use std::{
    collections::BTreeMap,
    error::Error,
    fs::File,
    io::{self, BufRead},
};

pub type Row = BTreeMap<String, serde_json::Value>;

#[derive(Debug, Clone)]
pub struct AppState {
    pub actors: BTreeMap<i64, Row>,
    pub movies: BTreeMap<i64, Row>,
    pub institutions: BTreeMap<i64, Row>,
}

fn read_json_lines(path: &str) -> Result<BTreeMap<i64, Row>, Box<dyn Error>> {
    let file = File::open(path)?;
    let lines = io::BufReader::new(file).lines();
    let mut records: BTreeMap<i64, Row> = BTreeMap::new();
    for line in lines {
        let row: BTreeMap<String, serde_json::Value> = serde_json::from_str(&line?)?;
        let id = row
            .get("id")
            .ok_or("'id' field not found in json file")?
            .as_i64()
            .ok_or("'id' field was not an integer in json file")?;
        records.insert(id, row);
    }
    Ok(records)
}

pub fn init_app_state() -> AppState {
    // Read the CSV data files
    let actors = read_json_lines("./custom-connector/data/actors.json").unwrap();
    let movies = read_json_lines("./custom-connector/data/movies.json").unwrap();
    let institutions = read_json_lines("./custom-connector/data/institutions.json").unwrap();

    AppState {
        actors,
        movies,
        institutions,
    }
}
