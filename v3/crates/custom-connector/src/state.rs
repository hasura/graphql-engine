use std::collections::BTreeMap;
use std::env;

pub type Row = BTreeMap<ndc_models::FieldName, serde_json::Value>;

#[derive(Debug, Clone)]
pub struct AppState {
    pub actors: BTreeMap<i32, Row>,
    pub countries: BTreeMap<i32, Row>,
    pub continents: BTreeMap<i32, Row>,
    pub institutions: BTreeMap<i32, Row>,
    pub movies: BTreeMap<i32, Row>,
    pub enable_relationship_support: bool,
}

const ACTORS_JSON: &str = include_str!("../data/actors.jsonl");
const INSTITUTIONS_JSON: &str = include_str!("../data/institutions.jsonl");
const MOVIES_JSON: &str = include_str!("../data/movies.jsonl");
const COUNTRIES_JSON: &str = include_str!("../data/countries.jsonl");
const CONTINENTS_JSON: &str = include_str!("../data/continents.jsonl");

fn read_json_lines(json: &str) -> anyhow::Result<BTreeMap<i32, Row>> {
    let lines = json.lines();
    let mut records: BTreeMap<i32, Row> = BTreeMap::new();
    for line in lines {
        let row: BTreeMap<ndc_models::FieldName, serde_json::Value> = serde_json::from_str(line)?;
        let id = row
            .get("id")
            .ok_or(anyhow::anyhow!("'id' field not found in json file"))?
            .as_i64()
            .ok_or(anyhow::anyhow!(
                "'id' field was not an integer in json file"
            ))?
            .try_into()?;
        records.insert(id, row);
    }
    Ok(records)
}

pub fn init_app_state() -> anyhow::Result<AppState> {
    let enable_relationship_support =
        env::var_os("ENABLE_RELATIONSHIP_SUPPORT") == Some("1".into());
    let actors = read_json_lines(ACTORS_JSON)?;
    let countries = read_json_lines(COUNTRIES_JSON)?;
    let continents = read_json_lines(CONTINENTS_JSON)?;
    let institutions = read_json_lines(INSTITUTIONS_JSON)?;
    let movies = read_json_lines(MOVIES_JSON)?;
    Ok(AppState {
        actors,
        countries,
        continents,
        institutions,
        movies,
        enable_relationship_support,
    })
}
