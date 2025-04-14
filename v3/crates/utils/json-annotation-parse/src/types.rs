use indexmap::IndexMap;

// like serde_json::Value but each node is annotated
#[derive(Debug, Clone)]
pub enum Value<Ann> {
    Null(Ann),
    Bool(Ann, bool),
    Number(Ann, f64),
    String(Ann, String),
    Array(Ann, Vec<Value<Ann>>),
    Object(Ann, IndexMap<String, Value<Ann>>),
}
