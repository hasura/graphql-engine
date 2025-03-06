use crate::types::Value;
use jsonpath::{JSONPath, JSONPathElement};

#[derive(Debug)]
pub enum WalkError<Ann> {
    PathValueMismatch(JSONPathElement, Value<Ann>),
    MissingIndex(usize, Value<Ann>),
    MissingKey(String, Value<Ann>),
}

// reverse the path then walk it
pub fn walk<'a, Ann: Clone>(
    value: &'a Value<Ann>,
    path: &mut JSONPath,
) -> Result<&'a Ann, WalkError<Ann>> {
    let mut reversed_path = JSONPath(path.0.iter().rev().cloned().collect());
    walk_inner(value, &mut reversed_path)
}

fn walk_inner<'a, Ann: Clone>(
    value: &'a Value<Ann>,
    path: &mut JSONPath,
) -> Result<&'a Ann, WalkError<Ann>> {
    match path.0.pop() {
        Some(tail) => {
            match (value, &tail) {
                (Value::Array(_, items), JSONPathElement::Index(i)) => {
                    match items.get(*i) {
                        Some(item) => {
                            // got it!
                            walk_inner(item, path)
                        }
                        None => Err(WalkError::MissingIndex(*i, value.clone())),
                    }
                }
                (Value::Object(_, items), JSONPathElement::Key(s)) => {
                    match items.get(s) {
                        Some(item) => {
                            // got it!
                            walk_inner(item, path)
                        }
                        None => Err(WalkError::MissingKey(s.to_string(), value.clone())),
                    }
                }
                _ => Err(WalkError::PathValueMismatch(tail.clone(), value.clone())),
            }
        }
        None => {
            // we are here, return outer annotation
            match value {
                Value::Bool(ann, _) => Ok(ann),
                Value::Null(ann) => Ok(ann),
                Value::Number(ann, _) => Ok(ann),
                Value::String(ann, _) => Ok(ann),
                Value::Array(ann, _) => Ok(ann),
                Value::Object(ann, _) => Ok(ann),
            }
        }
    }
}
