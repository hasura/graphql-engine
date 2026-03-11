use crate::types::Value;
use jsonpath::{JSONPath, JSONPathElement};

#[derive(Debug)]
pub enum WalkError<Ann> {
    PathValueMismatch(JSONPathElement, Value<Ann>),
    MissingIndex(usize, Value<Ann>),
    MissingKey(String, Value<Ann>),
}

// walk the path elements from root to leaf
pub fn walk<'a, Ann: Clone>(
    value: &'a Value<Ann>,
    path: &mut JSONPath,
) -> Result<&'a Ann, WalkError<Ann>> {
    let elements = path.to_vec();
    walk_inner(value, &elements, 0)
}

fn walk_inner<'a, Ann: Clone>(
    value: &'a Value<Ann>,
    elements: &[JSONPathElement],
    index: usize,
) -> Result<&'a Ann, WalkError<Ann>> {
    match elements.get(index) {
        Some(element) => {
            match (value, element) {
                (Value::Array(_, items), JSONPathElement::Index(i)) => {
                    match items.get(*i) {
                        Some(item) => {
                            // got it!
                            walk_inner(item, elements, index + 1)
                        }
                        None => Err(WalkError::MissingIndex(*i, value.clone())),
                    }
                }
                (Value::Object(_, items), JSONPathElement::Key(s)) => {
                    match items.get(s) {
                        Some(item) => {
                            // got it!
                            walk_inner(item, elements, index + 1)
                        }
                        None => Err(WalkError::MissingKey(s.to_string(), value.clone())),
                    }
                }
                _ => Err(WalkError::PathValueMismatch(element.clone(), value.clone())),
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
