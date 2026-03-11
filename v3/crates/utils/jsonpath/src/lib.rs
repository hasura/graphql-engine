use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// Represents a single element in a JSON path.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub enum JSONPathElement {
    Key(String),
    Index(usize),
}

// Internally, JSONPath is a reversed linked list (cons list) where each node
// points to its parent. This makes clone() O(1) (Arc ref count bump) and
// append O(1) (new node pointing to parent). Materialization to Vec only
// happens for display, serialization, or subpath checks.

#[derive(Clone)]
enum JSONPathInner {
    Empty,
    Cons {
        element: JSONPathElement,
        parent: JSONPath,
    },
}

/// Represents a JSON path.
#[derive(Clone)]
pub struct JSONPath(Arc<JSONPathInner>);

impl std::fmt::Debug for JSONPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("JSONPath").field(&self.to_vec()).finish()
    }
}

impl Default for JSONPath {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq for JSONPath {
    fn eq(&self, other: &Self) -> bool {
        // Fast path: same Arc
        Arc::ptr_eq(&self.0, &other.0) || self.to_vec() == other.to_vec()
    }
}

impl Eq for JSONPath {}

impl Serialize for JSONPath {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.to_vec().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for JSONPath {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let elements = Vec::<JSONPathElement>::deserialize(deserializer)?;
        Ok(JSONPath::from_vec(elements))
    }
}

impl std::fmt::Display for JSONPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "$")?;
        for element in &self.to_vec() {
            match element {
                JSONPathElement::Key(key) => write!(f, ".{key}")?,
                JSONPathElement::Index(index) => write!(f, "[{index}]")?,
            }
        }
        Ok(())
    }
}

impl JSONPath {
    pub fn new() -> Self {
        JSONPath(Arc::new(JSONPathInner::Empty))
    }

    pub fn new_key(key: &str) -> Self {
        JSONPath::new().append_key(key.to_string())
    }

    pub fn new_index(index: usize) -> Self {
        JSONPath::new().append_index(index)
    }

    pub fn append_key(self, key: String) -> Self {
        JSONPath(Arc::new(JSONPathInner::Cons {
            element: JSONPathElement::Key(key),
            parent: self,
        }))
    }

    pub fn append_index(self, index: usize) -> Self {
        JSONPath(Arc::new(JSONPathInner::Cons {
            element: JSONPathElement::Index(index),
            parent: self,
        }))
    }

    pub fn prepend_key(self, key: String) -> Self {
        let mut elements = self.to_vec();
        elements.insert(0, JSONPathElement::Key(key));
        JSONPath::from_vec(elements)
    }

    pub fn prepend_index(self, index: usize) -> Self {
        let mut elements = self.to_vec();
        elements.insert(0, JSONPathElement::Index(index));
        JSONPath::from_vec(elements)
    }

    pub fn parent(self) -> Self {
        match self.0.as_ref() {
            JSONPathInner::Empty => self,
            JSONPathInner::Cons { parent, .. } => parent.clone(),
        }
    }

    pub fn from_serde_path(path: &serde_path_to_error::Path) -> Self {
        let elements: Vec<JSONPathElement> = path
            .iter()
            .filter_map(|segment| match segment {
                serde_path_to_error::Segment::Seq { index } => Some(JSONPathElement::Index(*index)),
                serde_path_to_error::Segment::Map { key } => {
                    Some(JSONPathElement::Key(key.clone()))
                }
                serde_path_to_error::Segment::Enum { variant } => {
                    Some(JSONPathElement::Key(variant.clone()))
                }
                serde_path_to_error::Segment::Unknown => None,
            })
            .collect();
        JSONPath::from_vec(elements)
    }

    /// Determines if the path contains a specified subpath somewhere inside it.
    /// For example: "$.a.b.c.d" contains "b.c"
    pub fn contains_subpath(&self, subpath: &[JSONPathElement]) -> bool {
        let elements = self.to_vec();
        elements
            .as_slice()
            .windows(subpath.len())
            .any(|window| window == subpath)
    }

    /// Materialize the path into a Vec of elements.
    pub fn to_vec(&self) -> Vec<JSONPathElement> {
        let mut elements = Vec::new();
        self.collect_elements(&mut elements);
        elements
    }

    fn collect_elements(&self, out: &mut Vec<JSONPathElement>) {
        match self.0.as_ref() {
            JSONPathInner::Empty => {}
            JSONPathInner::Cons { element, parent } => {
                parent.collect_elements(out);
                out.push(element.clone());
            }
        }
    }

    pub fn from_vec(elements: Vec<JSONPathElement>) -> Self {
        let mut path = JSONPath::new();
        for element in elements {
            path = match element {
                JSONPathElement::Key(key) => path.append_key(key),
                JSONPathElement::Index(index) => path.append_index(index),
            };
        }
        path
    }
}
