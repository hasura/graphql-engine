/// Represents a single element in a JSON path.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub enum JSONPathElement {
    Key(String),
    Index(usize),
}

/// Represents a JSON path.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct JSONPath(pub Vec<JSONPathElement>);

impl Default for JSONPath {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Display for JSONPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elems = self
            .0
            .iter()
            .map(|element| match element {
                JSONPathElement::Key(key) => format!(".{key}"),
                JSONPathElement::Index(index) => format!("[{index}]"),
            })
            .collect::<Vec<String>>();
        let mut path = vec!["$".to_string()];
        path.extend(elems);
        write!(f, "{}", path.join(""))
    }
}

impl JSONPath {
    pub fn new() -> Self {
        JSONPath(Vec::new())
    }

    pub fn new_key(key: &str) -> Self {
        JSONPath(vec![JSONPathElement::Key(key.to_string())])
    }

    pub fn new_index(index: usize) -> Self {
        JSONPath(vec![JSONPathElement::Index(index)])
    }

    pub fn prepend_key(self, key: String) -> Self {
        let mut new_path = vec![JSONPathElement::Key(key)];
        new_path.extend(self.0);
        JSONPath(new_path)
    }

    pub fn prepend_index(self, index: usize) -> Self {
        let mut new_path = vec![JSONPathElement::Index(index)];
        new_path.extend(self.0);
        JSONPath(new_path)
    }

    pub fn from_serde_path(path: &serde_path_to_error::Path) -> Self {
        JSONPath(
            path.iter()
                .filter_map(|segment| match segment {
                    serde_path_to_error::Segment::Seq { index } => {
                        Some(JSONPathElement::Index(*index))
                    }
                    serde_path_to_error::Segment::Map { key } => {
                        Some(JSONPathElement::Key(key.to_string()))
                    }
                    serde_path_to_error::Segment::Enum { variant } => {
                        Some(JSONPathElement::Key(variant.to_string()))
                    }
                    serde_path_to_error::Segment::Unknown => None,
                })
                .collect(),
        )
    }
}
