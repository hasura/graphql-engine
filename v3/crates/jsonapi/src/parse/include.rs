use std::collections::BTreeMap;

// Represents a parsed "include" query parameter
#[derive(Debug, Default)]
pub struct IncludeRelationships {
    pub include: BTreeMap<String, Option<IncludeRelationships>>,
}

impl IncludeRelationships {
    pub fn parse(include_relationships: &[String]) -> Self {
        let mut parser = IncludeRelationships::default();
        for include_str in include_relationships {
            let path_segments: Vec<&str> = include_str.trim().split('.').collect();
            parser.add_include_path(&path_segments);
        }
        parser
    }

    fn add_include_path(&mut self, segments: &[&str]) {
        match segments {
            [] => {}
            [first, nested_segments @ ..] => {
                let entry = self
                    .include
                    .entry((*first).to_string())
                    .or_insert_with(|| None);

                if !nested_segments.is_empty() {
                    let mut nested_include = IncludeRelationships::default();
                    nested_include.add_include_path(nested_segments);
                    match entry {
                        None => *entry = Some(nested_include),
                        Some(existing) => {
                            existing.include.extend(nested_include.include);
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_include_various_scenarios() {
        // Single relationship
        let single_include = vec!["authors".to_string()];
        let single_result = IncludeRelationships::parse(&single_include);
        assert!(single_result.include.contains_key("authors"));
        assert_eq!(single_result.include.len(), 1);

        // Nested relationship
        let nested_include = vec!["authors.books".to_string()];
        let nested_result = IncludeRelationships::parse(&nested_include);
        let authors_nested = nested_result
            .include
            .get("authors")
            .unwrap()
            .as_ref()
            .unwrap();
        assert!(authors_nested.include.contains_key("books"));

        // Multiple relationships
        let multi_include = vec![
            "authors".to_string(),
            "comments".to_string(),
            "authors.books.publisher".to_string(),
        ];
        let multi_result = IncludeRelationships::parse(&multi_include);

        // Check multiple top-level relationships
        assert!(multi_result.include.contains_key("authors"));
        assert!(multi_result.include.contains_key("comments"));

        // Check deep nested relationships
        let authors_nested = multi_result
            .include
            .get("authors")
            .unwrap()
            .as_ref()
            .unwrap();
        let books_nested = authors_nested
            .include
            .get("books")
            .unwrap()
            .as_ref()
            .unwrap();
        assert!(books_nested.include.contains_key("publisher"));
    }

    #[test]
    fn test_parse_include_edge_cases() {
        // Empty input
        let empty_include: Vec<String> = vec![];
        let empty_result = IncludeRelationships::parse(&empty_include);
        assert!(empty_result.include.is_empty());

        // Whitespace handling
        let whitespace_include = vec![" authors.books ".to_string()];
        let whitespace_result = IncludeRelationships::parse(&whitespace_include);
        assert!(whitespace_result.include.contains_key("authors"));
        let authors_nested = whitespace_result
            .include
            .get("authors")
            .unwrap()
            .as_ref()
            .unwrap();
        assert!(authors_nested.include.contains_key("books"));
    }

    #[test]
    fn test_parse_include_overlapping_relationships() {
        let overlapping_include = vec![
            "authors.books".to_string(),
            "authors.books.publisher".to_string(),
            "authors.awards".to_string(),
        ];
        let result = IncludeRelationships::parse(&overlapping_include);

        // Check base relationship exists
        assert!(result.include.contains_key("authors"));

        // Check nested relationships are correctly merged
        let authors_nested = result.include.get("authors").unwrap().as_ref().unwrap();
        assert!(authors_nested.include.contains_key("books"));
        assert!(authors_nested.include.contains_key("awards"));

        // Check deep nested relationship
        let books_nested = authors_nested
            .include
            .get("books")
            .unwrap()
            .as_ref()
            .unwrap();
        assert!(books_nested.include.contains_key("publisher"));
    }
}
