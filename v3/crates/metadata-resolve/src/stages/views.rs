use crate::types::subgraph::Qualified;
use indexmap::IndexMap;
use open_dds::views::ViewV1;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

mod dependencies;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ResolvedView {
    pub view: ViewV1,
    pub resolved_dependencies: Vec<Qualified<open_dds::views::ViewName>>,
}

#[derive(Debug)]
pub struct ViewsOutput {
    pub views: IndexMap<Qualified<open_dds::views::ViewName>, ResolvedView>,
    pub issues: Vec<crate::types::warning::Warning>,
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum Error {
    #[error("Circular dependency detected for view {view_name}")]
    CircularDependency {
        view_name: Qualified<open_dds::views::ViewName>,
    },
    #[error("Failed to parse SQL for view {view_name}: {reason}")]
    SqlParseError {
        view_name: Qualified<open_dds::views::ViewName>,
        reason: String,
    },
}

pub fn resolve(
    views: &Vec<open_dds::accessor::QualifiedObject<open_dds::views::ViewV1>>,
) -> Result<ViewsOutput, Error> {
    let mut all_views = Vec::new();
    let mut view_by_name = HashMap::new();

    // Collect all views across all subgraphs
    for view_object in views {
        let qualified_name: Qualified<open_dds::views::ViewName> = Qualified::new(
            view_object.subgraph.clone(),
            view_object.object.name.clone(),
        );
        view_by_name.insert(qualified_name.clone(), view_object);
        all_views.push(qualified_name);
    }

    // Build dependency graph by parsing SQL
    let mut dependency_graph = HashMap::new();
    for view_object in views {
        let view_name = Qualified::new(
            view_object.subgraph.clone(),
            view_object.object.name.clone(),
        );

        let dependencies = dependencies::extract_view_dependencies_from_sql(
            &view_object.object.sql_expression,
            &view_by_name.keys().cloned().collect(),
            &view_name,
        )
        .map_err(
            |dependencies::Error::SqlParseError { reason }| Error::SqlParseError {
                view_name: view_name.clone(),
                reason,
            },
        )?;

        dependency_graph.insert(view_name, dependencies);
    }

    // Topological sort to resolve dependencies
    let sorted_views = topological_sort(&dependency_graph)?;

    // Store views in dependency order using IndexMap to preserve order
    let mut resolved_views = IndexMap::new();
    for view_name in sorted_views {
        if let Some(view_object) = view_by_name.get(&view_name) {
            let resolved_deps = dependency_graph[&view_name].clone();

            resolved_views.insert(
                view_name,
                ResolvedView {
                    view: view_object.object.clone(),
                    resolved_dependencies: resolved_deps,
                },
            );
        }
    }

    Ok(ViewsOutput {
        views: resolved_views,
        issues: vec![], // Add validation issues as needed
    })
}

fn topological_sort(
    graph: &HashMap<
        Qualified<open_dds::views::ViewName>,
        Vec<Qualified<open_dds::views::ViewName>>,
    >,
) -> Result<Vec<Qualified<open_dds::views::ViewName>>, Error> {
    let mut result = Vec::new();
    let mut visited = HashSet::new();
    let mut visiting = HashSet::new();

    for node in graph.keys() {
        if !visited.contains(node) {
            visit(node, graph, &mut visited, &mut visiting, &mut result)?;
        }
    }

    Ok(result)
}

fn visit(
    node: &Qualified<open_dds::views::ViewName>,
    graph: &HashMap<
        Qualified<open_dds::views::ViewName>,
        Vec<Qualified<open_dds::views::ViewName>>,
    >,
    visited: &mut HashSet<Qualified<open_dds::views::ViewName>>,
    visiting: &mut HashSet<Qualified<open_dds::views::ViewName>>,
    result: &mut Vec<Qualified<open_dds::views::ViewName>>,
) -> Result<(), Error> {
    if visiting.contains(node) {
        return Err(Error::CircularDependency {
            view_name: node.clone(),
        });
    }

    if visited.contains(node) {
        return Ok(());
    }

    visiting.insert(node.clone());

    if let Some(dependencies) = graph.get(node) {
        for dep in dependencies {
            visit(dep, graph, visited, visiting, result)?;
        }
    }

    visiting.remove(node);
    visited.insert(node.clone());
    result.push(node.clone());

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use open_dds::accessor::QualifiedObject;
    use open_dds::identifier::{Identifier, SubgraphName};
    use open_dds::views::{ViewName, ViewV1};

    fn create_view_object(subgraph: &str, name: &str, sql: &str) -> QualifiedObject<ViewV1> {
        QualifiedObject {
            subgraph: SubgraphName::try_new(subgraph).unwrap(),
            object: ViewV1 {
                name: ViewName::new(Identifier::new(name).unwrap()),
                sql_expression: sql.to_string(),
                description: None,
            },
            path: jsonpath::JSONPath::new(),
        }
    }

    #[test]
    fn test_resolve_single_view() {
        let views = vec![create_view_object(
            "default",
            "users_view",
            "SELECT * FROM users_table",
        )];

        let result = resolve(&views).unwrap();

        assert_eq!(result.views.len(), 1);
        assert!(result.views.contains_key(&Qualified::new(
            SubgraphName::try_new("default").unwrap(),
            ViewName::new(Identifier::new("users_view").unwrap())
        )));
        assert_eq!(result.issues.len(), 0);
    }

    #[test]
    fn test_resolve_views_with_dependencies() {
        let views = vec![
            create_view_object("default", "base_users", "SELECT * FROM users_table"),
            create_view_object(
                "default",
                "active_users",
                "SELECT * FROM base_users WHERE active = true",
            ),
            create_view_object(
                "default",
                "user_summary",
                "SELECT id, name FROM active_users",
            ),
        ];

        let result = resolve(&views).unwrap();

        assert_eq!(result.views.len(), 3);

        // Check dependency order - base_users should come before active_users, which should come before user_summary
        let view_names: Vec<_> = result.views.keys().map(|k| k.name.to_string()).collect();
        let base_pos = view_names.iter().position(|n| n == "base_users").unwrap();
        let active_pos = view_names.iter().position(|n| n == "active_users").unwrap();
        let summary_pos = view_names.iter().position(|n| n == "user_summary").unwrap();

        assert!(base_pos < active_pos);
        assert!(active_pos < summary_pos);

        // Check dependencies are recorded correctly
        let active_users_key = Qualified::new(
            SubgraphName::try_new("default").unwrap(),
            ViewName::new(Identifier::new("active_users").unwrap()),
        );
        let active_users_view = &result.views[&active_users_key];
        assert_eq!(active_users_view.resolved_dependencies.len(), 1);
        assert_eq!(
            active_users_view.resolved_dependencies[0].name.to_string(),
            "base_users"
        );
    }

    #[test]
    fn test_resolve_circular_dependency_error() {
        let views = vec![
            create_view_object("default", "view_a", "SELECT * FROM view_b"),
            create_view_object("default", "view_b", "SELECT * FROM view_a"),
        ];

        let result = resolve(&views);

        assert!(result.is_err());
        match result.unwrap_err() {
            Error::CircularDependency { view_name } => {
                // Either view_a or view_b could be reported as the circular dependency
                assert!(
                    view_name.name.to_string() == "view_a"
                        || view_name.name.to_string() == "view_b"
                );
            }
            Error::SqlParseError { .. } => panic!("Expected CircularDependency error"),
        }
    }

    #[test]
    fn test_resolve_sql_parse_error() {
        let views = vec![create_view_object(
            "default",
            "invalid_view",
            "INVALID SQL SYNTAX",
        )];

        let result = resolve(&views);

        assert!(result.is_err());
        match result.unwrap_err() {
            Error::SqlParseError {
                view_name,
                reason: _,
            } => {
                assert_eq!(view_name.name.to_string(), "invalid_view");
            }
            Error::CircularDependency { .. } => panic!("Expected SqlParseError"),
        }
    }

    #[test]
    fn test_resolve_cross_subgraph_dependencies() {
        let views = vec![
            create_view_object("users", "base_users", "SELECT * FROM users_table"),
            create_view_object(
                "orders",
                "user_orders",
                "SELECT * FROM users.base_users u JOIN orders_table o ON u.id = o.user_id",
            ),
        ];

        let result = resolve(&views).unwrap();

        assert_eq!(result.views.len(), 2);

        // Check that user_orders depends on users.base_users
        let user_orders_key = Qualified::new(
            SubgraphName::try_new("orders").unwrap(),
            ViewName::new(Identifier::new("user_orders").unwrap()),
        );
        let user_orders_view = &result.views[&user_orders_key];
        assert_eq!(user_orders_view.resolved_dependencies.len(), 1);
        assert_eq!(
            user_orders_view.resolved_dependencies[0]
                .subgraph
                .to_string(),
            "users"
        );
        assert_eq!(
            user_orders_view.resolved_dependencies[0].name.to_string(),
            "base_users"
        );
    }

    #[test]
    fn test_resolve_complex_dependency_chain() {
        let views = vec![
            create_view_object("default", "raw_data", "SELECT * FROM raw_table"),
            create_view_object(
                "default",
                "cleaned_data",
                "SELECT * FROM raw_data WHERE valid = true",
            ),
            create_view_object(
                "default",
                "aggregated_data",
                "SELECT category, COUNT(*) FROM cleaned_data GROUP BY category",
            ),
            create_view_object(
                "default",
                "final_report",
                "SELECT * FROM aggregated_data WHERE count > 10",
            ),
        ];

        let result = resolve(&views).unwrap();

        assert_eq!(result.views.len(), 4);

        // Verify topological order
        let view_names: Vec<_> = result.views.keys().map(|k| k.name.to_string()).collect();
        let raw_pos = view_names.iter().position(|n| n == "raw_data").unwrap();
        let cleaned_pos = view_names.iter().position(|n| n == "cleaned_data").unwrap();
        let agg_pos = view_names
            .iter()
            .position(|n| n == "aggregated_data")
            .unwrap();
        let final_pos = view_names.iter().position(|n| n == "final_report").unwrap();

        assert!(raw_pos < cleaned_pos);
        assert!(cleaned_pos < agg_pos);
        assert!(agg_pos < final_pos);
    }

    #[test]
    fn test_resolve_no_dependencies() {
        let views = vec![
            create_view_object("default", "constants", "SELECT 1 as one, 2 as two"),
            create_view_object("default", "literals", "SELECT 'hello' as greeting"),
        ];

        let result = resolve(&views).unwrap();

        assert_eq!(result.views.len(), 2);

        // Both views should have no dependencies
        for (_, view) in &result.views {
            assert_eq!(view.resolved_dependencies.len(), 0);
        }
    }

    #[test]
    fn test_resolve_diamond_dependency() {
        let views = vec![
            create_view_object("default", "base", "SELECT * FROM base_table"),
            create_view_object("default", "left", "SELECT * FROM base WHERE type = 'left'"),
            create_view_object(
                "default",
                "right",
                "SELECT * FROM base WHERE type = 'right'",
            ),
            create_view_object(
                "default",
                "combined",
                "SELECT * FROM left UNION SELECT * FROM right",
            ),
        ];

        let result = resolve(&views).unwrap();

        assert_eq!(result.views.len(), 4);

        // Check that combined depends on both left and right
        let combined_key = Qualified::new(
            SubgraphName::try_new("default").unwrap(),
            ViewName::new(Identifier::new("combined").unwrap()),
        );
        let combined_view = &result.views[&combined_key];
        assert_eq!(combined_view.resolved_dependencies.len(), 2);

        let dep_names: Vec<_> = combined_view
            .resolved_dependencies
            .iter()
            .map(|d| d.name.to_string())
            .collect();
        assert!(dep_names.contains(&"left".to_string()));
        assert!(dep_names.contains(&"right".to_string()));
    }
}
