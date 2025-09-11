use crate::types::subgraph::Qualified;
use open_dds::identifier::SubgraphName;
use sqlparser::ast::{ObjectName, Visit};
use sqlparser::dialect::GenericDialect;
use sqlparser::parser::Parser;
use std::collections::HashSet;
use std::ops::ControlFlow;

#[derive(Debug, Clone)]
pub enum Error {
    SqlParseError { reason: String },
}

pub(crate) fn extract_view_dependencies_from_sql(
    sql: &str,
    available_views: &HashSet<Qualified<open_dds::views::ViewName>>,
    view_name: &Qualified<open_dds::views::ViewName>,
) -> Result<Vec<Qualified<open_dds::views::ViewName>>, Error> {
    let statements: Vec<sqlparser::ast::Statement> = Parser::parse_sql(&GenericDialect {}, sql)
        .map_err(|e| Error::SqlParseError {
            reason: e.to_string(),
        })?;

    let mut visitor = ViewDependencyVisitor {
        dependencies: Vec::new(),
        available_views,
        current_subgraph: &view_name.subgraph,
    };
    let _ = statements.visit(&mut visitor);
    Ok(visitor.dependencies)
}

pub(crate) struct ViewDependencyVisitor<'a> {
    pub(crate) dependencies: Vec<Qualified<open_dds::views::ViewName>>,
    pub(crate) available_views: &'a HashSet<Qualified<open_dds::views::ViewName>>,
    pub(crate) current_subgraph: &'a SubgraphName,
}

impl sqlparser::ast::Visitor for ViewDependencyVisitor<'_> {
    type Break = ();

    fn pre_visit_relation(&mut self, relation: &sqlparser::ast::ObjectName) -> ControlFlow<()> {
        check_table_name(
            relation,
            &mut self.dependencies,
            self.available_views,
            self.current_subgraph,
        );
        ControlFlow::Continue(())
    }
}

pub(crate) fn check_table_name(
    object_name: &ObjectName,
    dependencies: &mut Vec<Qualified<open_dds::views::ViewName>>,
    available_views: &HashSet<Qualified<open_dds::views::ViewName>>,
    current_subgraph: &SubgraphName,
) {
    match &object_name.0[..] {
        [table_name] => {
            if let Ok(table_name) = open_dds::identifier::Identifier::new(table_name.to_string()) {
                let qual_name = Qualified::new(
                    current_subgraph.clone(),
                    open_dds::views::ViewName::new(table_name),
                );
                if available_views.contains(&qual_name) {
                    dependencies.push(qual_name);
                }
            }
        }
        [schema_name, table_name] => {
            if let Ok(subgraph_name) =
                open_dds::identifier::SubgraphName::try_new(schema_name.to_string())
            {
                if let Ok(table_name) =
                    open_dds::identifier::Identifier::new(table_name.to_string())
                {
                    let qual_name =
                        Qualified::new(subgraph_name, open_dds::views::ViewName::new(table_name));
                    if available_views.contains(&qual_name) {
                        dependencies.push(qual_name);
                    }
                }
            }
        }
        _ => {}
    }
}

// tests for extract_view_dependencies_from_sql
#[cfg(test)]
mod tests {
    use super::*;
    use open_dds::identifier::{Identifier, SubgraphName};
    use open_dds::views::ViewName;

    fn create_view_name(subgraph: &str, name: &str) -> Qualified<ViewName> {
        Qualified::new(
            SubgraphName::try_new(subgraph).unwrap(),
            ViewName::new(Identifier::new(name).unwrap()),
        )
    }

    #[test]
    fn test_simple_select_from_view() {
        let sql = "SELECT * FROM users";
        let view_name = create_view_name("default", "user_summary");

        let mut available_views = HashSet::new();
        available_views.insert(create_view_name("default", "users"));
        available_views.insert(create_view_name("other", "users"));

        let result = extract_view_dependencies_from_sql(sql, &available_views, &view_name).unwrap();

        assert_eq!(result.len(), 1);
        assert_eq!(result[0], create_view_name("default", "users"));
    }

    #[test]
    fn test_qualified_view_reference() {
        let sql = "SELECT * FROM other.users";
        let view_name = create_view_name("default", "user_summary");

        let mut available_views = HashSet::new();
        available_views.insert(create_view_name("default", "users"));
        available_views.insert(create_view_name("other", "users"));

        let result = extract_view_dependencies_from_sql(sql, &available_views, &view_name).unwrap();

        assert_eq!(result.len(), 1);
        assert_eq!(result[0], create_view_name("other", "users"));
    }

    #[test]
    fn test_join_multiple_views() {
        let sql = "SELECT * FROM users u JOIN orders o ON u.id = o.user_id";
        let view_name = create_view_name("default", "user_orders");

        let mut available_views = HashSet::new();
        available_views.insert(create_view_name("default", "users"));
        available_views.insert(create_view_name("default", "orders"));

        let result = extract_view_dependencies_from_sql(sql, &available_views, &view_name).unwrap();

        assert_eq!(result.len(), 2);
        assert!(result.contains(&create_view_name("default", "users")));
        assert!(result.contains(&create_view_name("default", "orders")));
    }

    #[test]
    fn test_subquery_dependencies() {
        let sql = "SELECT * FROM (SELECT * FROM users) u JOIN orders o ON u.id = o.user_id";
        let view_name = create_view_name("default", "complex_view");

        let mut available_views = HashSet::new();
        available_views.insert(create_view_name("default", "users"));
        available_views.insert(create_view_name("default", "orders"));

        let result = extract_view_dependencies_from_sql(sql, &available_views, &view_name).unwrap();

        assert_eq!(result.len(), 2);
        assert!(result.contains(&create_view_name("default", "users")));
        assert!(result.contains(&create_view_name("default", "orders")));
    }

    #[test]
    fn test_cte_dependencies() {
        let sql = "WITH user_stats AS (SELECT * FROM users) SELECT * FROM user_stats JOIN orders ON user_stats.id = orders.user_id";
        let view_name = create_view_name("default", "cte_view");

        let mut available_views = HashSet::new();
        available_views.insert(create_view_name("default", "users"));
        available_views.insert(create_view_name("default", "orders"));

        let result = extract_view_dependencies_from_sql(sql, &available_views, &view_name).unwrap();

        assert_eq!(result.len(), 2);
        assert!(result.contains(&create_view_name("default", "users")));
        assert!(result.contains(&create_view_name("default", "orders")));
    }

    #[test]
    fn test_no_dependencies() {
        let sql = "SELECT 1 as value";
        let view_name = create_view_name("default", "constant_view");

        let available_views = HashSet::new();

        let result = extract_view_dependencies_from_sql(sql, &available_views, &view_name).unwrap();

        assert_eq!(result.len(), 0);
    }

    #[test]
    fn test_non_existent_view_ignored() {
        let sql = "SELECT * FROM non_existent_table";
        let view_name = create_view_name("default", "test_view");

        let mut available_views = HashSet::new();
        available_views.insert(create_view_name("default", "users"));

        let result = extract_view_dependencies_from_sql(sql, &available_views, &view_name).unwrap();

        assert_eq!(result.len(), 0);
    }

    #[test]
    fn test_invalid_sql_returns_error() {
        let sql = "INVALID SQL SYNTAX";
        let view_name = create_view_name("default", "test_view");

        let available_views = HashSet::new();

        let result = extract_view_dependencies_from_sql(sql, &available_views, &view_name);

        assert!(result.is_err());
    }

    #[test]
    fn test_union_dependencies() {
        let sql = "SELECT * FROM users UNION SELECT * FROM customers";
        let view_name = create_view_name("default", "union_view");

        let mut available_views = HashSet::new();
        available_views.insert(create_view_name("default", "users"));
        available_views.insert(create_view_name("default", "customers"));

        let result = extract_view_dependencies_from_sql(sql, &available_views, &view_name).unwrap();

        assert_eq!(result.len(), 2);
        assert!(result.contains(&create_view_name("default", "users")));
        assert!(result.contains(&create_view_name("default", "customers")));
    }
}
