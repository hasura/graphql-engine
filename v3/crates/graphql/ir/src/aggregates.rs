use lang_graphql::ast::common::Alias;

use plan_types::NdcFieldAlias;

pub fn mk_alias_from_graphql_field_path(graphql_field_path: &[&Alias]) -> NdcFieldAlias {
    NdcFieldAlias::from(
        graphql_field_path
            .iter()
            .map(|alias| alias.0.as_str())
            .collect::<Vec<&str>>()
            .join("_")
            .as_str(),
    )
}
