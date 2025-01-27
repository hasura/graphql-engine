use crate::helpers::types::mk_name;
use crate::types::subgraph::Qualified;
use lang_graphql::ast::common as ast;
use open_dds::types::InbuiltType;
use std::collections::BTreeMap;
mod error;
pub use error::ScalarTypesError;
pub mod types;
use crate::stages::graphql_config;
use std::sync::LazyLock;
use strum::IntoEnumIterator;
pub use types::{ScalarTypeRepresentation, ScalarTypesIssue, ScalarTypesOutput};

// enumerate over inbuilt types to create list of disallowed names
static BUILT_IN_NAMES: LazyLock<Vec<String>> = LazyLock::new(|| {
    InbuiltType::iter()
        .map(|inbuilt| inbuilt.to_string())
        .collect()
});

/// resolve scalar types
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<ScalarTypesOutput, ScalarTypesError> {
    let mut scalar_types = BTreeMap::new();
    let mut issues = vec![];

    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: scalar_type,
    } in &metadata_accessor.scalar_types
    {
        let graphql_type_name = match scalar_type.graphql.as_ref() {
            None => Ok(None),
            Some(type_name) => mk_name(type_name.type_name.as_ref())
                .map(ast::TypeName)
                .map(Some),
        }
        .map_err(ScalarTypesError::GraphqlConfigError)?;

        // stop custom types being created that conflict with built-in ones
        if BUILT_IN_NAMES.contains(&scalar_type.name.to_string()) {
            issues.push(ScalarTypesIssue::NameConflictsWithBuiltInType {
                type_name: scalar_type.name.clone(),
            });
        }

        let qualified_scalar_type_name = Qualified::new(subgraph.clone(), scalar_type.name.clone());

        if scalar_types
            .insert(
                qualified_scalar_type_name.clone(),
                ScalarTypeRepresentation {
                    graphql_type_name: graphql_type_name.clone(),
                    description: scalar_type.description.clone(),
                },
            )
            .is_some()
        {
            return Err(ScalarTypesError::DuplicateTypeDefinition {
                name: qualified_scalar_type_name,
            });
        }
        graphql_types
            .store(graphql_type_name.as_ref())
            .map_err(ScalarTypesError::GraphqlConfigError)?;
    }
    Ok(ScalarTypesOutput {
        scalar_types,
        issues,
    })
}
