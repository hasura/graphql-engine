use crate::helpers::types::mk_name;
use crate::types::subgraph::Qualified;
use lang_graphql::ast::common as ast;
use open_dds::types::InbuiltType;
use std::collections::BTreeMap;
mod error;
pub use error::ScalarTypesError;
pub mod types;
use crate::stages::graphql_config;
use open_dds::types::CustomTypeName;
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
) -> Result<ScalarTypesOutput, Vec<ScalarTypesError>> {
    let mut scalar_types = BTreeMap::new();
    let mut issues = vec![];
    let mut results = vec![];

    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: scalar_type,
    } in &metadata_accessor.scalar_types
    {
        results.push(resolve_scalar_type(
            scalar_type,
            &mut issues,
            &mut scalar_types,
            graphql_types,
            subgraph,
        ));
    }

    // if everything succeeds, return results, otherwise collect all errors together
    partition_eithers::collect_any_errors(results).map(|_| ScalarTypesOutput {
        scalar_types,
        issues,
    })
}

fn resolve_scalar_type(
    scalar_type: &open_dds::types::ScalarTypeV1,
    issues: &mut Vec<ScalarTypesIssue>,
    scalar_types: &mut BTreeMap<Qualified<CustomTypeName>, ScalarTypeRepresentation>,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
    subgraph: &open_dds::identifier::SubgraphName,
) -> Result<(), ScalarTypesError> {
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

    graphql_types
        .store(graphql_type_name.as_ref())
        .map_err(ScalarTypesError::GraphqlConfigError)?;

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
        Err(ScalarTypesError::DuplicateTypeDefinition {
            name: qualified_scalar_type_name,
        })
    } else {
        Ok(())
    }
}
