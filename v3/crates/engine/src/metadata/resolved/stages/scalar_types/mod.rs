use std::collections::{HashMap, HashSet};

use lang_graphql::ast::common as ast;

use crate::metadata::resolved::error::Error;
use crate::metadata::resolved::subgraph::Qualified;
use crate::metadata::resolved::types::{mk_name, store_new_graphql_type};

pub mod types;
pub use types::{ScalarTypeRepresentation, ScalarTypesOutput};

/// resolve scalar types
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    existing_graphql_types: &HashSet<ast::TypeName>,
) -> Result<ScalarTypesOutput, Error> {
    let mut scalar_types = HashMap::new();
    let mut graphql_types = existing_graphql_types.clone();

    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: scalar_type,
    } in &metadata_accessor.scalar_types
    {
        let graphql_type_name = match scalar_type.graphql.as_ref() {
            None => Ok(None),
            Some(type_name) => mk_name(type_name.type_name.0.as_ref())
                .map(ast::TypeName)
                .map(Some),
        }?;

        let qualified_scalar_type_name =
            Qualified::new(subgraph.to_string(), scalar_type.name.clone());

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
            return Err(Error::DuplicateTypeDefinition {
                name: qualified_scalar_type_name,
            });
        }
        store_new_graphql_type(&mut graphql_types, graphql_type_name.as_ref())?;
    }
    Ok(ScalarTypesOutput {
        scalar_types,
        graphql_types,
    })
}
