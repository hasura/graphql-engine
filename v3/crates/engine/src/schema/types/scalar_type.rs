use crate::{
    metadata::resolved::{subgraph::Qualified, types::TypeRepresentation},
    schema::GDS,
};
use lang_graphql::ast::common as ast;
use lang_graphql::schema as gql_schema;
use open_dds::types::CustomTypeName;

type Error = crate::schema::Error;

pub fn scalar_type_schema(
    gds: &GDS,
    type_name: &Qualified<CustomTypeName>,
    graphql_type_name: &ast::TypeName,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    let type_representation =
        gds.metadata
            .types
            .get(type_name)
            .ok_or_else(|| Error::InternalTypeNotFound {
                type_name: type_name.clone(),
            })?;

    let graphql_type_name = graphql_type_name.clone();

    match &type_representation {
        TypeRepresentation::Object(_object_type_representation) => {
            Err(Error::InternalUnsupported {
                summary: format!(
                    "a non-scalar type {} mapping to scalar GraphQL types",
                    type_name.clone()
                ),
            })
        }
        TypeRepresentation::ScalarType(scalar_type_representation) => {
            Ok(gql_schema::TypeInfo::Scalar(gql_schema::Scalar {
                name: graphql_type_name,
                description: scalar_type_representation.description.clone(),
                directives: Vec::new(),
            }))
        }
    }
}
