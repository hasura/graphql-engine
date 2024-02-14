use lang_graphql::ast::common as ast;
use lang_graphql::schema as gql_schema;
use open_dds::types::InbuiltType;

pub(crate) fn base_type_container_for_inbuilt_type(
    inbuilt_type: &InbuiltType,
) -> ast::BaseTypeContainer<gql_schema::RegisteredTypeName> {
    match inbuilt_type {
        InbuiltType::ID => ast::BaseTypeContainer::Named(gql_schema::RegisteredTypeName::id()),
        InbuiltType::Int => ast::BaseTypeContainer::Named(gql_schema::RegisteredTypeName::int()),
        InbuiltType::Float => {
            ast::BaseTypeContainer::Named(gql_schema::RegisteredTypeName::float())
        }
        InbuiltType::Boolean => {
            ast::BaseTypeContainer::Named(gql_schema::RegisteredTypeName::boolean())
        }
        InbuiltType::String => {
            ast::BaseTypeContainer::Named(gql_schema::RegisteredTypeName::string())
        }
    }
}
