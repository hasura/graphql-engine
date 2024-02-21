use lang_graphql::ast::common as ast;
use lang_graphql::schema as gql_schema;
use open_dds::{
    commands::CommandName, models::ModelName, permissions::Role, relationships::RelationshipName,
    types::CustomTypeName,
};
use serde::{Deserialize, Serialize};
use std::str::FromStr;
use thiserror::Error;

use crate::metadata::{
    resolved::error::Error as ResolveMetadataError,
    resolved::metadata::{resolve_metadata, Metadata},
    resolved::subgraph::Qualified,
};

use self::types::RootFieldAnnotation;

pub mod commands;
pub mod model_arguments;
pub mod model_filter;
pub mod model_order_by;
pub mod mutation_root;
pub mod permissions;
pub mod query_root;
pub mod relay;
pub mod types;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct GDS {
    pub metadata: Metadata,
}

impl GDS {
    pub fn new(user_metadata: open_dds::Metadata) -> Result<Self, Error> {
        let resolved_metadata = resolve_metadata(user_metadata)?;
        Ok(GDS {
            metadata: resolved_metadata,
        })
    }

    pub fn build_schema(&self) -> std::result::Result<gql_schema::Schema<GDS>, Error> {
        gql_schema::build::build_schema(self)
    }
}

impl gql_schema::SchemaContext for GDS {
    type Namespace = Role;
    type GenericNodeInfo = types::Annotation;
    type NamespacedNodeInfo = Option<types::NamespaceAnnotation>;

    fn introspection_node() -> types::Annotation {
        types::Annotation::Output(types::OutputAnnotation::RootField(
            RootFieldAnnotation::Introspection,
        ))
    }

    fn introspection_namespace_node() -> Self::NamespacedNodeInfo {
        None
    }

    type TypeId = types::TypeId;

    fn to_type_name(type_id: &Self::TypeId) -> ast::TypeName {
        type_id.to_type_name()
    }

    type SchemaError = Error;

    fn build_type_info(
        &self,
        builder: &mut gql_schema::Builder<Self>,
        type_id: &Self::TypeId,
    ) -> std::result::Result<gql_schema::TypeInfo<Self>, Error> {
        match type_id {
            types::TypeId::QueryRoot { graphql_type_name } => Ok(gql_schema::TypeInfo::Object(
                query_root::query_root_schema(builder, self, graphql_type_name)?,
            )),
            types::TypeId::MutationRoot { graphql_type_name } => Ok(gql_schema::TypeInfo::Object(
                mutation_root::mutation_root_schema(builder, self, graphql_type_name)?,
            )),
            types::TypeId::OutputType {
                gds_type_name,
                graphql_type_name,
            } => types::output_type::output_type_schema(
                self,
                builder,
                gds_type_name,
                graphql_type_name,
            ),
            types::TypeId::ScalarType {
                gds_type_name,
                graphql_type_name,
                ..
            } => types::scalar_type::scalar_type_schema(self, gds_type_name, graphql_type_name),
            types::TypeId::InputObjectType {
                gds_type_name,
                graphql_type_name,
            } => types::input_type::input_object_type_schema(
                self,
                builder,
                gds_type_name,
                graphql_type_name,
            ),
            types::TypeId::NodeRoot => Ok(gql_schema::TypeInfo::Interface(
                relay::node_interface_schema(builder, self)?,
            )),
            types::TypeId::ModelArgumentsInput {
                model_name,
                type_name,
            } => model_arguments::build_model_arguments_input_schema(
                self, builder, type_name, model_name,
            ),
            types::TypeId::ModelBooleanExpression {
                model_name,
                graphql_type_name,
            } => model_filter::build_model_filter_expression_input_schema(
                self,
                builder,
                graphql_type_name,
                model_name,
            ),
            types::TypeId::ScalarTypeComparisonExpression {
                scalar_type_name: _,
                graphql_type_name,
                operators,
                is_null_operator_name,
            } => model_filter::build_scalar_comparison_input(
                self,
                builder,
                graphql_type_name,
                operators,
                is_null_operator_name,
            ),
            types::TypeId::ModelOrderByExpression {
                model_name,
                graphql_type_name,
            } => model_order_by::build_model_order_by_input_schema(
                self,
                builder,
                graphql_type_name,
                model_name,
            ),
            types::TypeId::OrderByEnumType { graphql_type_name } => {
                model_order_by::build_order_by_enum_type_schema(self, builder, graphql_type_name)
            }
        }
    }

    fn get_schema_entry_point(&self) -> gql_schema::EntryPoint<Self> {
        gql_schema::EntryPoint {
            query: types::TypeId::QueryRoot {
                graphql_type_name: self.metadata.graphql_config.query_root_type_name.clone(),
            },
            mutation: Some(types::TypeId::MutationRoot {
                graphql_type_name: self.metadata.graphql_config.mutation_root_type_name.clone(),
            }),
            subscription: None,
        }
    }
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("metadata is not consistent: {error}")]
    ResolveError {
        #[source]
        error: ResolveMetadataError,
    },
    #[error("internal error while building schema: {error}")]
    InternalBuildError {
        #[source]
        error: gql_schema::build::Error,
    },
    #[error("internal error: no support for: {summary}")]
    InternalUnsupported { summary: String },
    #[error("internal error while building schema, type not found: {type_name}")]
    InternalTypeNotFound {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("duplicate field name {field_name} generated while building object type {type_name}")]
    DuplicateFieldNameGeneratedInObjectType {
        field_name: ast::Name,
        type_name: Qualified<CustomTypeName>,
    },
    #[error(
        "internal error: duplicate models with global id implementing the same type {type_name} are found"
    )]
    InternalErrorDuplicateGlobalIdSourceFound { type_name: ast::TypeName },
    #[error("internal error while building schema, model not found: {model_name}")]
    InternalModelNotFound { model_name: Qualified<ModelName> },
    #[error(
        "internal error while building schema, filter_expression for model not found: {model_name}"
    )]
    InternalModelFilterExpressionNotFound { model_name: Qualified<ModelName> },
    #[error(
        "Conflicting argument names {argument_name} for field {field_name} of type {type_name}"
    )]
    GraphQlArgumentConflict {
        field_name: ast::Name,
        argument_name: ast::Name,
        type_name: ast::TypeName,
    },
    #[error("internal error while building schema, command not found: {command_name}")]
    InternalCommandNotFound {
        command_name: Qualified<CommandName>,
    },
    #[error("Cannot generate select_many API for model {model_name} since order_by_expression isn't defined")]
    NoOrderByExpression { model_name: Qualified<ModelName> },
    #[error("No graphql type name has been defined for scalar type: {type_name}")]
    NoGraphQlTypeNameForScalar {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("No graphql output type name has been defined for object type: {type_name}")]
    NoGraphQlOutputTypeNameForObject {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("No graphql input type name has been defined for object type: {type_name}")]
    NoGraphQlInputTypeNameForObject {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("\"{name:}\" is not a valid GraphQL name.")]
    InvalidGraphQlName { name: String },
    #[error(
        "Cannot generate arguments for model {model_name} since argumentsInputType and it's corresponding graphql config argumentsInput isn't defined"
    )]
    NoArgumentsInputConfigForSelectMany { model_name: Qualified<ModelName> },
    #[error("Internal error: Relationship capabilities are missing for {relationship} on type {type_name}")]
    InternalMissingRelationshipCapabilities {
        type_name: Qualified<CustomTypeName>,
        relationship: RelationshipName,
    },
    #[error("internal error: expected type {type_name} to be an object type")]
    ExpectedTypeToBeObject {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("internal error: Cannot generate select_many API for model {model_name} since the order_by_input GraphqlConfig is not defined")]
    InternalNoOrderByGraphqlConfig { model_name: Qualified<ModelName> },
    #[error("internal error: Cannot generate order_by enum type for type {type_name} since the order_by_input GraphqlConfig is not defined")]
    InternalNoOrderByGraphqlConfigOrderByEnumType { type_name: ast::TypeName },
    #[error("duplicate field name {field_name} generated while building query root")]
    DuplicateFieldInQueryRoot { field_name: ast::Name },
    #[error("Cannot have a function based command backed by a procedure or vice versa. Found for command {command_name:}")]
    IncorrectCommandBacking {
        command_name: Qualified<CommandName>,
    },
    #[error("relationships to procedure based commands are not supported")]
    RelationshipsToProcedureBasedCommandsAreNotSupported,
}

impl From<ast::InvalidGraphQlName> for Error {
    fn from(error: ast::InvalidGraphQlName) -> Self {
        Error::InvalidGraphQlName { name: error.0 }
    }
}

impl From<ResolveMetadataError> for Error {
    fn from(error: ResolveMetadataError) -> Self {
        Error::ResolveError { error }
    }
}

impl From<gql_schema::build::Error> for Error {
    fn from(error: gql_schema::build::Error) -> Self {
        Self::InternalBuildError { error }
    }
}

pub fn mk_typename(name: &str) -> Result<ast::TypeName, Error> {
    match ast::Name::from_str(name) {
        Ok(name) => Ok(ast::TypeName(name)),
        Err(_) => Err(Error::InvalidGraphQlName {
            name: name.to_string(),
        }),
    }
}
