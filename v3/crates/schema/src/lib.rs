// we deliberately do not export these entire modules and instead explicitly export types below
mod aggregates;
mod apollo_federation;
mod boolean_expression;
mod commands;
mod field_arguments;
mod model_arguments;
mod model_filter;
mod model_filter_input;
mod model_order_by;
mod mutation_root;
mod permissions;
mod query_root;
mod relay;
mod types;

use std::str::FromStr;

use serde::{Deserialize, Serialize};

use lang_graphql::schema::{self as gql_schema, SchemaContext};
use lang_graphql::{ast::common as ast, mk_name};
use metadata_resolve::Qualified;
use open_dds::{
    aggregates::AggregateExpressionName,
    commands::CommandName,
    models::ModelName,
    permissions::Role,
    relationships::RelationshipName,
    types::{CustomTypeName, Deprecated, FieldName},
};

pub use aggregates::{AggregateOutputAnnotation, AggregationFunctionAnnotation};
pub use types::output_type::relationship::{
    CommandRelationshipAnnotation, CommandTargetSource, FilterRelationshipAnnotation,
    ModelAggregateRelationshipAnnotation, ModelRelationshipAnnotation,
    OrderByRelationshipAnnotation,
};
pub use types::{
    Annotation, ApolloFederationRootFields, ArgumentNameAndPath, ArgumentPresets,
    BooleanExpressionAnnotation, CommandSourceDetail, EntityFieldTypeNameMapping, GlobalID,
    InputAnnotation, ModelFilterArgument, ModelInputAnnotation, ModelOrderByDirection,
    NamespaceAnnotation, NodeFieldTypeNameMapping, OutputAnnotation, RootFieldAnnotation,
    RootFieldKind, TypeKind,
};

/// This 'NamespacedGetter' looks up 'NamespacedNodeInfo's according to actual roles.
/// It is that instance of 'NamespacedGetter' that is used during normal request-processing
/// operations.
pub struct GDSRoleNamespaceGetter {
    pub scope: Role,
}

impl lang_graphql::schema::NamespacedGetter<GDS> for GDSRoleNamespaceGetter {
    fn get<'s, C>(
        &self,
        namespaced: &'s gql_schema::Namespaced<GDS, C>,
    ) -> Option<(&'s C, &'s <GDS as SchemaContext>::NamespacedNodeInfo)> {
        match &namespaced.namespaced {
            lang_graphql::schema::NamespacedData::AllowAll => Some((&namespaced.data, &None)),
            lang_graphql::schema::NamespacedData::Conditional(map) => map
                .get(&self.scope)
                .map(|namespaced_node_info| (&namespaced.data, namespaced_node_info)),
        }
    }
}

/// This 'NamespacedGetter' always returns empty 'NamespacedNodeInfo's without regard to the roles
/// that are asked for.
///
/// This produces a schema without any input presets defined and with access to all fields.
pub struct GDSNamespaceGetterAgnostic;

impl lang_graphql::schema::NamespacedGetter<GDS> for GDSNamespaceGetterAgnostic {
    fn get<'s, C>(
        &self,
        namespaced: &'s gql_schema::Namespaced<GDS, C>,
    ) -> Option<(&'s C, &'s <GDS as SchemaContext>::NamespacedNodeInfo)> {
        Some((&namespaced.data, &None))
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct GDS {
    pub metadata: metadata_resolve::Metadata,
}

impl GDS {
    pub fn new(
        user_metadata: open_dds::Metadata,
        metadata_resolve_configuration: metadata_resolve::configuration::Configuration,
    ) -> Result<Self, Error> {
        let resolved_metadata =
            metadata_resolve::resolve(user_metadata, metadata_resolve_configuration)?;
        Ok(GDS {
            metadata: resolved_metadata,
        })
    }

    pub fn new_with_default_flags(user_metadata: open_dds::Metadata) -> Result<Self, Error> {
        let resolved_metadata = metadata_resolve::resolve(
            user_metadata,
            metadata_resolve::configuration::Configuration::default(),
        )?;
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
            types::TypeId::InputObjectBooleanExpressionType {
                gds_type_name,
                graphql_type_name,
            } => boolean_expression::build_boolean_expression_input_schema(
                self,
                builder,
                graphql_type_name,
                gds_type_name,
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
            types::TypeId::ScalarTypeComparisonExpression {
                graphql_type_name,
                operators,
                operator_mapping,
                is_null_operator_name,
            } => model_filter::build_scalar_comparison_input(
                self,
                builder,
                graphql_type_name,
                operators,
                operator_mapping,
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
            types::TypeId::ApolloFederationType(types::PossibleApolloFederationTypes::Entity) => {
                Ok(gql_schema::TypeInfo::Union(
                    apollo_federation::apollo_federation_entities_schema(builder, self)?,
                ))
            }
            types::TypeId::ApolloFederationType(types::PossibleApolloFederationTypes::Any) => {
                Ok(gql_schema::TypeInfo::Scalar(gql_schema::Scalar {
                    name: ast::TypeName(mk_name!("_Any")),
                    description: None,
                    directives: Vec::new(),
                }))
            }
            types::TypeId::ApolloFederationType(types::PossibleApolloFederationTypes::Service) => {
                Ok(gql_schema::TypeInfo::Object(
                    apollo_federation::apollo_federation_service_schema(builder)?,
                ))
            }
            types::TypeId::AggregateSelectOutputType {
                aggregate_expression_name,
                graphql_type_name,
            } => aggregates::build_aggregate_select_output_type(
                self,
                builder,
                aggregate_expression_name,
                graphql_type_name,
            ),
            types::TypeId::ModelFilterInputType {
                model_name,
                graphql_type_name,
            } => model_filter_input::build_model_filter_input_type(
                self,
                builder,
                model_name,
                graphql_type_name,
            ),
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

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("metadata is not consistent: {error}")]
    ResolveError {
        #[from]
        error: metadata_resolve::Error,
    },
    #[error("internal error while building schema: {error}")]
    InternalBuildError {
        #[from]
        error: gql_schema::build::Error,
    },
    #[error("internal error: no support for: {summary}")]
    InternalUnsupported { summary: String },
    #[error("internal error while building schema, relationship not found: {relationship_name}")]
    InternalRelationshipNotFound { relationship_name: RelationshipName },
    #[error("internal error while building schema, type not found: {type_name}")]
    InternalTypeNotFound {
        type_name: Qualified<CustomTypeName>,
    },
    #[error(
        "internal error while building schema, field {field_name} not found in type {type_name}"
    )]
    InternalObjectTypeFieldNotFound {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
    },
    #[error("duplicate field name {field_name} generated while building object type {type_name}")]
    DuplicateFieldNameGeneratedInObjectType {
        field_name: ast::Name,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("field name for relationship {relationship_name} of type {type_name} conflicts with the existing field {field_name}")]
    RelationshipFieldNameConflict {
        relationship_name: RelationshipName,
        field_name: ast::Name,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("the aggregation function {field_name} conflicts with the aggregatable field {field_name} in the aggregate expression {aggregate_expression} is named {field_name}. Either rename the aggregation function or the field")]
    AggregationFunctionFieldNameConflict {
        aggregate_expression: Qualified<AggregateExpressionName>,
        field_name: ast::Name,
    },
    #[error("internal error: duplicate aggregatable field {field_name} in the aggregate expression {aggregate_expression} is named {field_name}")]
    InternalDuplicateAggregatableField {
        aggregate_expression: Qualified<AggregateExpressionName>,
        field_name: ast::Name,
    },
    #[error(
        "internal error: duplicate models with global id implementing the same type {type_name} are found"
    )]
    InternalErrorDuplicateGlobalIdSourceFound { type_name: ast::TypeName },
    #[error(
        "internal error: duplicate models with entity source for the same type {type_name} are found"
    )]
    InternalErrorDuplicateEntitySourceFound { type_name: ast::TypeName },
    #[error("internal error while building schema, model not found: {model_name}")]
    InternalModelNotFound { model_name: Qualified<ModelName> },
    #[error(
        "internal error while building schema, filter_expression for model not found: {model_name}"
    )]
    InternalModelFilterExpressionNotFound { model_name: Qualified<ModelName> },
    #[error("internal error while building schema, boolean expression not found: {type_name}")]
    InternalBooleanExpressionNotFound {
        type_name: Qualified<CustomTypeName>,
    },
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
    #[error("internal error while building schema, aggregate expression not found: {aggregate_expression}")]
    InternalAggregateExpressionNotFound {
        aggregate_expression: Qualified<AggregateExpressionName>,
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
    #[error("No graphql select type name has been defined for aggregate expression: {aggregate_expression}")]
    NoGraphQlSelectTypeNameForAggregateExpression {
        aggregate_expression: Qualified<AggregateExpressionName>,
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
    #[error(
        "Cannot generate the filter input type for model {model_name} since filterInputTypeName isn't defined in the graphql config"
    )]
    NoFilterInputTypeNameConfigNameForModel { model_name: Qualified<ModelName> },
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

    #[error("internal error: type mapping or field mapping not found for type {type_name:} and field {field_name:}")]
    InternalMappingNotFound {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
    },

    #[error("internal error: boolean expressions cannot be used as output types")]
    BooleanExpressionUsedAsOutputType,
}

impl From<ast::InvalidGraphQlName> for Error {
    fn from(error: ast::InvalidGraphQlName) -> Self {
        Error::InvalidGraphQlName { name: error.0 }
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

pub(crate) fn mk_deprecation_status(
    deprecated: &Option<Deprecated>,
) -> gql_schema::DeprecationStatus {
    match deprecated {
        Some(Deprecated { reason }) => gql_schema::DeprecationStatus::Deprecated {
            reason: Some(
                reason
                    .as_ref()
                    // Use @deprecated built-in directive default reason.
                    // Ref: https://spec.graphql.org/October2021/#sec--deprecated
                    .map_or_else(
                        || "No longer supported".to_owned(),
                        std::clone::Clone::clone,
                    ),
            ),
        },
        None => gql_schema::DeprecationStatus::NotDeprecated,
    }
}

#[cfg(test)]
mod tests {
    use std::{fs, path::Path, path::PathBuf};

    use hasura_authn_core::Role;

    use crate::{GDSNamespaceGetterAgnostic, GDSRoleNamespaceGetter};

    #[allow(clippy::print_stdout)]
    fn make_sdl_from_metadata_file_for_role(path: &Path, role: &Role) -> String {
        println!("{path:#?}");
        let metadata_string = fs::read_to_string(path).unwrap();
        let metadata =
            open_dds::traits::OpenDd::deserialize(serde_json::from_str(&metadata_string).unwrap())
                .unwrap();
        let gds = crate::GDS::new_with_default_flags(metadata).unwrap();
        let sch = gds.build_schema().unwrap();

        sch.generate_sdl(&GDSRoleNamespaceGetter {
            scope: role.clone(),
        })
    }

    #[allow(clippy::print_stdout)]
    fn make_role_agnostic_sdl_from_metadata_file(path: &Path) -> String {
        println!("{path:#?}");
        let metadata_string = fs::read_to_string(path).unwrap();
        let metadata =
            open_dds::traits::OpenDd::deserialize(serde_json::from_str(&metadata_string).unwrap())
                .unwrap();
        let gds = crate::GDS::new_with_default_flags(metadata).unwrap();
        let sch = gds.build_schema().unwrap();

        sch.generate_sdl(&GDSNamespaceGetterAgnostic)
    }

    #[test]
    fn test_roled_schemas_have_some_presets() {
        insta::with_settings!({snapshot_path => "../tests/snapshots"}, {
            insta::assert_snapshot!(make_sdl_from_metadata_file_for_role(
                PathBuf::from("tests/metadata_with_presets.json").as_ref(),
                &Role("role_with_presets".to_string())
            ));
        });
    }

    #[test]
    fn test_role_agnostic_schemas_have_no_presets() {
        insta::with_settings!({snapshot_path => "../tests/snapshots"}, {
            insta::assert_snapshot!(make_role_agnostic_sdl_from_metadata_file(
                PathBuf::from("tests/metadata_with_presets.json").as_ref()
            ));
        });
    }

    #[test]
    fn test_roled_schemas_have_some_permissions() {
        insta::with_settings!({snapshot_path => "../tests/snapshots"}, {
            insta::assert_snapshot!(make_sdl_from_metadata_file_for_role(
                PathBuf::from("tests/metadata_with_select_permissions.json").as_ref(),
                &Role("role_with_some_permissions".to_string())
            ));
        });
    }

    #[test]
    fn test_role_agnostic_schemas_have_all_permissions() {
        insta::with_settings!({snapshot_path => "../tests/snapshots"}, {
            insta::assert_snapshot!(make_role_agnostic_sdl_from_metadata_file(
                PathBuf::from("tests/metadata_with_select_permissions.json").as_ref()
            ));
        });
    }
}
