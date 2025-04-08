//! IR of the query root type

use std::collections::BTreeMap;

use hasura_authn_core::Session;
use indexmap::IndexMap;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use open_dds::{commands::CommandName, models};

use plan_types::UsagesCounts;
use std::collections::HashMap;

use super::commands;
use super::error;
use super::root_field;
use graphql_schema::ApolloFederationRootFields;
use graphql_schema::EntityFieldTypeNameMapping;
use graphql_schema::RootFieldKind;
use graphql_schema::TypeKind;
use graphql_schema::{Annotation, NodeFieldTypeNameMapping, OutputAnnotation, RootFieldAnnotation};
use graphql_schema::{GDS, mk_typename};

pub mod apollo_federation;
pub mod node_field;
pub mod select_aggregate;
pub mod select_many;
pub mod select_one;

/// Generates IR for the selection set of type 'query root'
pub fn generate_ir<'n, 's>(
    schema: &'s gql::schema::Schema<GDS>,
    metadata: &'s metadata_resolve::Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    selection_set: &'s gql::normalized_ast::SelectionSet<'s, GDS>,
) -> Result<IndexMap<ast::Alias, root_field::QueryRootField<'n, 's>>, error::Error> {
    let type_name = selection_set
        .type_name
        .clone()
        .ok_or_else(|| gql::normalized_ast::Error::NoTypenameFound)?;
    let mut ir = IndexMap::new();
    for (alias, field) in &selection_set.fields {
        let field_call = field.field_call()?;
        let field_ir = match field_call.name.as_str() {
            "__typename" => Ok(root_field::QueryRootField::TypeName {
                type_name: type_name.clone(),
            }),
            "__schema" => Ok(root_field::QueryRootField::SchemaField {
                role: session.role.clone(),
                selection_set: &field.selection_set,
                schema,
            }),
            "__type" => {
                let ir = generate_type_field_ir(schema, &field.selection_set, field_call, session)?;
                Ok(ir)
            }
            _ => match field_call.info.generic {
                annotation @ Annotation::Output(field_annotation) => match field_annotation {
                    OutputAnnotation::RootField(root_field) => match root_field {
                        RootFieldAnnotation::Model {
                            kind,
                            name: model_name,
                        } => {
                            let model = metadata.models.get(model_name).ok_or_else(|| {
                                error::InternalEngineError::InternalGeneric {
                                    description: format!("Model {model_name} not found"),
                                }
                            })?;
                            let ir = generate_model_rootfield_ir(
                                &type_name,
                                model,
                                &metadata.models,
                                &metadata.object_types,
                                kind,
                                field,
                                field_call,
                                session,
                                request_headers,
                                model_name,
                            )?;
                            Ok(ir)
                        }
                        RootFieldAnnotation::FunctionCommand {
                            name,
                            function_name,
                            result_type,
                            result_base_type_kind,
                        } => {
                            let command = metadata.commands.get(name).ok_or_else(|| {
                                error::InternalEngineError::InternalGeneric {
                                    description: format!("Command {name} not found"),
                                }
                            })?;
                            let ir = generate_command_rootfield_ir(
                                name,
                                &type_name,
                                function_name.as_ref(),
                                command,
                                result_type,
                                result_base_type_kind,
                                field,
                                field_call,
                                &metadata.models,
                                &metadata.object_types,
                                session,
                                request_headers,
                            )?;
                            Ok(ir)
                        }
                        RootFieldAnnotation::RelayNode { typename_mappings } => {
                            let ir = generate_nodefield_ir(
                                field,
                                field_call,
                                typename_mappings,
                                &metadata.models,
                                &metadata.object_types,
                                session,
                                request_headers,
                            )?;
                            Ok(ir)
                        }
                        RootFieldAnnotation::ApolloFederation(
                            ApolloFederationRootFields::Entities { typename_mappings },
                        ) => {
                            let ir = generate_entities_ir(
                                field,
                                field_call,
                                typename_mappings,
                                &metadata.models,
                                &metadata.object_types,
                                session,
                                request_headers,
                            )?;
                            Ok(ir)
                        }
                        RootFieldAnnotation::ApolloFederation(
                            ApolloFederationRootFields::Service,
                        ) => Ok(root_field::QueryRootField::ApolloFederation(
                            root_field::ApolloFederationRootFields::ServiceField {
                                selection_set: &field.selection_set,
                                schema,
                                role: session.role.clone(),
                            },
                        )),
                        _ => Err(error::Error::from(
                            error::InternalEngineError::UnexpectedAnnotation {
                                annotation: annotation.clone(),
                            },
                        )),
                    },
                    _ => Err(error::Error::from(
                        error::InternalEngineError::UnexpectedAnnotation {
                            annotation: annotation.clone(),
                        },
                    )),
                },
                annotation => Err(error::Error::from(
                    error::InternalEngineError::UnexpectedAnnotation {
                        annotation: annotation.clone(),
                    },
                )),
            },
        }?;
        ir.insert(alias.clone(), field_ir);
    }
    Ok(ir)
}

fn generate_type_field_ir<'n, 's>(
    schema: &'s gql::schema::Schema<GDS>,
    selection_set: &'s gql::normalized_ast::SelectionSet<GDS>,
    field_call: &gql::normalized_ast::FieldCall<GDS>,
    session: &Session,
) -> Result<root_field::QueryRootField<'n, 's>, error::Error> {
    let name = field_call
        .expected_argument(&lang_graphql::mk_name!("name"))?
        .value
        .as_string()?;
    let type_name = mk_typename(name).map_err(|_e| error::Error::TypeFieldInvalidGraphQlName {
        name: name.to_string(),
    })?;
    Ok(root_field::QueryRootField::TypeField {
        role: session.role.clone(),
        selection_set,
        schema,
        type_name,
    })
}

#[allow(clippy::too_many_arguments)]
pub fn generate_model_rootfield_ir<'n, 's>(
    type_name: &ast::TypeName,
    model: &'s metadata_resolve::ModelWithPermissions,
    models: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    object_types: &'s BTreeMap<
        metadata_resolve::Qualified<open_dds::types::CustomTypeName>,
        metadata_resolve::ObjectTypeWithRelationships,
    >,
    kind: &RootFieldKind,
    field: &'n gql::normalized_ast::Field<'s, GDS>,
    field_call: &'s gql::normalized_ast::FieldCall<'s, GDS>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    model_name: &'s metadata_resolve::Qualified<models::ModelName>,
) -> Result<root_field::QueryRootField<'n, 's>, error::Error> {
    let source = model.model.source.as_deref().ok_or_else(|| {
        error::InternalDeveloperError::NoSourceDataConnector {
            type_name: type_name.clone(),
            field_name: field_call.name.clone(),
        }
    })?;
    let ir = match kind {
        RootFieldKind::SelectOne => root_field::QueryRootField::ModelSelectOne {
            selection_set: &field.selection_set,
            ir: select_one::select_one_generate_ir(
                field,
                field_call,
                source,
                models,
                object_types,
                session,
                request_headers,
                model_name,
            )?,
        },
        RootFieldKind::SelectMany => root_field::QueryRootField::ModelSelectMany {
            selection_set: &field.selection_set,
            ir: select_many::select_many_generate_ir(
                field,
                field_call,
                source,
                models,
                object_types,
                session,
                request_headers,
                model_name,
            )?,
        },
        RootFieldKind::SelectAggregate => root_field::QueryRootField::ModelSelectAggregate {
            selection_set: &field.selection_set,
            ir: select_aggregate::select_aggregate_generate_ir(
                field, field_call, source, model_name,
            )?,
        },
    };
    Ok(ir)
}

fn generate_command_rootfield_ir<'n, 's>(
    name: &'s metadata_resolve::Qualified<CommandName>,
    type_name: &ast::TypeName,
    function_name: Option<&'s open_dds::commands::FunctionName>,
    command: &'s metadata_resolve::CommandWithPermissions,
    result_type: &'s metadata_resolve::QualifiedTypeReference,
    result_base_type_kind: &'s TypeKind,
    field: &'n gql::normalized_ast::Field<'s, GDS>,
    field_call: &'s gql::normalized_ast::FieldCall<'s, GDS>,
    models: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    object_types: &'s BTreeMap<
        metadata_resolve::Qualified<open_dds::types::CustomTypeName>,
        metadata_resolve::ObjectTypeWithRelationships,
    >,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
) -> Result<root_field::QueryRootField<'n, 's>, error::Error> {
    let mut usage_counts = UsagesCounts::new();
    let source = command.command.source.as_deref().ok_or_else(|| {
        error::InternalDeveloperError::NoSourceDataConnector {
            type_name: type_name.clone(),
            field_name: field_call.name.clone(),
        }
    })?;

    let function_name = function_name.as_ref().ok_or_else(|| {
        error::InternalDeveloperError::NoFunctionOrProcedure {
            type_name: type_name.clone(),
            field_name: field_call.name.clone(),
        }
    })?;

    let ir = root_field::QueryRootField::FunctionBasedCommand {
        selection_set: &field.selection_set,
        ir: commands::generate_function_based_command_open_dd(
            models,
            object_types,
            name,
            function_name,
            field,
            field_call,
            result_type,
            *result_base_type_kind,
            source,
            &session.variables,
            request_headers,
            &mut usage_counts,
        )?,
    };
    Ok(ir)
}

fn generate_nodefield_ir<'n, 's>(
    field: &'n gql::normalized_ast::Field<'s, GDS>,
    field_call: &'n gql::normalized_ast::FieldCall<'s, GDS>,
    typename_mappings: &'s HashMap<ast::TypeName, NodeFieldTypeNameMapping>,
    models: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    object_types: &'s BTreeMap<
        metadata_resolve::Qualified<open_dds::types::CustomTypeName>,
        metadata_resolve::ObjectTypeWithRelationships,
    >,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
) -> Result<root_field::QueryRootField<'n, 's>, error::Error> {
    let ir = root_field::QueryRootField::NodeSelect(node_field::relay_node_ir(
        field,
        field_call,
        typename_mappings,
        models,
        object_types,
        session,
        request_headers,
    )?);
    Ok(ir)
}

fn generate_entities_ir<'n, 's>(
    field: &'n gql::normalized_ast::Field<'s, GDS>,
    field_call: &'n gql::normalized_ast::FieldCall<'s, GDS>,
    typename_mappings: &'s HashMap<ast::TypeName, EntityFieldTypeNameMapping>,
    models: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    object_types: &'s BTreeMap<
        metadata_resolve::Qualified<open_dds::types::CustomTypeName>,
        metadata_resolve::ObjectTypeWithRelationships,
    >,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
) -> Result<root_field::QueryRootField<'n, 's>, error::Error> {
    let ir = root_field::QueryRootField::ApolloFederation(
        root_field::ApolloFederationRootFields::EntitiesSelect(apollo_federation::entities_ir(
            field,
            field_call,
            typename_mappings,
            models,
            object_types,
            session,
            request_headers,
        )?),
    );
    Ok(ir)
}
