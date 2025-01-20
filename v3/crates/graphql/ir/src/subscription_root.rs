//! IR of the subscription root type

use std::collections::BTreeMap;

use hasura_authn_core::Session;
use indexmap::IndexMap;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use open_dds::{models, types::CustomTypeName};

use super::error;
use super::query_root::{select_aggregate, select_many, select_one};
use super::root_field;
use crate::GraphqlRequestPipeline;
use graphql_schema::RootFieldKind;
use graphql_schema::GDS;
use graphql_schema::{Annotation, NamespaceAnnotation, OutputAnnotation, RootFieldAnnotation};

pub fn generate_ir<'n, 's>(
    request_pipeline: GraphqlRequestPipeline,
    session: &Session,
    metadata: &'s metadata_resolve::Metadata,
    request_headers: &reqwest::header::HeaderMap,
    selection_set: &'s gql::normalized_ast::SelectionSet<'s, GDS>,
) -> Result<(ast::Alias, root_field::SubscriptionRootField<'n, 's>), error::Error> {
    let type_name = selection_set
        .type_name
        .clone()
        .ok_or_else(|| gql::normalized_ast::Error::NoTypenameFound)?;
    let mut field_iter = selection_set.fields.iter();
    match (field_iter.next(), field_iter.next()) {
        (Some((alias, field)), None) => {
            let field_call = field.field_call()?;
            match field_call.info.generic {
                annotation @ Annotation::Output(OutputAnnotation::RootField(root_field)) => {
                    match root_field {
                        RootFieldAnnotation::ModelSubscription {
                            data_type,
                            kind,
                            name: model_name,
                            polling_interval_ms,
                        } => {
                            let model = metadata.models.get(model_name).ok_or_else(|| {
                                error::InternalEngineError::InternalGeneric {
                                    description: format!("Model {model_name} not found"),
                                }
                            })?;
                            let ir = generate_model_rootfield_ir(
                                request_pipeline,
                                &type_name,
                                model,
                                &metadata.models,
                                &metadata.commands,
                                &metadata.object_types,
                                data_type,
                                kind,
                                field,
                                field_call,
                                session,
                                request_headers,
                                model_name,
                                polling_interval_ms,
                            )?;
                            Ok((alias.clone(), ir))
                        }
                        _ => Err(error::Error::from(
                            error::InternalEngineError::UnexpectedAnnotation {
                                annotation: annotation.clone(),
                            },
                        )),
                    }
                }
                annotation => Err(error::Error::from(
                    error::InternalEngineError::UnexpectedAnnotation {
                        annotation: annotation.clone(),
                    },
                )),
            }
        }
        _ => Err(error::Error::NoneOrMoreSubscriptionRootFields),
    }
}

#[allow(clippy::too_many_arguments)]
fn generate_model_rootfield_ir<'n, 's>(
    request_pipeline: GraphqlRequestPipeline,
    type_name: &ast::TypeName,
    model: &'s metadata_resolve::ModelWithPermissions,
    models: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    commands: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::commands::CommandName>,
        metadata_resolve::CommandWithPermissions,
    >,
    object_types: &'s BTreeMap<
        metadata_resolve::Qualified<open_dds::types::CustomTypeName>,
        metadata_resolve::ObjectTypeWithRelationships,
    >,
    data_type: &metadata_resolve::Qualified<CustomTypeName>,
    kind: &RootFieldKind,
    field: &'n gql::normalized_ast::Field<'s, GDS>,
    field_call: &'s gql::normalized_ast::FieldCall<'s, GDS>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    model_name: &'s metadata_resolve::Qualified<models::ModelName>,
    polling_interval_ms: &u64,
) -> Result<root_field::SubscriptionRootField<'n, 's>, error::Error> {
    let source = model.model.source.as_deref().ok_or_else(|| {
        error::InternalDeveloperError::NoSourceDataConnector {
            type_name: type_name.clone(),
            field_name: field_call.name.clone(),
        }
    })?;
    // Check if subscription is allowed
    // We won't be generating graphql schema any way if subscription is not allowed in permission.
    // This is just a double check, if in case we missed something.
    if let Some(NamespaceAnnotation::Model {
        allow_subscriptions,
        ..
    }) = field_call.info.namespaced
    {
        if !allow_subscriptions {
            Err(error::InternalEngineError::SubscriptionNotAllowed)?;
        }
    }
    let ir = match kind {
        RootFieldKind::SelectOne => root_field::SubscriptionRootField::ModelSelectOne {
            selection_set: &field.selection_set,
            ir: select_one::select_one_generate_ir(
                request_pipeline,
                field,
                field_call,
                data_type,
                model,
                source,
                models,
                commands,
                object_types,
                session,
                request_headers,
                model_name,
            )?,
            polling_interval_ms: *polling_interval_ms,
        },
        RootFieldKind::SelectMany => root_field::SubscriptionRootField::ModelSelectMany {
            selection_set: &field.selection_set,
            ir: select_many::select_many_generate_ir(
                request_pipeline,
                field,
                field_call,
                data_type,
                model,
                source,
                models,
                commands,
                object_types,
                session,
                request_headers,
                model_name,
            )?,
            polling_interval_ms: *polling_interval_ms,
        },
        RootFieldKind::SelectAggregate => root_field::SubscriptionRootField::ModelSelectAggregate {
            selection_set: &field.selection_set,
            ir: select_aggregate::select_aggregate_generate_ir(
                request_pipeline,
                field,
                field_call,
                data_type,
                model,
                source,
                object_types,
                session,
                request_headers,
                model_name,
            )?,
            polling_interval_ms: *polling_interval_ms,
        },
    };
    Ok(ir)
}
