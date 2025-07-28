//! IR of the subscription root type

use std::collections::BTreeMap;

use hasura_authn_core::Session;
use indexmap::IndexMap;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use open_dds::models;

use crate::flags::GraphqlIrFlags;

use super::error;
use super::query_root::{select_aggregate, select_many, select_one};
use super::root_field;
use graphql_schema::GDS;
use graphql_schema::RootFieldKind;
use graphql_schema::{Annotation, NamespaceAnnotation, OutputAnnotation, RootFieldAnnotation};

pub fn generate_ir<'n, 's>(
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
                                polling_interval_ms,
                                &GraphqlIrFlags::from_runtime_flags(&metadata.runtime_flags),
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
    polling_interval_ms: &u64,
    flags: &GraphqlIrFlags,
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
    if matches!(
        field_call.info.namespaced.as_deref(),
        Some(NamespaceAnnotation::Model {
            allow_subscriptions: false,
            ..
        })
    ) {
        Err(error::InternalEngineError::SubscriptionNotAllowed)?;
    }
    let ir = match kind {
        RootFieldKind::SelectOne => root_field::SubscriptionRootField::ModelSelectOne {
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
                flags,
            )?,
            polling_interval_ms: *polling_interval_ms,
        },
        RootFieldKind::SelectMany => root_field::SubscriptionRootField::ModelSelectMany {
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
                flags,
            )?,
            polling_interval_ms: *polling_interval_ms,
        },
        RootFieldKind::SelectAggregate => root_field::SubscriptionRootField::ModelSelectAggregate {
            selection_set: &field.selection_set,
            ir: select_aggregate::select_aggregate_generate_ir(
                field, field_call, source, model_name, flags,
            )?,
            polling_interval_ms: *polling_interval_ms,
        },
    };
    Ok(ir)
}
