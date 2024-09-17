//! IR of the subscription root type

use hasura_authn_core::Session;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use open_dds::{models, types::CustomTypeName};

use super::error;
use super::query_root::{select_aggregate, select_many, select_one};
use super::root_field;
use graphql_schema::RootFieldKind;
use graphql_schema::GDS;
use graphql_schema::{Annotation, OutputAnnotation, RootFieldAnnotation};

pub fn generate_ir<'n, 's>(
    session: &Session,
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
                        RootFieldAnnotation::Model {
                            data_type,
                            source,
                            kind,
                            name: model_name,
                        } => {
                            let ir = generate_model_rootfield_ir(
                                &type_name,
                                source,
                                data_type,
                                kind,
                                field,
                                field_call,
                                session,
                                request_headers,
                                model_name,
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
    source: &'s Option<metadata_resolve::ModelSource>,
    data_type: &metadata_resolve::Qualified<CustomTypeName>,
    kind: &RootFieldKind,
    field: &'n gql::normalized_ast::Field<'s, GDS>,
    field_call: &'s gql::normalized_ast::FieldCall<'s, GDS>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    model_name: &'s metadata_resolve::Qualified<models::ModelName>,
) -> Result<root_field::SubscriptionRootField<'n, 's>, error::Error> {
    let source =
        source
            .as_ref()
            .ok_or_else(|| error::InternalDeveloperError::NoSourceDataConnector {
                type_name: type_name.clone(),
                field_name: field_call.name.clone(),
            })?;
    let ir = match kind {
        RootFieldKind::SelectOne => root_field::SubscriptionRootField::ModelSelectOne {
            selection_set: &field.selection_set,
            ir: select_one::select_one_generate_ir(
                field,
                field_call,
                data_type,
                source,
                &session.variables,
                request_headers,
                model_name,
            )?,
        },
        RootFieldKind::SelectMany => root_field::SubscriptionRootField::ModelSelectMany {
            selection_set: &field.selection_set,
            ir: select_many::select_many_generate_ir(
                field,
                field_call,
                data_type,
                source,
                &session.variables,
                request_headers,
                model_name,
            )?,
        },
        RootFieldKind::SelectAggregate => root_field::SubscriptionRootField::ModelSelectAggregate {
            selection_set: &field.selection_set,
            ir: select_aggregate::select_aggregate_generate_ir(
                field,
                field_call,
                data_type,
                source,
                &session.variables,
                request_headers,
                model_name,
            )?,
        },
    };
    Ok(ir)
}
