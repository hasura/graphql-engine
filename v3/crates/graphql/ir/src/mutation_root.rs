//! IR of the mutation root type

use hasura_authn_core::Session;
use indexmap::IndexMap;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use tracing_util::SpanVisibility;

use graphql_schema::Annotation;
use graphql_schema::GDS;

use super::{commands, root_field};
use crate::error;
use crate::flags::GraphqlIrFlags;
use graphql_schema::{OutputAnnotation, RootFieldAnnotation};

/// Generates IR for the selection set of type 'mutation root'
pub fn generate_ir<'n, 's>(
    selection_set: &'s gql::normalized_ast::SelectionSet<'s, GDS>,
    metadata: &'s metadata_resolve::Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
) -> Result<IndexMap<ast::Alias, root_field::MutationRootField<'n, 's>>, error::Error> {
    let tracer = tracing_util::global_tracer();
    tracer.in_span(
        "generate_ir",
        "Generate IR for request",
        SpanVisibility::Internal,
        || {
            let type_name = selection_set
                .type_name
                .clone()
                .ok_or_else(|| gql::normalized_ast::Error::NoTypenameFound)?;
            let mut root_fields = IndexMap::new();
            for (alias, field) in &selection_set.fields {
                let field_call = field.field_call()?;
                let field_response = match field_call.name.as_str() {
                    "__typename" => Ok(root_field::MutationRootField::TypeName {
                        type_name: type_name.clone(),
                    }),
                    _ => match field_call.info.generic {
                        Annotation::Output(OutputAnnotation::RootField(
                            RootFieldAnnotation::ProcedureCommand {
                                name,
                                procedure_name,
                                result_type,
                                result_base_type_kind,
                            },
                        )) => {
                            let command = metadata.commands.get(name).ok_or_else(|| {
                                error::InternalEngineError::InternalGeneric {
                                    description: format!("Command {name} not found"),
                                }
                            })?;
                            let source = command.command.source.as_deref().ok_or_else(|| {
                                error::InternalDeveloperError::NoSourceDataConnector {
                                    type_name: type_name.clone(),
                                    field_name: field_call.name.clone(),
                                }
                            })?;

                            let procedure_name = procedure_name.as_ref().ok_or_else(|| {
                                error::InternalDeveloperError::NoFunctionOrProcedure {
                                    type_name: type_name.clone(),
                                    field_name: field_call.name.clone(),
                                }
                            })?;

                            Ok(root_field::MutationRootField::ProcedureBasedCommand {
                                selection_set: &field.selection_set,
                                ir: Box::new(commands::generate_procedure_based_command_open_dd(
                                    &metadata.models,
                                    &metadata.object_types,
                                    name,
                                    procedure_name,
                                    field,
                                    field_call,
                                    result_type,
                                    *result_base_type_kind,
                                    source,
                                    &session.variables,
                                    request_headers,
                                    &GraphqlIrFlags::from_runtime_flags(&metadata.runtime_flags),
                                )?),
                            })
                        }
                        annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                            annotation: annotation.clone(),
                        }),
                    },
                }?;
                root_fields.insert(alias.clone(), field_response);
            }
            Ok(root_fields)
        },
    )
}
