//! IR of the mutation root type

use hasura_authn_core::SessionVariables;
use indexmap::IndexMap;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use tracing_util::SpanVisibility;

use super::{commands, root_field};
use crate::execute::error;
use crate::schema::types::{Annotation, OutputAnnotation, RootFieldAnnotation};
use crate::schema::GDS;

/// Generates IR for the selection set of type 'mutation root'
pub fn generate_ir<'n, 's>(
    selection_set: &'s gql::normalized_ast::SelectionSet<'s, GDS>,
    session_variables: &SessionVariables,
) -> Result<IndexMap<ast::Alias, root_field::RootField<'n, 's>>, error::Error> {
    let tracer = tracing_util::global_tracer();
    tracer.in_span("generate_ir", SpanVisibility::Internal, || {
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
                        RootFieldAnnotation::Command {
                            name,
                            underlying_object_typename,
                            source,
                        },
                    )) => {
                        let source = source.as_ref().ok_or_else(|| {
                            error::InternalDeveloperError::NoSourceDataConnector {
                                type_name: type_name.clone(),
                                field_name: field_call.name.clone(),
                            }
                        })?;
                        Ok(root_field::MutationRootField::CommandRepresentation {
                            selection_set: &field.selection_set,
                            ir: commands::command_generate_ir(
                                name,
                                field,
                                field_call,
                                underlying_object_typename,
                                source,
                                session_variables,
                            )?,
                        })
                    }
                    annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                        annotation: annotation.clone(),
                    }),
                },
            }?;
            root_fields.insert(
                alias.clone(),
                root_field::RootField::MutationRootField(field_response),
            );
        }
        Ok(root_fields)
    })
}
