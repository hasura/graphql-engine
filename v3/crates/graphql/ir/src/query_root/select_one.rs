//! model_source IR for 'select_one' operation
//!
//! A 'select_one' operation fetches zero or one row from a model

use crate::arguments;
use crate::error;
use crate::model_selection;
/// Generates the IR for a 'select_one' operation
// TODO: Remove once TypeMapping has more than one variant
use hasura_authn_core::Session;
use indexmap::IndexMap;
use lang_graphql::{ast::common as ast, normalized_ast};
use metadata_resolve;
use metadata_resolve::Qualified;
use open_dds;
use plan::count_model;
use plan_types::UsagesCounts;
use serde::Serialize;
use std::collections::BTreeMap;

use graphql_schema::GDS;
use graphql_schema::{self, Annotation, ModelInputAnnotation};

/// IR for the 'select_one' operation on a model
#[derive(Serialize, Debug)]
pub struct ModelSelectOne<'n> {
    // The name of the field as published in the schema
    pub field_name: ast::Name,

    pub model_selection: open_dds::query::ModelSelection,

    // The Graphql output type of the operation
    pub type_container: &'n ast::TypeContainer<ast::TypeName>,

    // All the models/commands used in this operation. This includes the models/commands
    // used via relationships. And in future, the models/commands used in the filter clause
    pub usage_counts: UsagesCounts,
}

#[allow(irrefutable_let_patterns)]
pub fn select_one_generate_ir<'n, 's>(
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'s normalized_ast::FieldCall<'s, GDS>,
    model_source: &'s metadata_resolve::ModelSource,
    models: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    object_types: &'s BTreeMap<
        Qualified<open_dds::types::CustomTypeName>,
        metadata_resolve::ObjectTypeWithRelationships,
    >,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    model_name: &'s Qualified<open_dds::models::ModelName>,
) -> Result<ModelSelectOne<'n>, error::Error> {
    let mut unique_identifier_arguments = vec![];
    let mut model_argument_fields = Vec::new();

    for argument in field_call.arguments.values() {
        match argument.info.generic {
            annotation @ Annotation::Input(graphql_schema::InputAnnotation::Model(
                model_input_argument_annotation,
            )) => match model_input_argument_annotation {
                ModelInputAnnotation::ModelArgument { .. } => {
                    model_argument_fields.push(argument);
                }
                ModelInputAnnotation::ModelUniqueIdentifierArgument {
                    field_name,
                    ndc_column,
                } => {
                    let ndc_column = ndc_column.as_ref().ok_or_else(|| error::InternalEngineError::InternalGeneric {
                        description: format!("Missing NDC column mapping for unique identifier argument {} on field {}", argument.name, field_call.name)})?;
                    unique_identifier_arguments.push((
                        ndc_column,
                        field_name,
                        argument.value.as_json(),
                    ));
                }
                _ => Err(error::InternalEngineError::UnexpectedAnnotation {
                    annotation: annotation.clone(),
                })?,
            },
            annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                annotation: annotation.clone(),
            })?,
        }
    }
    // Add the name of the root model
    let mut usage_counts = UsagesCounts::new();
    count_model(model_name, &mut usage_counts);

    let filter_expressions: Vec<_> = unique_identifier_arguments
        .into_iter()
        .map(|(_ndc_column, field_name, argument_value)| {
            open_dds::query::BooleanExpression::Comparison {
                operand: open_dds::query::Operand::Field(open_dds::query::ObjectFieldOperand {
                    target: Box::new(open_dds::query::ObjectFieldTarget {
                        arguments: IndexMap::new(),
                        field_name: field_name.clone(),
                    }),
                    nested: None,
                }),
                operator: open_dds::query::ComparisonOperator::Equals,
                argument: Box::new(open_dds::query::Value::Literal(argument_value)),
            }
        })
        .collect();

    let mut where_clause = None;
    if !filter_expressions.is_empty() {
        where_clause = Some(open_dds::query::BooleanExpression::And(filter_expressions));
    }

    let model_arguments = model_argument_fields
        .iter()
        .map(|argument| {
            arguments::resolve_argument_opendd(
                argument,
                &model_source.type_mappings,
                &mut usage_counts,
            )
        })
        .collect::<Result<IndexMap<_, _>, _>>()?;

    let model_selection = model_selection::model_selection_open_dd_ir(
        &field.selection_set,
        model_name,
        models,
        &model_source.type_mappings,
        object_types,
        Some(model_arguments),
        where_clause,
        vec![], // order_by
        None,   // limit
        None,   // offset
        &session.variables,
        request_headers,
        // Get all the models/commands that were used as relationships
        &mut usage_counts,
    )?;

    Ok(ModelSelectOne {
        field_name: field_call.name.clone(),
        model_selection,
        type_container: &field.type_container,
        usage_counts,
    })
}
