//! model_source IR for 'select_one' operation
//!
//! A 'select_one' operation fetches zero or one row from a model

/// Generates the IR for a 'select_one' operation
// TODO: Remove once TypeMapping has more than one variant
use hasura_authn_core::SessionVariables;
use lang_graphql::{ast::common as ast, normalized_ast};
use ndc_client as ndc;
use open_dds;
use serde::Serialize;

use super::error;
use crate::execute::ir::arguments;
use crate::execute::ir::model_selection;
use crate::execute::ir::permissions;
use crate::execute::model_tracking::{count_model, UsagesCounts};
use crate::metadata::resolved;
use crate::metadata::resolved::subgraph::Qualified;
use crate::schema::types::{self, Annotation, ModelInputAnnotation};
use crate::schema::GDS;

/// IR for the 'select_one' operation on a model
#[derive(Serialize, Debug)]
pub struct ModelSelectOne<'n, 's> {
    // The name of the field as published in the schema
    pub field_name: ast::Name,

    pub model_selection: model_selection::ModelSelection<'s>,

    // The Graphql output type of the operation
    pub(crate) type_container: &'n ast::TypeContainer<ast::TypeName>,

    // All the models/commands used in this operation. This includes the models/commands
    // used via relationships. And in future, the models/commands used in the filter clause
    pub(crate) usage_counts: UsagesCounts,
}

#[allow(irrefutable_let_patterns)]
pub(crate) fn select_one_generate_ir<'n, 's>(
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'s normalized_ast::FieldCall<'s, GDS>,
    data_type: &Qualified<open_dds::types::CustomTypeName>,
    model_source: &'s resolved::model::ModelSource,
    session_variables: &SessionVariables,
    model_name: &'s Qualified<open_dds::models::ModelName>,
) -> Result<ModelSelectOne<'n, 's>, error::Error> {
    let field_mappings = model_source
        .type_mappings
        .get(data_type)
        .and_then(|type_mapping| {
            if let resolved::types::TypeMapping::Object { field_mappings } = type_mapping {
                Some(field_mappings)
            } else {
                None
            }
        })
        .ok_or_else(|| error::InternalEngineError::InternalGeneric {
            description: format!("type '{:}' not found in source type_mappings", data_type),
        })?;

    let mut filter_clause = vec![];
    let mut model_argument_fields = Vec::new();
    for argument in field_call.arguments.values() {
        match argument.info.generic {
            annotation @ Annotation::Input(types::InputAnnotation::Model(
                model_input_argument_annotation,
            )) => match model_input_argument_annotation {
                ModelInputAnnotation::ModelArgument { .. } => {
                    model_argument_fields.push(argument);
                }
                ModelInputAnnotation::ModelUniqueIdentifierArgument { field_name } => {
                    let field_mapping = &field_mappings.get(field_name).ok_or_else(|| {
                        error::InternalEngineError::InternalGeneric {
                            description: format!(
                                "invalid unique identifier field in annotation: {field_name:}"
                            ),
                        }
                    })?;
                    let ndc_expression = ndc::models::Expression::BinaryComparisonOperator {
                        column: ndc::models::ComparisonTarget::Column {
                            name: field_mapping.column.clone(),
                            path: vec![],
                        },
                        operator: ndc::models::BinaryComparisonOperator::Equal,
                        value: ndc::models::ComparisonValue::Scalar {
                            value: argument.value.as_json(),
                        },
                    };
                    filter_clause.push(ndc_expression);
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
    let model_arguments = arguments::build_ndc_model_arguments(
        &field_call.name,
        model_argument_fields.into_iter(),
        &model_source.type_mappings,
    )?;

    // Add the name of the root model
    let mut usage_counts = UsagesCounts::new();
    count_model(model_name.clone(), &mut usage_counts);

    let model_selection = model_selection::model_selection_ir(
        &field.selection_set,
        data_type,
        model_source,
        model_arguments,
        filter_clause,
        permissions::get_select_filter_predicate(field_call)?,
        None, // limit
        None, // offset
        None, // order_by
        session_variables,
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
