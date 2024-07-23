//! model_source IR for 'select_one' operation
//!
//! A 'select_one' operation fetches zero or one row from a model

use crate::ir::arguments;
use crate::ir::error;
use crate::ir::filter;
use crate::ir::filter::expression as filter_expression;
use crate::ir::model_selection;
use crate::ir::permissions;
use crate::model_tracking::{count_model, UsagesCounts};
/// Generates the IR for a 'select_one' operation
// TODO: Remove once TypeMapping has more than one variant
use hasura_authn_core::SessionVariables;
use lang_graphql::{ast::common as ast, normalized_ast};
use metadata_resolve;
use metadata_resolve::Qualified;
use open_dds;
use serde::Serialize;
use std::collections::BTreeMap;

use schema::GDS;
use schema::{self, Annotation, ModelInputAnnotation};

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
    model_source: &'s metadata_resolve::ModelSource,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    model_name: &'s Qualified<open_dds::models::ModelName>,
) -> Result<ModelSelectOne<'n, 's>, error::Error> {
    let mut filter_expressions = vec![];
    let mut model_argument_fields = Vec::new();
    for argument in field_call.arguments.values() {
        match argument.info.generic {
            annotation @ Annotation::Input(schema::InputAnnotation::Model(
                model_input_argument_annotation,
            )) => match model_input_argument_annotation {
                ModelInputAnnotation::ModelArgument { .. } => {
                    model_argument_fields.push(argument);
                }
                ModelInputAnnotation::ModelUniqueIdentifierArgument { ndc_column } => {
                    let ndc_column = ndc_column.as_ref().ok_or_else(|| error::InternalEngineError::InternalGeneric {
                        description: format!("Missing NDC column mapping for unique identifier argument {} on field {}", argument.name, field_call.name)})?;
                    let filter_expression =
                        filter_expression::LocalFieldComparison::BinaryComparison {
                            column: ndc_models::ComparisonTarget::Column {
                                name: ndc_models::FieldName::from(ndc_column.column.as_str()),
                                field_path: None,
                            },
                            operator: ndc_models::ComparisonOperatorName::from(
                                ndc_column.equal_operator.as_str(),
                            ),
                            value: ndc_models::ComparisonValue::Scalar {
                                value: argument.value.as_json(),
                            },
                        };
                    filter_expressions
                        .push(filter_expression::Expression::LocalField(filter_expression));
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

    let mut model_arguments = BTreeMap::new();

    for argument in model_argument_fields {
        let (ndc_arg_name, ndc_val) = arguments::build_ndc_argument_as_value(
            &field_call.name,
            argument,
            &model_source.type_mappings,
            &model_source.data_connector,
            &mut usage_counts,
        )?;

        model_arguments.insert(ndc_arg_name, ndc_val);
    }

    if let Some(argument_presets) = permissions::get_argument_presets(field_call.info.namespaced)? {
        // add any preset arguments from model permissions
        arguments::process_model_arguments_presets(
            argument_presets,
            session_variables,
            &mut model_arguments,
            &mut usage_counts,
        )?;
    }

    let query_filter = filter::QueryFilter {
        where_clause: None,
        additional_filter: Some(filter_expression::Expression::mk_and(filter_expressions)),
    };

    let model_selection = model_selection::model_selection_ir(
        &field.selection_set,
        data_type,
        model_source,
        model_arguments,
        query_filter,
        permissions::get_select_filter_predicate(field_call)?,
        None, // limit
        None, // offset
        None, // order_by
        session_variables,
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
