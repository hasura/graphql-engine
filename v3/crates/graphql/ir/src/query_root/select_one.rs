//! model_source IR for 'select_one' operation
//!
//! A 'select_one' operation fetches zero or one row from a model

use crate::arguments;
use crate::error;
use crate::filter;
use crate::model_selection;
use crate::model_tracking::count_model;
use crate::permissions;
/// Generates the IR for a 'select_one' operation
// TODO: Remove once TypeMapping has more than one variant
use hasura_authn_core::SessionVariables;
use lang_graphql::{ast::common as ast, normalized_ast};
use metadata_resolve;
use metadata_resolve::Qualified;
use open_dds;
use plan_types::{
    ComparisonTarget, ComparisonValue, Expression, LocalFieldComparison, UsagesCounts,
};
use serde::Serialize;
use std::collections::BTreeMap;

use graphql_schema::GDS;
use graphql_schema::{self, Annotation, ModelInputAnnotation};

/// IR for the 'select_one' operation on a model
#[derive(Serialize, Debug)]
pub struct ModelSelectOne<'n, 's> {
    // The name of the field as published in the schema
    pub field_name: ast::Name,

    pub model_selection: model_selection::ModelSelection<'s>,

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
            annotation @ Annotation::Input(graphql_schema::InputAnnotation::Model(
                model_input_argument_annotation,
            )) => match model_input_argument_annotation {
                ModelInputAnnotation::ModelArgument { .. } => {
                    model_argument_fields.push(argument);
                }
                ModelInputAnnotation::ModelUniqueIdentifierArgument { ndc_column } => {
                    let ndc_column = ndc_column.as_ref().ok_or_else(|| error::InternalEngineError::InternalGeneric {
                        description: format!("Missing NDC column mapping for unique identifier argument {} on field {}", argument.name, field_call.name)})?;
                    let filter_expression = LocalFieldComparison::BinaryComparison {
                        column: ComparisonTarget::Column {
                            name: ndc_column.column.clone(),
                            field_path: vec![],
                        },
                        operator: ndc_column.equal_operator.clone(),
                        value: ComparisonValue::Scalar {
                            value: argument.value.as_json(),
                        },
                    };
                    filter_expressions.push(Expression::LocalField(filter_expression));
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
            session_variables,
            &mut usage_counts,
        )?;

        model_arguments.insert(ndc_arg_name, ndc_val);
    }

    let argument_presets = permissions::get_argument_presets(field_call.info.namespaced.as_ref())?;
    // add any preset arguments from model permissions
    model_arguments = arguments::process_argument_presets(
        &model_source.data_connector,
        &model_source.type_mappings,
        argument_presets,
        &model_source.data_connector_link_argument_presets,
        session_variables,
        request_headers,
        model_arguments,
        &mut usage_counts,
    )?;

    let query_filter = filter::QueryFilter {
        where_clause: None,
        additional_filter: Some(Expression::mk_and(filter_expressions)),
    };

    let model_selection = model_selection::model_selection_ir(
        &field.selection_set,
        data_type,
        model_source,
        model_arguments,
        query_filter,
        permissions::get_select_filter_predicate(&field_call.info)?,
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
