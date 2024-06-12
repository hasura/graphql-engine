use std::collections::BTreeMap;

use indexmap::IndexMap;
use lang_graphql::{ast::common::Alias, normalized_ast};
use metadata_resolve::{Qualified, QualifiedTypeName, TypeMapping};
use open_dds::{
    aggregates::DataConnectorAggregationFunctionName,
    data_connector::DataConnectorName,
    types::{CustomTypeName, FieldName},
};
use schema::{
    AggregateOutputAnnotation, AggregationFunctionAnnotation, Annotation, OutputAnnotation, GDS,
};
use serde::Serialize;

use crate::ir::error;

/// IR that represents the selected fields of an output type.
#[derive(Debug, Serialize, Default, PartialEq)]
pub struct AggregateSelectionSet<'s> {
    // The fields in the selection set. They are stored in the form that would
    // be converted and sent over the wire. Serialized the map as ordered to
    // produce deterministic golden files.
    pub fields: IndexMap<String, AggregateFieldSelection<'s>>,
}

#[derive(Debug, Serialize, PartialEq)]
pub enum AggregateFieldSelection<'s> {
    Count {
        column_path: Vec<&'s str>,
        graphql_field_path: Vec<Alias>,
    },
    CountDistinct {
        column_path: Vec<&'s str>,
        graphql_field_path: Vec<Alias>,
    },
    AggregationFunction {
        function_name: &'s DataConnectorAggregationFunctionName,
        column_path: nonempty::NonEmpty<&'s str>,
        graphql_field_path: Vec<Alias>,
    },
}

impl<'s> AggregateFieldSelection<'s> {
    pub fn get_graphql_field_path(&self) -> &Vec<Alias> {
        match self {
            AggregateFieldSelection::Count {
                graphql_field_path, ..
            }
            | AggregateFieldSelection::CountDistinct {
                graphql_field_path, ..
            }
            | AggregateFieldSelection::AggregationFunction {
                graphql_field_path, ..
            } => graphql_field_path,
        }
    }
}

pub fn generate_aggregate_selection_set_ir<'s>(
    selection_set: &normalized_ast::SelectionSet<'s, GDS>,
    data_connector: &'s metadata_resolve::DataConnectorLink,
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    field_mappings: &'s BTreeMap<FieldName, metadata_resolve::FieldMapping>,
    aggregate_operand_type: &QualifiedTypeName,
) -> Result<AggregateSelectionSet<'s>, error::Error> {
    let mut aggregate_field_selections = IndexMap::new();

    add_aggregate_selections(
        &mut aggregate_field_selections,
        selection_set,
        aggregate_operand_type,
        &data_connector.name,
        &[], // column_path
        &[], // graphql_field_path
        type_mappings,
        Some(field_mappings),
    )?;

    Ok(AggregateSelectionSet {
        fields: aggregate_field_selections,
    })
}

fn add_aggregate_selections<'s>(
    aggregate_field_selections: &mut IndexMap<String, AggregateFieldSelection<'s>>,
    selection_set: &normalized_ast::SelectionSet<'s, GDS>,
    aggregate_operand_type: &QualifiedTypeName,
    data_connector_name: &Qualified<DataConnectorName>,
    column_path: &[&'s metadata_resolve::FieldMapping],
    graphql_field_path: &[Alias],
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    field_mappings: Option<&'s BTreeMap<FieldName, metadata_resolve::FieldMapping>>,
) -> Result<(), error::Error> {
    for field in selection_set.fields.values() {
        let graphql_field_path = graphql_field_path
            .iter()
            .chain(std::iter::once(&field.alias))
            .cloned()
            .collect::<Vec<Alias>>();

        let field_call = field.field_call()?;
        match field_call.info.generic {
            Annotation::Output(OutputAnnotation::Aggregate(
                AggregateOutputAnnotation::AggregationFunctionField(aggregate_function),
            )) => match aggregate_function {
                AggregationFunctionAnnotation::Count => {
                    let selection_field_name =
                        mk_alias_from_graphql_field_path(&graphql_field_path);
                    let selection = AggregateFieldSelection::Count {
                        column_path: column_path.iter().map(|m| m.column.0.as_str()).collect(),
                        graphql_field_path,
                    };
                    aggregate_field_selections.insert(selection_field_name, selection);
                }

                AggregationFunctionAnnotation::CountDistinct => {
                    let selection_field_name =
                        mk_alias_from_graphql_field_path(&graphql_field_path);
                    let selection = AggregateFieldSelection::CountDistinct {
                        column_path: column_path.iter().map(|m| m.column.0.as_str()).collect(),
                        graphql_field_path,
                    };
                    aggregate_field_selections.insert(selection_field_name, selection);
                }

                AggregationFunctionAnnotation::Function {
                    function_name,
                    data_connector_functions,
                } => {
                    let selection_field_name =
                        mk_alias_from_graphql_field_path(&graphql_field_path);

                    let column_path = nonempty::NonEmpty::from_slice(column_path)
                        .ok_or_else(|| error::InternalDeveloperError::ColumnAggregationFunctionUsedOnModelObjectType {
                            aggregate_operand_type: aggregate_operand_type.clone(),
                            aggregation_function: function_name.clone(),
                        })?;

                    let column_scalar_type =
                        get_ndc_underlying_type_name(&column_path.last().column_type);

                    let data_connector_function_info = data_connector_functions
                        .iter()
                        .find(|fn_info| {
                            fn_info.data_connector_name == *data_connector_name &&
                            fn_info.operand_scalar_type.0 == *column_scalar_type
                        })
                        .ok_or_else(|| {
                            error::InternalDeveloperError::DataConnectorAggregationFunctionNotFound {
                                aggregate_operand_type: aggregate_operand_type.clone(),
                                aggregation_function: function_name.clone(),
                                data_connector_name: data_connector_name.clone(),
                            }
                        })?;

                    let selection = AggregateFieldSelection::AggregationFunction {
                        function_name: &data_connector_function_info.function_name,
                        column_path: column_path.map(|m| m.column.0.as_str()),
                        graphql_field_path,
                    };
                    aggregate_field_selections.insert(selection_field_name, selection);
                }
            },

            Annotation::Output(OutputAnnotation::Aggregate(
                AggregateOutputAnnotation::AggregatableField {
                    field_name,
                    aggregate_operand_type: field_aggregate_operand_type,
                },
            )) => {
                let field_mapping = field_mappings
                    .ok_or_else(|| {
                        error::InternalDeveloperError::AggregatableFieldFoundOnScalarTypedOperand {
                            field_name: field_name.clone(),
                            aggregate_operand_type: aggregate_operand_type.clone(),
                        }
                    })?
                    .get(field_name)
                    .ok_or_else(|| error::InternalEngineError::InternalGeneric {
                        description: format!("invalid field in annotation: {field_name}"),
                    })?;
                let column_path = column_path
                    .iter()
                    .copied() // This just dereferences the double reference: &&FieldMapping -> &FieldMapping
                    .chain(std::iter::once(field_mapping))
                    .collect::<Vec<&metadata_resolve::FieldMapping>>();

                // If the type name is not in the object type mappings or is inbuilt, it is a scalar type
                // and therefore does not have field mappings
                let field_operand_field_mappings = match field_aggregate_operand_type {
                    QualifiedTypeName::Custom(custom_type_name) => {
                        type_mappings.get(custom_type_name).map(|type_mapping| {
                            let metadata_resolve::TypeMapping::Object { field_mappings, .. } =
                                type_mapping;
                            field_mappings
                        })
                    }
                    QualifiedTypeName::Inbuilt(_) => None,
                };

                add_aggregate_selections(
                    aggregate_field_selections,
                    &field.selection_set,
                    aggregate_operand_type,
                    data_connector_name,
                    &column_path,
                    &graphql_field_path,
                    type_mappings,
                    field_operand_field_mappings,
                )?;
            }
            annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                annotation: annotation.clone(),
            })?,
        }
    }

    Ok(())
}

fn mk_alias_from_graphql_field_path(graphql_field_path: &[Alias]) -> String {
    graphql_field_path
        .iter()
        .map(|alias| alias.0.as_str())
        .collect::<Vec<_>>()
        .join("_")
}

fn get_ndc_underlying_type_name(result_type: &ndc_models::Type) -> &String {
    match result_type {
        ndc_models::Type::Named { name } => name,
        ndc_models::Type::Array { element_type } => get_ndc_underlying_type_name(element_type),
        ndc_models::Type::Nullable { underlying_type } => {
            get_ndc_underlying_type_name(underlying_type)
        }
        ndc_models::Type::Predicate { object_type_name } => object_type_name,
    }
}
