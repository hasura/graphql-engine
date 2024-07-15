pub mod types;
use crate::stages::{
    boolean_expressions, data_connector_scalar_types, data_connectors, graphql_config,
    object_types, type_permissions,
};
use crate::types::error::Error;

use crate::helpers::model::resolve_ndc_type;
use crate::helpers::types::{mk_name, store_new_graphql_type};
use crate::types::subgraph::Qualified;

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use open_dds::{data_connector::DataConnectorName, types::OperatorName};
use std::collections::{BTreeMap, BTreeSet};
pub use types::{
    ObjectBooleanExpressionDataConnector, ObjectBooleanExpressionType,
    ObjectBooleanExpressionsOutput,
};

/// resolve object boolean expression types
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::ScalarTypeWithRepresentationInfoMap,
    >,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    existing_graphql_types: &BTreeSet<ast::TypeName>,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<ObjectBooleanExpressionsOutput, Error> {
    let mut object_boolean_expression_types = BTreeMap::new();
    let mut graphql_types = existing_graphql_types.clone();

    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: object_boolean_expression_type,
    } in &metadata_accessor.object_boolean_expression_types
    {
        let resolved_boolean_expression = resolve_object_boolean_expression_type(
            object_boolean_expression_type,
            subgraph,
            data_connectors,
            data_connector_scalars,
            object_types,
            &mut graphql_types,
            graphql_config,
        )?;
        if let Some(existing) = object_boolean_expression_types.insert(
            resolved_boolean_expression.name.clone(),
            resolved_boolean_expression,
        ) {
            return Err(Error::from(
                boolean_expressions::BooleanExpressionError::DuplicateObjectBooleanExpressionTypeDefinition {
                    name: existing.name,
                },
            ));
        }
    }
    Ok(ObjectBooleanExpressionsOutput {
        object_boolean_expression_types,
        graphql_types,
    })
}

/// Resolves a given object boolean expression type
pub(crate) fn resolve_object_boolean_expression_type(
    object_boolean_expression: &open_dds::types::ObjectBooleanExpressionTypeV1,
    subgraph: &str,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::ScalarTypeWithRepresentationInfoMap,
    >,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    existing_graphql_types: &mut BTreeSet<ast::TypeName>,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<ObjectBooleanExpressionType, Error> {
    // name of the boolean expression
    let qualified_name =
        Qualified::new(subgraph.to_string(), object_boolean_expression.name.clone());
    // name of the object type backing the boolean expression
    let qualified_object_type_name = Qualified::new(
        subgraph.to_string(),
        object_boolean_expression.object_type.clone(),
    );
    let object_type_representation =
        object_types.get(&qualified_object_type_name).map_err(|_| {
            Error::from(
                boolean_expressions::BooleanExpressionError::UnknownTypeInObjectBooleanExpressionType {
                    type_name: qualified_object_type_name.clone(),
                },
            )
        })?;

    let qualified_data_connector_name = Qualified::new(
        subgraph.to_string(),
        object_boolean_expression.data_connector_name.clone(),
    );

    // validate data connector name
    let data_connector_context = data_connectors
        .0
        .get(&qualified_data_connector_name)
        .ok_or_else(|| {
            Error::from(
                boolean_expressions::BooleanExpressionError::UnknownDataConnectorInObjectBooleanExpressionType {
                    data_connector: qualified_data_connector_name.clone(),
                    object_boolean_expression_type: qualified_name.clone(),
                },
            )
        })?;

    // validate data connector object type
    if !data_connector_context.schema.object_types.contains_key(
        object_boolean_expression
            .data_connector_object_type
            .as_str(),
    ) {
        return Err(Error::from(
            boolean_expressions::BooleanExpressionError::UnknownDataConnectorTypeInObjectBooleanExpressionType {
                data_connector: qualified_data_connector_name.clone(),
                object_boolean_expression_type: qualified_name.clone(),
                data_connector_object_type: object_boolean_expression
                    .data_connector_object_type
                    .clone(),
            },
        ));
    }

    object_type_representation.type_mappings
                .get(
                    &qualified_data_connector_name,
                    &object_boolean_expression.data_connector_object_type,
                )
                .ok_or_else(|| {
                    Error::from(boolean_expressions::BooleanExpressionError::NoDataConnectorTypeMappingForObjectTypeInBooleanExpression {
                        object_type: qualified_object_type_name.clone(),
                        object_boolean_expression_type: qualified_name.clone(),
                        data_connector_object_type: object_boolean_expression
                            .data_connector_object_type.clone(),
                        data_connector: qualified_data_connector_name.clone(),
                    })
                })?;

    // validate comparable fields
    for comparable_field in &object_boolean_expression.comparable_fields {
        if !object_type_representation
            .object_type
            .fields
            .contains_key(&comparable_field.field_name)
        {
            return Err(
                boolean_expressions::BooleanExpressionError::UnknownFieldInObjectBooleanExpressionType {
                    field_name: comparable_field.field_name.clone(),
                    object_boolean_expression_type: qualified_name.clone(),
                }
                .into(),
            );
        }

        // As of now, only `"enableAll": true` is allowed for field operators
        match &comparable_field.operators {
                    open_dds::models::EnableAllOrSpecific::EnableAll(true) => {}
                    _ => {
                        return Err(Error::UnsupportedFeature {
                            message: "Field level comparison operator configuration is not fully supported yet. Please use \"enableAll\":true.".to_string(),
                        })
                    }
                }
    }

    // Comparable fields should have all type fields
    if object_boolean_expression.comparable_fields.len()
        != object_type_representation.object_type.fields.len()
    {
        return Err(Error::UnsupportedFeature {
                    message: "Field level comparison operator configuration is not fully supported yet. Please add all fields in filterable_fields.".to_string(),
                });
    }

    let object_boolean_expression_type =
        Qualified::new(subgraph.to_string(), object_boolean_expression.name.clone());

    let data_connector_name = Qualified::new(
        subgraph.to_string(),
        object_boolean_expression.data_connector_name.clone(),
    );

    // validate graphql config
    let boolean_expression_graphql_config = object_boolean_expression
        .graphql
        .as_ref()
        .map(|object_boolean_graphql_config| {
            let graphql_type_name =
                mk_name(object_boolean_graphql_config.type_name.as_ref()).map(ast::TypeName)?;

            store_new_graphql_type(existing_graphql_types, Some(&graphql_type_name))?;

            let type_mapping = object_type_representation
                .type_mappings
                .get(
                    &data_connector_name,
                    &object_boolean_expression.data_connector_object_type,
                )
                .unwrap();

            let scalars =
                data_connector_scalars
                    .get(&data_connector_name)
                    .ok_or(Error::from (
                        boolean_expressions::BooleanExpressionError::UnknownDataConnectorInObjectBooleanExpressionType {
                            object_boolean_expression_type: object_boolean_expression_type.clone(),
                            data_connector: data_connector_name.clone(),
                        },
                    ))?;

            resolve_boolean_expression_graphql_config(
                &data_connector_name,
                graphql_type_name,
                subgraph,
                scalars,
                type_mapping,
                graphql_config,
                &object_type_representation.object_type.fields,
            )
        })
        .transpose()?;

    let data_connector_link =
        data_connectors::DataConnectorLink::new(data_connector_name, data_connector_context)?;

    let resolved_boolean_expression = ObjectBooleanExpressionType {
        name: qualified_name.clone(),
        object_type: qualified_object_type_name.clone(),
        data_connector: ObjectBooleanExpressionDataConnector {
            object_type: object_boolean_expression.data_connector_object_type.clone(),
            name: qualified_data_connector_name,
            link: data_connector_link,
        },
        graphql: boolean_expression_graphql_config,
    };
    Ok(resolved_boolean_expression)
}

// record filter expression info
pub fn resolve_boolean_expression_graphql_config(
    data_connector_name: &Qualified<open_dds::data_connector::DataConnectorName>,
    where_type_name: ast::TypeName,
    subgraph: &str,
    scalars: &data_connector_scalar_types::ScalarTypeWithRepresentationInfoMap,
    type_mappings: &object_types::TypeMapping,
    graphql_config: &graphql_config::GraphqlConfig,
    fields: &IndexMap<open_dds::types::FieldName, object_types::FieldDefinition>,
) -> Result<boolean_expressions::BooleanExpressionGraphqlConfig, Error> {
    let mut scalar_fields = BTreeMap::new();

    let object_types::TypeMapping::Object { field_mappings, .. } = type_mappings;

    let filter_graphql_config = graphql_config
        .query
        .filter_input_config
        .as_ref()
        .ok_or_else(|| Error::GraphqlConfigError {
            graphql_config_error:
                graphql_config::GraphqlConfigError::MissingFilterInputFieldInGraphqlConfig,
        })?;

    for (field_name, field_mapping) in field_mappings {
        // fields with field arguments are not allowed in boolean expressions
        if let Some(field_definition) = fields.get(field_name) {
            if !field_definition.field_arguments.is_empty() {
                continue;
            }
        }
        // Generate comparison expression for fields mapped to simple scalar type
        if let Some(scalar_type_info) = data_connector_scalar_types::get_simple_scalar(
            field_mapping.column_type.clone(),
            scalars,
        ) {
            if let Some(graphql_type_name) = &scalar_type_info.comparison_expression_name.clone() {
                let mut operators = BTreeMap::new();
                for (op_name, op_definition) in &scalar_type_info.scalar_type.comparison_operators {
                    let operator_name = OperatorName::from(op_name.as_str());
                    operators.insert(
                        operator_name,
                        resolve_ndc_type(
                            data_connector_name,
                            &get_argument_type(op_definition, &field_mapping.column_type),
                            scalars,
                            subgraph,
                        )?,
                    );
                }

                let mut operator_mapping = BTreeMap::new();
                operator_mapping.insert(data_connector_name.clone(), BTreeMap::new());

                // Register scalar comparison field only if it contains non-zero operators.
                if !operators.is_empty() {
                    scalar_fields.insert(
                        field_name.clone(),
                        boolean_expressions::ComparisonExpressionInfo {
                            object_type_name: None,
                            type_name: graphql_type_name.clone(),
                            operator_mapping,
                            operators,
                            is_null_operator_name: Some(
                                filter_graphql_config.operator_names.is_null.clone(),
                            ),
                        },
                    );
                };
            }
        }
    }

    Ok(boolean_expressions::BooleanExpressionGraphqlConfig {
        type_name: where_type_name,
        scalar_fields,
        object_fields: BTreeMap::new(),
        relationship_fields: BTreeMap::new(),
        graphql_config: (boolean_expressions::BooleanExpressionGraphqlFieldConfig {
            where_field_name: filter_graphql_config.where_field_name.clone(),
            and_operator_name: filter_graphql_config.operator_names.and.clone(),
            or_operator_name: filter_graphql_config.operator_names.or.clone(),
            not_operator_name: filter_graphql_config.operator_names.not.clone(),
        }),
    })
}

fn unwrap_nullable(field_type: &ndc_models::Type) -> &ndc_models::Type {
    if let ndc_models::Type::Nullable { underlying_type } = field_type {
        unwrap_nullable(underlying_type)
    } else {
        field_type
    }
}

fn get_argument_type(
    op_definition: &ndc_models::ComparisonOperatorDefinition,
    field_type: &ndc_models::Type,
) -> ndc_models::Type {
    match op_definition {
        ndc_models::ComparisonOperatorDefinition::Equal => unwrap_nullable(field_type).clone(),
        ndc_models::ComparisonOperatorDefinition::In => ndc_models::Type::Array {
            element_type: Box::new(unwrap_nullable(field_type).clone()),
        },
        ndc_models::ComparisonOperatorDefinition::Custom { argument_type } => argument_type.clone(),
    }
}
