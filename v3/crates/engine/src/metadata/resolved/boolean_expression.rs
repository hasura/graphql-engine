use super::stages::data_connector_scalar_types;

use crate::metadata::resolved::data_connector;

use crate::metadata::resolved::error::{BooleanExpressionError, Error, GraphqlConfigError};
use crate::metadata::resolved::model;

use crate::metadata::resolved::stages::graphql_config::GraphqlConfig;
use crate::metadata::resolved::subgraph::{Qualified, QualifiedTypeReference};

use crate::metadata::resolved::types::TypeMapping;

use lang_graphql::ast::common::{self as ast};
use ndc_models;

use open_dds::{
    data_connector::DataConnectorName,
    types::{CustomTypeName, FieldName},
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ComparisonExpressionInfo {
    pub data_connector_name: Qualified<DataConnectorName>,
    pub scalar_type_name: String,
    pub type_name: ast::TypeName,
    pub ndc_column: String,
    pub operators: BTreeMap<String, QualifiedTypeReference>,
    pub is_null_operator_name: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionGraphqlConfig {
    pub where_field_name: ast::Name,
    pub and_operator_name: ast::Name,
    pub or_operator_name: ast::Name,
    pub not_operator_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpression {
    pub type_name: ast::TypeName,
    pub scalar_fields: HashMap<FieldName, ComparisonExpressionInfo>,
    pub graphql_config: BooleanExpressionGraphqlConfig,
}

// record filter expression info
pub fn resolve_boolean_expression(
    name: &Qualified<CustomTypeName>,
    data_connector_name: &Qualified<DataConnectorName>,
    where_type_name: ast::TypeName,
    subgraph: &str,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    type_mappings: &TypeMapping,
    graphql_config: &GraphqlConfig,
) -> Result<BooleanExpression, Error> {
    let mut scalar_fields = HashMap::new();

    let scalar_types = &data_connectors
        .data_connectors_with_scalars
        .get(data_connector_name)
        .ok_or(Error::BooleanExpressionError {
            boolean_expression_error:
                BooleanExpressionError::UnknownDataConnectorInObjectBooleanExpressionType {
                    boolean_expression_type: name.clone(),
                    data_connector: data_connector_name.clone(),
                },
        })?
        .scalars;

    let TypeMapping::Object { field_mappings, .. } = type_mappings;

    let filter_graphql_config = graphql_config
        .query
        .filter_input_config
        .as_ref()
        .ok_or_else(|| Error::GraphqlConfigError {
            graphql_config_error: GraphqlConfigError::MissingFilterInputFieldInGraphqlConfig,
        })?;

    for (field_name, field_mapping) in field_mappings.iter() {
        // Generate comparison expression for fields mapped to simple scalar type
        if let Some((scalar_type_name, scalar_type_info)) =
            data_connector::get_simple_scalar(field_mapping.column_type.clone(), scalar_types)
        {
            if let Some(graphql_type_name) = &scalar_type_info.comparison_expression_name.clone() {
                let mut operators = BTreeMap::new();
                for (op_name, op_definition) in
                    scalar_type_info.scalar_type.comparison_operators.iter()
                {
                    operators.insert(
                        op_name.clone(),
                        model::resolve_ndc_type(
                            data_connector_name,
                            &get_argument_type(op_definition, &field_mapping.column_type),
                            scalar_types,
                            subgraph,
                        )?,
                    );
                }

                // Register scalar comparison field only if it contains non-zero operators.
                if !operators.is_empty() {
                    scalar_fields.insert(
                        field_name.clone(),
                        ComparisonExpressionInfo {
                            data_connector_name: data_connector_name.clone(),
                            scalar_type_name: scalar_type_name.clone(),
                            type_name: graphql_type_name.clone(),
                            ndc_column: field_mapping.column.clone(),
                            operators,
                            is_null_operator_name: filter_graphql_config
                                .operator_names
                                .is_null
                                .to_string(),
                        },
                    );
                };
            }
        }
    }

    Ok(BooleanExpression {
        type_name: where_type_name,
        scalar_fields,
        graphql_config: (BooleanExpressionGraphqlConfig {
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
