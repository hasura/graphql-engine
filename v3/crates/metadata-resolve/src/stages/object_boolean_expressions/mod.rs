pub mod types;
use crate::stages::{
    data_connector_scalar_types, data_connectors, graphql_config, object_types, scalar_types,
    type_permissions,
};
use crate::types::error::{BooleanExpressionError, Error, GraphqlConfigError};

use crate::helpers::model::resolve_ndc_type;
use crate::helpers::types::{mk_name, store_new_graphql_type};
use crate::types::subgraph::Qualified;

use crate::helpers::type_mappings;
use lang_graphql::ast::common as ast;
use open_dds::types::CustomTypeName;
use std::collections::{BTreeMap, BTreeSet};
pub use types::{
    BooleanExpressionGraphqlConfig, BooleanExpressionInfo, BooleanExpressionsOutput,
    ComparisonExpressionInfo, ObjectBooleanExpressionType,
};

/// resolve object boolean expression types
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    existing_graphql_types: &BTreeSet<ast::TypeName>,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<BooleanExpressionsOutput, Error> {
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
            object_types,
            scalar_types,
            &mut graphql_types,
            graphql_config,
        )?;
        if let Some(existing) = object_boolean_expression_types.insert(
            resolved_boolean_expression.name.clone(),
            resolved_boolean_expression,
        ) {
            return Err(Error::from(
                BooleanExpressionError::DuplicateObjectBooleanExpressionTypeDefinition {
                    name: existing.name,
                },
            ));
        }
    }
    Ok(BooleanExpressionsOutput {
        object_boolean_expression_types,
        graphql_types,
    })
}

/// Resolves a given object boolean expression type
pub(crate) fn resolve_object_boolean_expression_type(
    object_boolean_expression: &open_dds::types::ObjectBooleanExpressionTypeV1,
    subgraph: &str,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    existing_graphql_types: &mut BTreeSet<ast::TypeName>,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<ObjectBooleanExpressionType, Error> {
    // name of the boolean expression
    let qualified_name = Qualified::new(
        subgraph.to_string(),
        object_boolean_expression.name.to_owned(),
    );
    // name of the object type backing the boolean expression
    let qualified_object_type_name = Qualified::new(
        subgraph.to_string(),
        object_boolean_expression.object_type.to_owned(),
    );
    let object_type_representation =
        object_types
            .get(&qualified_object_type_name)
            .ok_or_else(|| {
                Error::from(
                    BooleanExpressionError::UnknownTypeInObjectBooleanExpressionType {
                        type_name: qualified_object_type_name.clone(),
                    },
                )
            })?;

    let qualified_data_connector_name = Qualified::new(
        subgraph.to_string(),
        object_boolean_expression.data_connector_name.to_owned(),
    );

    // validate data connector name
    let data_connector_context = data_connectors
        .data_connectors_with_scalars
        .get(&qualified_data_connector_name)
        .ok_or_else(|| {
            Error::from(
                BooleanExpressionError::UnknownDataConnectorInObjectBooleanExpressionType {
                    data_connector: qualified_data_connector_name.clone(),
                    object_boolean_expression_type: qualified_name.clone(),
                },
            )
        })?;

    // validate data connector object type
    if !data_connector_context
        .inner
        .schema
        .object_types
        .contains_key(&object_boolean_expression.data_connector_object_type.0)
    {
        return Err(Error::from(
            BooleanExpressionError::UnknownDataConnectorTypeInObjectBooleanExpressionType {
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
                    Error::from(BooleanExpressionError::NoDataConnectorTypeMappingForObjectTypeInBooleanExpression {
                        object_type: qualified_object_type_name.clone(),
                        object_boolean_expression_type: qualified_name.clone(),
                        data_connector_object_type: object_boolean_expression
                            .data_connector_object_type.clone(),
                        data_connector: qualified_data_connector_name.clone(),
                    })
                })?;

    // validate comparable fields
    for comparable_field in object_boolean_expression.comparable_fields.iter() {
        if !object_type_representation
            .object_type
            .fields
            .contains_key(&comparable_field.field_name)
        {
            return Err(
                BooleanExpressionError::UnknownFieldInObjectBooleanExpressionType {
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

    let object_type = Qualified::new(
        subgraph.to_string(),
        object_boolean_expression.object_type.clone(),
    );

    let data_connector_name = Qualified::new(
        subgraph.to_string(),
        object_boolean_expression.data_connector_name.clone(),
    );

    // Collect type mappings.
    let mut type_mappings = BTreeMap::new();

    let type_mapping_to_collect = type_mappings::TypeMappingToCollect {
        type_name: &object_type,
        ndc_object_type_name: &object_boolean_expression.data_connector_object_type,
    };
    type_mappings::collect_type_mapping_for_source(
        &type_mapping_to_collect,
        &qualified_data_connector_name,
        object_types,
        scalar_types,
        &mut type_mappings,
    )
    .map_err(|error| {
        Error::from(
            BooleanExpressionError::BooleanExpressionTypeMappingCollectionError {
                object_boolean_expression_type: object_boolean_expression_type.clone(),
                error,
            },
        )
    })?;

    // validate graphql config
    let boolean_expression_graphql_config = object_boolean_expression
        .graphql
        .as_ref()
        .map(|object_boolean_graphql_config| {
            let graphql_type_name =
                mk_name(object_boolean_graphql_config.type_name.0.as_ref()).map(ast::TypeName)?;

            store_new_graphql_type(existing_graphql_types, Some(&graphql_type_name))?;

            let type_mapping = type_mappings
                .get(&Qualified::new(
                    subgraph.to_string(),
                    object_boolean_expression.object_type.clone(),
                ))
                .unwrap();

            resolve_boolean_expression_info(
                &object_boolean_expression_type,
                &data_connector_name,
                graphql_type_name.clone(),
                subgraph,
                data_connectors,
                type_mapping,
                graphql_config,
            )
        })
        .transpose()?;

    let data_connector_link = data_connectors::DataConnectorLink::new(
        data_connector_name,
        data_connector_context.inner.url.clone(),
        data_connector_context.inner.headers,
    )?;

    let resolved_boolean_expression = ObjectBooleanExpressionType {
        name: qualified_name.clone(),
        type_mappings,
        object_type: qualified_object_type_name.clone(),
        data_connector_object_type_dont_use_please: Some(
            object_boolean_expression.data_connector_object_type.clone(),
        ),
        data_connector_name: qualified_data_connector_name,
        data_connector_link,
        graphql: boolean_expression_graphql_config,
    };
    Ok(resolved_boolean_expression)
}

// record filter expression info
pub fn resolve_boolean_expression_info(
    name: &Qualified<CustomTypeName>,
    data_connector_name: &Qualified<open_dds::data_connector::DataConnectorName>,
    where_type_name: ast::TypeName,
    subgraph: &str,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    type_mappings: &object_types::TypeMapping,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<BooleanExpressionInfo, Error> {
    let mut scalar_fields = BTreeMap::new();

    let scalar_types = &data_connectors
        .data_connectors_with_scalars
        .get(data_connector_name)
        .ok_or(Error::BooleanExpressionError {
            boolean_expression_error:
                BooleanExpressionError::UnknownDataConnectorInObjectBooleanExpressionType {
                    object_boolean_expression_type: name.clone(),
                    data_connector: data_connector_name.clone(),
                },
        })?
        .scalars;

    let object_types::TypeMapping::Object { field_mappings, .. } = type_mappings;

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
            data_connector_scalar_types::get_simple_scalar(
                field_mapping.column_type.clone(),
                scalar_types,
            )
        {
            if let Some(graphql_type_name) = &scalar_type_info.comparison_expression_name.clone() {
                let mut operators = BTreeMap::new();
                for (op_name, op_definition) in
                    scalar_type_info.scalar_type.comparison_operators.iter()
                {
                    operators.insert(
                        op_name.clone(),
                        resolve_ndc_type(
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

    Ok(BooleanExpressionInfo {
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
