use super::object;
use super::{
    BooleanExpressionIssue, BooleanExpressionTypeIdentifier, ComparableFieldKind,
    ComparisonExpressionInfo, DataConnectorType, IncludeLogicalOperators,
    ObjectBooleanExpressionDataConnector, ResolvedObjectBooleanExpressionType,
    ResolvedObjectBooleanExpressionTypeFields, error::BooleanExpressionError, graphql,
};
use crate::stages::{
    data_connector_scalar_types, data_connectors, graphql_config, object_types, relationships,
    scalar_boolean_expressions, type_permissions,
};
use crate::types::subgraph::mk_qualified_type_name;
use crate::{Qualified, ScalarComparisonKind};
use open_dds::{
    data_connector::DataConnectorName,
    models::ModelName,
    types::{CustomTypeName, FieldName, GraphQlTypeName},
};
use std::collections::{BTreeMap, BTreeSet};

pub(crate) type RawBooleanExpressionTypes<'a> = BTreeMap<
    Qualified<CustomTypeName>,
    (
        &'a open_dds::identifier::SubgraphName,
        &'a open_dds::boolean_expression::BooleanExpressionTypeV1,
    ),
>;

/// Resolves a legacy `ObjectBooleanExpressionType`
pub(crate) fn resolve_object_boolean_expression_type(
    object_boolean_expression_type: &open_dds::types::ObjectBooleanExpressionTypeV1,
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_boolean_expression_types: &BTreeMap<
        BooleanExpressionTypeIdentifier,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    raw_boolean_expression_types: &RawBooleanExpressionTypes,
    relationships: &relationships::Relationships,
    raw_models: &BTreeMap<Qualified<ModelName>, &open_dds::models::Model>,
    object_boolean_expression_type_names: &BTreeSet<Qualified<CustomTypeName>>,
    graphql_config: &graphql_config::GraphqlConfig,
    flags: &open_dds::flags::OpenDdFlags,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<
    (
        ResolvedObjectBooleanExpressionType,
        Vec<BooleanExpressionIssue>,
    ),
    BooleanExpressionError,
> {
    let mut issues = vec![];
    let qualified_object_type_name = Qualified::new(
        boolean_expression_type_name.subgraph.clone(),
        object_boolean_expression_type.object_type.clone(),
    );
    let qualified_data_connector_name = Qualified::new(
        boolean_expression_type_name.subgraph.clone(),
        object_boolean_expression_type.data_connector_name.clone(),
    );

    // validate data connector name
    let data_connector_context = data_connectors
        .0
        .get(&qualified_data_connector_name)
        .ok_or_else(|| {
            BooleanExpressionError::UnknownDataConnectorInObjectBooleanExpressionType {
                data_connector: qualified_data_connector_name.clone(),
                object_boolean_expression_type: boolean_expression_type_name.clone(),
            }
        })?;

    // get the underlying object type
    let object_type_representation =
        object_types
            .get(&qualified_object_type_name)
            .ok_or_else(
                || BooleanExpressionError::UnknownTypeInObjectBooleanExpressionType {
                    type_name: qualified_object_type_name.clone(),
                    boolean_expression_type_name: boolean_expression_type_name.clone(),
                },
            )?;

    // validate data connector object type
    if !data_connector_context.schema.object_types.contains_key(
        object_boolean_expression_type
            .data_connector_object_type
            .as_str(),
    ) {
        return Err(
            BooleanExpressionError::UnknownDataConnectorTypeInObjectBooleanExpressionType {
                data_connector: qualified_data_connector_name.clone(),
                object_boolean_expression_type: boolean_expression_type_name.clone(),
                data_connector_object_type: object_boolean_expression_type
                    .data_connector_object_type
                    .clone(),
            },
        );
    }

    let type_mapping = object_type_representation
        .type_mappings
        .get(
            &qualified_data_connector_name,
            &object_boolean_expression_type.data_connector_object_type,
        )
        .ok_or_else(|| {
            BooleanExpressionError::NoDataConnectorTypeMappingForObjectTypeInBooleanExpression {
                object_type: qualified_object_type_name.clone(),
                object_boolean_expression_type: boolean_expression_type_name.clone(),
                data_connector_object_type: object_boolean_expression_type
                    .data_connector_object_type
                    .clone(),
                data_connector: qualified_data_connector_name.clone(),
            }
        })?;

    let object_types::TypeMapping::Object { field_mappings, .. } = type_mapping;

    let binding = object_boolean_expression_type
        .graphql
        .as_ref()
        .map(|graphql| graphql.type_name.clone());
    let graphql_type_name = binding.as_ref();

    let mut generated_comparable_fields = vec![];

    // we will refer to scalar boolean expressions created earlier
    for comparable_field in &object_boolean_expression_type.comparable_fields {
        let field_mapping = field_mappings
            .get(&comparable_field.field_name)
            .ok_or_else(
                || BooleanExpressionError::UnknownFieldInObjectBooleanExpressionType {
                    field_name: comparable_field.field_name.clone(),
                    object_boolean_expression_type: boolean_expression_type_name.clone(),
                },
            )?;

        let scalar_types = data_connector_scalars
            .get(&qualified_data_connector_name)
            .ok_or_else(
                || BooleanExpressionError::DataConnectorScalarRepresentationsNotFound {
                    data_connector_name: qualified_data_connector_name.clone(),
                    boolean_expression_type: boolean_expression_type_name.clone(),
                },
            )?;

        if let Ok(scalar_type_info) = data_connector_scalar_types::get_simple_scalar(
            field_mapping.column_type.clone(),
            scalar_types,
        ) {
            if let Some(representation) = &scalar_type_info.representation {
                // As of now, only `"enableAll": true` is allowed for field operators
                match &comparable_field.operators {
                    open_dds::models::EnableAllOrSpecific::EnableAll(true) => {}
                    _ => {
                        return Err(
                            BooleanExpressionError::FieldLevelComparisonOperatorConfigurationNotSupported,
                        )
                    }
                }

                let qualified_type_name =
                    mk_qualified_type_name(representation, &qualified_data_connector_name.subgraph);

                let data_connector_type = DataConnectorType {
                    data_connector_name: qualified_data_connector_name.clone(),
                    type_name: qualified_type_name.clone(),
                };

                generated_comparable_fields.push(object::ComparableField {
                    field_name: comparable_field.field_name.clone(),
                    boolean_expression_type:
                        BooleanExpressionTypeIdentifier::FromDataConnectorScalarRepresentation(
                            data_connector_type,
                        ),
                });
            }
        }
    }

    // Comparable fields should have all type fields
    if object_boolean_expression_type.comparable_fields.len()
        != object_type_representation.object_type.fields.len()
    {
        return Err(BooleanExpressionError::FieldLevelComparisonOperatorNeedsAllFields);
    }

    // resolve any comparable fields
    let ComparableFieldsOutput {
        comparable_fields,
        scalar_fields,
    } = resolve_comparable_fields(
        &generated_comparable_fields,
        &object_type_representation.object_type,
        boolean_expression_type_name,
        scalar_boolean_expression_types,
        graphql_type_name,
        flags,
        &mut issues,
    )?;

    // we use all relationships from the underlying type
    let comparable_relationships_input = relationships
        .get_relationships_for_type(&qualified_object_type_name)
        .keys()
        .map(|relationship_name| {
            open_dds::boolean_expression::BooleanExpressionComparableRelationship {
                relationship_name: relationship_name.clone(),
                boolean_expression_type: None,
                aggregate_boolean_expression_type: None,
            }
        })
        .collect();

    let comparable_relationships = object::resolve_comparable_relationships(
        boolean_expression_type_name,
        &qualified_object_type_name,
        &comparable_relationships_input,
        relationships,
        &boolean_expression_type_name.subgraph,
        raw_boolean_expression_types,
        raw_models,
        object_boolean_expression_type_names,
        &mut issues,
    )?;

    let include_logical_operators = IncludeLogicalOperators::Yes;

    // resolve graphql schema information
    let resolved_graphql = graphql_type_name
        .as_ref()
        .map(|object_boolean_graphql_config| {
            graphql::resolve_object_boolean_graphql(
                boolean_expression_type_name,
                object_boolean_graphql_config,
                &comparable_fields,
                &comparable_relationships,
                include_logical_operators,
                scalar_boolean_expression_types,
                raw_boolean_expression_types,
                graphql_config,
                graphql_types,
                &mut issues,
            )
        })
        .transpose()?;

    let object_boolean_type_data_connector = ObjectBooleanExpressionDataConnector {
        name: qualified_data_connector_name,
        object_type: object_boolean_expression_type
            .data_connector_object_type
            .clone(),
    };

    Ok((
        ResolvedObjectBooleanExpressionType {
            name: boolean_expression_type_name.clone(),
            include_logical_operators,
            fields: ResolvedObjectBooleanExpressionTypeFields {
                object_fields: BTreeMap::new(),
                scalar_fields,
                relationship_fields: comparable_relationships,
            },
            object_type: qualified_object_type_name.clone(),
            graphql: resolved_graphql,
            data_connector: Some(object_boolean_type_data_connector),
        },
        issues,
    ))
}

struct ComparableFieldsOutput {
    pub comparable_fields:
        BTreeMap<FieldName, (ComparableFieldKind, BooleanExpressionTypeIdentifier)>,
    pub scalar_fields: BTreeMap<FieldName, ComparisonExpressionInfo>,
}

// comparable_fields don't do much, all we can do is ensure that the other BooleanExpressionTypes
// they refer to exist
fn resolve_comparable_fields(
    comparable_fields: &Vec<object::ComparableField>,
    object_type_representation: &object_types::ObjectTypeRepresentation,
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    scalar_boolean_expression_types: &BTreeMap<
        BooleanExpressionTypeIdentifier,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    graphql: Option<&GraphQlTypeName>,
    flags: &open_dds::flags::OpenDdFlags,
    issues: &mut Vec<BooleanExpressionIssue>,
) -> Result<ComparableFieldsOutput, BooleanExpressionError> {
    let mut resolved_comparable_fields = BTreeMap::new();

    let mut scalar_fields = BTreeMap::new();

    // validate comparable fields all exist in underlying object
    for comparable_field in comparable_fields {
        let field = object_type_representation
            .fields
            .get(&comparable_field.field_name)
            .ok_or_else(
                || BooleanExpressionError::UnknownFieldInObjectBooleanExpressionType {
                    field_name: comparable_field.field_name.clone(),
                    object_boolean_expression_type: boolean_expression_type_name.clone(),
                },
            )?;

        // fields with field arguments are not allowed in boolean expressions
        if !field.field_arguments.is_empty() {
            continue;
        }

        let field_boolean_expression_type_name = comparable_field.boolean_expression_type.clone();

        if let Some(_duplicate_field) = resolved_comparable_fields.insert(
            comparable_field.field_name.clone(),
            (
                ComparableFieldKind::Scalar,
                field_boolean_expression_type_name,
            ),
        ) {
            issues.push(BooleanExpressionIssue::DuplicateComparableFieldFound {
                type_name: boolean_expression_type_name.clone(),
                name: comparable_field.field_name.clone(),
            });
        };
    }

    // doing this validation when there is no graphql configuration is a breaking change, so we
    // only do it if the flag allows it
    if graphql.is_some()
        || flags.contains(open_dds::flags::Flag::AllowBooleanExpressionFieldsWithoutGraphql)
    {
        for (comparable_field_name, (_comparable_field_kind, comparable_field_type_name)) in
            &resolved_comparable_fields
        {
            if let Some(scalar_boolean_expression_type) =
                scalar_boolean_expression_types.get(comparable_field_type_name)
            {
                // Register scalar comparison field only if it contains non-zero operators.
                if !scalar_boolean_expression_type
                    .comparison_operators
                    .is_empty()
                {
                    scalar_fields.insert(
                        comparable_field_name.clone(),
                        ComparisonExpressionInfo {
                            field_kind: ScalarComparisonKind::Scalar,
                            boolean_expression_type_name: comparable_field_type_name.clone(),
                            operators: scalar_boolean_expression_type.comparison_operators.clone(),
                            operator_mapping: scalar_boolean_expression_type
                                .data_connector_operator_mappings
                                .iter()
                                .map(|(data_connector_name, mappings)| {
                                    (
                                        data_connector_name.clone(),
                                        crate::OperatorMapping(mappings.operator_mapping.clone()),
                                    )
                                })
                                .collect(),
                            logical_operators: scalar_boolean_expression_type
                                .logical_operators
                                .clone(),
                        },
                    );
                };
            }
        }
    }

    Ok(ComparableFieldsOutput {
        scalar_fields,
        comparable_fields: resolved_comparable_fields,
    })
}
