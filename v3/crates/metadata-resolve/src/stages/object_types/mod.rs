mod error;
mod recursive_types;
pub mod types;

pub use error::{ObjectTypesError, TypeMappingValidationError};
use open_dds::aggregates::{
    DataConnectorAggregationFunctionName, DataConnectorExtractionFunctionName,
};
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

use open_dds::commands::ArgumentMapping;
use open_dds::{
    data_connector::{DataConnectorColumnName, DataConnectorName, DataConnectorOperatorName},
    types::CustomTypeName,
};
pub use types::{
    AggregateFunctions, ComparisonOperators, DataConnectorTypeMappingsForObject,
    ExtractionFunctions, FieldArgumentInfo, FieldDefinition, FieldMapping,
    ObjectTypeRepresentation, ObjectTypeWithTypeMappings, ObjectTypesIssue, ObjectTypesOutput,
    ObjectTypesWithTypeMappings, ResolvedApolloFederationObjectKey,
    ResolvedObjectApolloFederationConfig, TypeMapping,
};

use crate::helpers::ndc_validation::get_underlying_named_type;
use crate::helpers::type_validation::validate_type_compatibility;
use crate::helpers::types::{mk_name, unwrap_custom_type_name, unwrap_qualified_type_name};
use crate::stages::{
    apollo, data_connector_scalar_types, data_connectors, graphql_config, scalar_types,
};

use crate::types::subgraph::{Qualified, mk_qualified_type_name, mk_qualified_type_reference};

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;

/// resolve object types, matching them to that in the data connectors
pub(crate) fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalar_types: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<ObjectTypesOutput, Vec<ObjectTypesError>> {
    let mut object_types = BTreeMap::new();
    let mut global_id_enabled_types = BTreeMap::new();
    let mut apollo_federation_entity_enabled_types = BTreeMap::new();
    let mut issues = Vec::new();
    let mut raw_object_types = BTreeMap::new();
    let mut results = vec![];
    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: object_type_definition,
    } in &metadata_accessor.object_types
    {
        let qualified_object_type_name =
            Qualified::new(subgraph.clone(), object_type_definition.name.clone());
        raw_object_types.insert(qualified_object_type_name, object_type_definition);
    }

    // collect names of all object boolean expression types
    let mut object_boolean_expression_type_names = BTreeSet::new();
    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: boolean_expression_type,
    } in &metadata_accessor.boolean_expression_types
    {
        if let open_dds::boolean_expression::BooleanExpressionOperand::Object(_) =
            boolean_expression_type.operand
        {
            object_boolean_expression_type_names.insert(Qualified::new(
                subgraph.clone(),
                boolean_expression_type.name.clone(),
            ));
        }
    }

    for (qualified_object_type_name, object_type_definition) in &raw_object_types {
        results.push(resolve_object_type(
            object_type_definition,
            qualified_object_type_name,
            &raw_object_types,
            scalar_types,
            &object_boolean_expression_type_names,
            graphql_types,
            data_connectors,
            data_connector_scalar_types,
            &mut global_id_enabled_types,
            &mut apollo_federation_entity_enabled_types,
            &mut issues,
            &mut object_types,
        ));
    }

    // Check for recursive object types
    issues.extend(recursive_types::check_recursive_object_types(&object_types));

    // if everything succeeds, return results, otherwise collect all errors together
    partition_eithers::collect_any_errors(results).map(|_| ObjectTypesOutput {
        issues,
        global_id_enabled_types,
        apollo_federation_entity_enabled_types,
        object_types: ObjectTypesWithTypeMappings(object_types),
    })
}

fn resolve_object_type(
    object_type_definition: &open_dds::types::ObjectTypeV1,
    qualified_object_type_name: &Qualified<CustomTypeName>,
    raw_object_types: &BTreeMap<Qualified<CustomTypeName>, &open_dds::types::ObjectTypeV1>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_type_names: &BTreeSet<Qualified<CustomTypeName>>,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalar_types: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    global_id_enabled_types: &mut BTreeMap<
        Qualified<CustomTypeName>,
        Vec<Qualified<open_dds::models::ModelName>>,
    >,
    apollo_federation_entity_enabled_types: &mut BTreeMap<
        Qualified<CustomTypeName>,
        Option<Qualified<open_dds::models::ModelName>>,
    >,
    issues: &mut Vec<ObjectTypesIssue>,
    object_types: &mut BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithTypeMappings>,
) -> Result<(), ObjectTypesError> {
    let resolved_object_type = resolve_object_type_representation(
        object_type_definition,
        qualified_object_type_name,
        raw_object_types,
        scalar_types,
        object_boolean_expression_type_names,
        graphql_types,
        global_id_enabled_types,
        apollo_federation_entity_enabled_types,
        issues,
    )?;

    let mut type_mappings = DataConnectorTypeMappingsForObject::new();

    // resolve object types' type mappings
    for dc_type_mapping in &object_type_definition.data_connector_type_mapping {
        let qualified_data_connector_name = Qualified::new(
            qualified_object_type_name.subgraph.clone(),
            dc_type_mapping.data_connector_name.clone(),
        );
        let (type_mapping, new_issues) = resolve_data_connector_type_mapping(
            dc_type_mapping,
            qualified_object_type_name,
            &resolved_object_type,
            data_connectors,
            data_connector_scalar_types,
        )
        .map_err(|type_validation_error| {
            ObjectTypesError::DataConnectorTypeMappingValidationError {
                type_name: qualified_object_type_name.clone(),
                error: type_validation_error,
            }
        })?;

        issues.extend(new_issues);

        type_mappings.insert(
            &qualified_data_connector_name,
            &dc_type_mapping.data_connector_object_type,
            type_mapping,
        )?;
    }

    let object_type_with_type_mappings = ObjectTypeWithTypeMappings {
        object_type: resolved_object_type,
        type_mappings,
    };

    if object_types
        .insert(
            qualified_object_type_name.clone(),
            object_type_with_type_mappings,
        )
        .is_some()
    {
        return Err(ObjectTypesError::DuplicateTypeDefinition {
            name: qualified_object_type_name.clone(),
        });
    }
    Ok(())
}

fn resolve_field(
    field: &open_dds::types::FieldDefinition,
    qualified_type_name: &Qualified<CustomTypeName>,
    raw_object_types: &BTreeMap<Qualified<CustomTypeName>, &open_dds::types::ObjectTypeV1>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_type_names: &BTreeSet<Qualified<CustomTypeName>>,
    issues: &mut Vec<ObjectTypesIssue>,
) -> Result<FieldDefinition, ObjectTypesError> {
    let qualified_type_reference =
        mk_qualified_type_reference(&field.field_type, &qualified_type_name.subgraph);

    // let's check that any object, scalar or object boolean expression types used in this field exist
    if let Some(custom_type_name) = unwrap_custom_type_name(&qualified_type_reference) {
        if raw_object_types.get(custom_type_name).is_none()
            && scalar_types.get(custom_type_name).is_none()
            && !object_boolean_expression_type_names.contains(custom_type_name)
        {
            issues.push(ObjectTypesIssue::FieldTypeNotFound {
                field_name: field.name.clone(),
                object_type_name: qualified_type_name.clone(),
                field_type: custom_type_name.clone(),
            });
        }
    }

    let mut field_arguments = IndexMap::new();
    for argument in &field.arguments {
        let field_argument_definition = FieldArgumentInfo {
            argument_type: mk_qualified_type_reference(
                &argument.argument_type,
                &qualified_type_name.subgraph,
            ),
            description: argument.description.clone(),
        };
        if field_arguments
            .insert(argument.name.clone(), field_argument_definition)
            .is_some()
        {
            return Err(ObjectTypesError::DuplicateArgumentDefinition {
                field_name: field.name.clone(),
                argument_name: argument.name.clone(),
                type_name: qualified_type_name.clone(),
            });
        }
    }
    Ok(FieldDefinition {
        field_type: qualified_type_reference,
        description: field.description.clone(),
        deprecated: field.deprecated.clone(),
        field_arguments,
    })
}

pub fn resolve_object_type_representation(
    object_type_definition: &open_dds::types::ObjectTypeV1,
    qualified_type_name: &Qualified<CustomTypeName>,
    raw_object_types: &BTreeMap<Qualified<CustomTypeName>, &open_dds::types::ObjectTypeV1>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_type_names: &BTreeSet<Qualified<CustomTypeName>>,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
    global_id_enabled_types: &mut BTreeMap<
        Qualified<CustomTypeName>,
        Vec<Qualified<open_dds::models::ModelName>>,
    >,
    apollo_federation_entity_enabled_types: &mut BTreeMap<
        Qualified<CustomTypeName>,
        Option<Qualified<open_dds::models::ModelName>>,
    >,
    issues: &mut Vec<ObjectTypesIssue>,
) -> Result<ObjectTypeRepresentation, ObjectTypesError> {
    let mut resolved_fields = IndexMap::new();
    let mut resolved_global_id_fields = Vec::new();

    for field in &object_type_definition.fields {
        if resolved_fields
            .insert(
                field.name.clone(),
                resolve_field(
                    field,
                    qualified_type_name,
                    raw_object_types,
                    scalar_types,
                    object_boolean_expression_type_names,
                    issues,
                )?,
            )
            .is_some()
        {
            return Err(ObjectTypesError::DuplicateFieldDefinition {
                type_name: qualified_type_name.clone(),
                field_name: field.name.clone(),
            });
        }
    }

    if let Some(global_id_fields) = &object_type_definition.global_id_fields {
        if !global_id_fields.is_empty() {
            // Throw error if the object type has a field called id" and has global fields configured.
            // Because, when the global id fields are configured, the `id` field will be auto-generated.
            if resolved_fields.contains_key("id") {
                return Err(ObjectTypesError::IdFieldConflictingGlobalId {
                    type_name: qualified_type_name.clone(),
                });
            }
            // To check if global_id_fields are defined in object type but no model has global_id_source set to
            // true:
            //   - If the object type has globalIdFields configured, add the object type to the
            //     global_id_enabled_types map.
            global_id_enabled_types.insert(qualified_type_name.clone(), Vec::new());
        };
        for global_id_field in global_id_fields {
            if resolved_fields.contains_key(global_id_field) {
                resolved_global_id_fields.push(global_id_field.clone());
            } else {
                return Err(ObjectTypesError::UnknownFieldInGlobalId {
                    field_name: global_id_field.clone(),
                    type_name: qualified_type_name.clone(),
                });
            }
        }
    }
    let (graphql_type_name, graphql_input_type_name, apollo_federation_config) =
        match object_type_definition.graphql.as_ref() {
            None => Ok::<_, ObjectTypesError>((None, None, None)),
            Some(graphql) => {
                let graphql_type_name = graphql
                    .type_name
                    .as_ref()
                    .map(|type_name| mk_name(type_name.as_ref()).map(ast::TypeName))
                    .transpose()?;
                let graphql_input_type_name = graphql
                    .input_type_name
                    .as_ref()
                    .map(|input_type_name| mk_name(input_type_name.as_ref()).map(ast::TypeName))
                    .transpose()?;
                // To check if apolloFederation.keys are defined in object type but no model has
                // apollo_federation_entity_source set to true:
                //   - If the object type has apolloFederation.keys configured, add the object type to the
                //     apollo_federation_entity_enabled_types map.
                let resolved_apollo_federation_config = match &graphql.apollo_federation {
                    None => Ok(None),
                    Some(apollo_federation) => {
                        // Validate that the fields in the apollo federation keys are defined in the object type
                        let mut resolved_keys: Vec<ResolvedApolloFederationObjectKey> = Vec::new();
                        for key in &apollo_federation.keys {
                            let mut resolved_key_fields = Vec::new();
                            for field in &key.fields {
                                if !resolved_fields.contains_key(field) {
                                    return Err(ObjectTypesError::from(
                                        apollo::ApolloError::UnknownFieldInApolloFederationKey {
                                            field_name: field.clone(),
                                            object_type: qualified_type_name.clone(),
                                        },
                                    ));
                                }
                                resolved_key_fields.push(field.clone());
                            }
                            let resolved_key =
                                match nonempty::NonEmpty::from_vec(resolved_key_fields) {
                                    None => {
                                        return Err(
                                            ObjectTypesError::from(apollo::ApolloError::EmptyFieldsInApolloFederationConfigForObject {
                                                object_type: qualified_type_name.clone(),
                                            }),
                                        )
                                    }
                                    Some(fields) => ResolvedApolloFederationObjectKey { fields },
                                };
                            resolved_keys.push(resolved_key);
                        }
                        apollo_federation_entity_enabled_types
                            .insert(qualified_type_name.clone(), None);
                        match nonempty::NonEmpty::from_vec(resolved_keys) {
                            None => Err(ObjectTypesError::from(
                                apollo::ApolloError::EmptyKeysInApolloFederationConfigForObject {
                                    object_type: qualified_type_name.clone(),
                                },
                            )),
                            Some(keys) => Ok(Some(ResolvedObjectApolloFederationConfig { keys })),
                        }
                    }
                }?;
                Ok((
                    graphql_type_name,
                    graphql_input_type_name,
                    resolved_apollo_federation_config,
                ))
            }
        }?;

    graphql_types.store(graphql_type_name.as_ref())?;
    graphql_types.store(graphql_input_type_name.as_ref())?;

    Ok(ObjectTypeRepresentation {
        fields: resolved_fields,
        global_id_fields: resolved_global_id_fields,
        graphql_output_type_name: graphql_type_name,
        graphql_input_type_name,
        description: object_type_definition.description.clone(),
        apollo_federation_config,
    })
}

/// Resolve a given data connector type mapping
pub fn resolve_data_connector_type_mapping(
    data_connector_type_mapping: &open_dds::types::DataConnectorTypeMapping,
    qualified_type_name: &Qualified<CustomTypeName>,
    type_representation: &ObjectTypeRepresentation,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalar_types: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
) -> Result<(TypeMapping, Vec<ObjectTypesIssue>), TypeMappingValidationError> {
    let mut issues = Vec::new();
    let qualified_data_connector_name = Qualified::new(
        qualified_type_name.subgraph.clone(),
        data_connector_type_mapping.data_connector_name.clone(),
    );

    let data_connector_context = data_connectors
        .0
        .get(&qualified_data_connector_name)
        .ok_or_else(|| TypeMappingValidationError::UnknownDataConnector {
            data_connector: qualified_data_connector_name.clone(),
            type_name: qualified_type_name.clone(),
        })?;

    let data_connector_scalars = data_connector_scalar_types
        .get(&qualified_data_connector_name)
        .ok_or_else(|| TypeMappingValidationError::UnknownDataConnector {
            data_connector: qualified_data_connector_name.clone(),
            type_name: qualified_type_name.clone(),
        })?;

    let ndc_object_type = data_connector_context
        .schema
        .object_types
        .get(
            data_connector_type_mapping
                .data_connector_object_type
                .as_str(),
        )
        .ok_or_else(|| TypeMappingValidationError::UnknownNdcType {
            type_name: qualified_type_name.clone(),
            unknown_ndc_type: data_connector_type_mapping
                .data_connector_object_type
                .clone(),
        })?;

    // Walk all the fields in the ObjectType, if there's a mapping for the field
    // use it, otherwise assume the destination column is the same name as the field.
    // At the end, if there are any mappings left over, these are invalid as they do not
    // exist in the actual ObjectType.
    let mut unconsumed_field_mappings = data_connector_type_mapping
        .field_mapping
        .0
        .iter()
        .collect::<BTreeMap<_, _>>();
    let mut resolved_field_mappings = BTreeMap::new();
    for (field_name, field_definition) in &type_representation.fields {
        let (resolved_field_mapping_column, resolved_argument_mappings) =
            if let Some(field_mapping) = unconsumed_field_mappings.remove(field_name) {
                match field_mapping {
                    open_dds::types::FieldMapping::Column(column_mapping) => (
                        Cow::Borrowed(&column_mapping.name),
                        column_mapping.argument_mapping.clone().unwrap_or_default(),
                    ),
                }
            } else {
                // If no mapping is defined for a field, implicitly create a mapping
                // with the same column name as the field.
                (
                    Cow::Owned(DataConnectorColumnName::from(field_name.as_str())),
                    ArgumentMapping::default(),
                )
            };
        let source_column =
            get_column(ndc_object_type, field_name, &resolved_field_mapping_column)?;

        // Validate OpenDd type mapping to NDC type
        if let Some(issue) = validate_type_compatibility(
            data_connector_scalars,
            &field_definition.field_type,
            &source_column.r#type,
        ) {
            issues.push(ObjectTypesIssue::FieldTypeNdcMappingIssue {
                field_name: field_name.clone(),
                type_name: qualified_type_name.clone(),
                data_connector: qualified_data_connector_name.clone(),
                data_connector_object: data_connector_type_mapping
                    .data_connector_object_type
                    .clone(),
                data_connector_column: resolved_field_mapping_column.clone().into_owned(),
                issue,
            });
        }

        let underlying_column_type = get_underlying_named_type(&source_column.r#type);

        let scalar_type = data_connector_context
            .schema
            .scalar_types
            .get(underlying_column_type.as_str());

        // we may know what type should be underlying this, in which case
        // check it against what is defined in the field for the ObjectType itself
        if let Some(data_connector_scalar_representation) = data_connector_scalars
            .by_ndc_type
            .get(underlying_column_type.as_str())
            .and_then(|scalar_type| scalar_type.representation.as_ref())
        {
            let ndc_field_type = mk_qualified_type_name(
                data_connector_scalar_representation,
                &qualified_type_name.subgraph,
            );
            let opendd_field_type = unwrap_qualified_type_name(&field_definition.field_type);

            if ndc_field_type != *opendd_field_type {
                issues.push(ObjectTypesIssue::FieldTypeMismatch {
                    data_connector: qualified_data_connector_name.clone(),
                    expected: ndc_field_type,
                    provided: opendd_field_type.clone(),
                    field_name: field_name.clone(),
                    type_name: qualified_type_name.clone(),
                });
            }
        }

        let column_type_representation = scalar_type.map(|ty| ty.representation.clone());

        let scalar_type_name = ndc_models::ScalarTypeName::new(underlying_column_type.clone());

        let comparison_operators = scalar_type.map(|ty| {
            let (c, new_issues) =
                get_comparison_operators(&scalar_type_name, ty, &qualified_data_connector_name);

            issues.extend(new_issues);
            c
        });

        let aggregate_functions = scalar_type.map(|ty| {
            let (c, new_issues) =
                make_aggregate_functions(&scalar_type_name, ty, &qualified_data_connector_name);

            issues.extend(new_issues);
            c
        });

        let extraction_functions = scalar_type.map(|ty| {
            let (c, new_issues) =
                make_extraction_functions(&scalar_type_name, ty, &qualified_data_connector_name);

            issues.extend(new_issues);
            c
        });

        let resolved_field_mapping = FieldMapping {
            column: resolved_field_mapping_column.into_owned(),
            column_type: source_column.r#type.clone(),
            column_type_representation,
            comparison_operators,
            aggregate_functions,
            extraction_functions,
            argument_mappings: resolved_argument_mappings.0,
        };

        let existing_mapping =
            resolved_field_mappings.insert(field_name.clone(), resolved_field_mapping);
        if existing_mapping.is_some() {
            return Err(TypeMappingValidationError::DuplicateFieldMapping {
                type_name: qualified_type_name.clone(),
                field_name: field_name.clone(),
            });
        }
    }
    // If any unconsumed field mappings, these do not exist in the actual ObjectType
    if !unconsumed_field_mappings.is_empty() {
        let mut unconsumed_field_names = unconsumed_field_mappings
            .into_keys()
            .cloned()
            .collect::<Vec<_>>();
        unconsumed_field_names.sort();
        return Err(TypeMappingValidationError::UnknownSourceFields {
            type_name: qualified_type_name.clone(),
            field_names: unconsumed_field_names,
        });
    }

    let resolved_type_mapping = TypeMapping::Object {
        ndc_object_type_name: data_connector_type_mapping
            .data_connector_object_type
            .clone(),
        field_mappings: resolved_field_mappings,
    };

    Ok((resolved_type_mapping, issues))
}

pub(crate) fn get_comparison_operators(
    scalar_type_name: &ndc_models::ScalarTypeName,
    scalar_type: &ndc_models::ScalarType,
    data_connector_name: &Qualified<DataConnectorName>,
) -> (ComparisonOperators, Vec<ObjectTypesIssue>) {
    let mut comparison_operators = ComparisonOperators::default();
    let mut issues = Vec::new();

    for (operator_name, operator_definition) in &scalar_type.comparison_operators {
        match operator_definition {
            ndc_models::ComparisonOperatorDefinition::Equal => {
                if comparison_operators.eq_operator.is_none() {
                    comparison_operators.eq_operator = Some(DataConnectorOperatorName::new(
                        operator_name.inner().clone(),
                    ));
                } else {
                    issues.push(ObjectTypesIssue::DuplicateOperatorsDefined {
                        scalar_type: scalar_type_name.clone(),
                        operator_name: "equals".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ComparisonOperatorDefinition::In => {
                if comparison_operators.in_operator.is_none() {
                    comparison_operators.in_operator = Some(DataConnectorOperatorName::new(
                        operator_name.inner().clone(),
                    ));
                } else {
                    issues.push(ObjectTypesIssue::DuplicateOperatorsDefined {
                        scalar_type: scalar_type_name.clone(),
                        operator_name: "in".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ComparisonOperatorDefinition::LessThan => {
                if comparison_operators.lt_operator.is_none() {
                    comparison_operators.lt_operator = Some(DataConnectorOperatorName::new(
                        operator_name.inner().clone(),
                    ));
                } else {
                    issues.push(ObjectTypesIssue::DuplicateOperatorsDefined {
                        scalar_type: scalar_type_name.clone(),
                        operator_name: "less than".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ComparisonOperatorDefinition::LessThanOrEqual => {
                if comparison_operators.lte_operator.is_none() {
                    comparison_operators.lte_operator = Some(DataConnectorOperatorName::new(
                        operator_name.inner().clone(),
                    ));
                } else {
                    issues.push(ObjectTypesIssue::DuplicateOperatorsDefined {
                        scalar_type: scalar_type_name.clone(),
                        operator_name: "less than or equal to".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ComparisonOperatorDefinition::GreaterThan => {
                if comparison_operators.gt_operator.is_none() {
                    comparison_operators.gt_operator = Some(DataConnectorOperatorName::new(
                        operator_name.inner().clone(),
                    ));
                } else {
                    issues.push(ObjectTypesIssue::DuplicateOperatorsDefined {
                        scalar_type: scalar_type_name.clone(),
                        operator_name: "greater than".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ComparisonOperatorDefinition::GreaterThanOrEqual => {
                if comparison_operators.gte_operator.is_none() {
                    comparison_operators.gte_operator = Some(DataConnectorOperatorName::new(
                        operator_name.inner().clone(),
                    ));
                } else {
                    issues.push(ObjectTypesIssue::DuplicateOperatorsDefined {
                        scalar_type: scalar_type_name.clone(),
                        operator_name: "greater than or equal to".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ComparisonOperatorDefinition::Contains => {
                if comparison_operators.contains_operator.is_none() {
                    comparison_operators.contains_operator = Some(DataConnectorOperatorName::new(
                        operator_name.inner().clone(),
                    ));
                } else {
                    issues.push(ObjectTypesIssue::DuplicateOperatorsDefined {
                        scalar_type: scalar_type_name.clone(),
                        operator_name: "contains".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ComparisonOperatorDefinition::ContainsInsensitive => {
                if comparison_operators.icontains_operator.is_none() {
                    comparison_operators.icontains_operator = Some(DataConnectorOperatorName::new(
                        operator_name.inner().clone(),
                    ));
                } else {
                    issues.push(ObjectTypesIssue::DuplicateOperatorsDefined {
                        scalar_type: scalar_type_name.clone(),
                        operator_name: "contains insensitive".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ComparisonOperatorDefinition::StartsWith => {
                if comparison_operators.starts_with_operator.is_none() {
                    comparison_operators.starts_with_operator = Some(
                        DataConnectorOperatorName::new(operator_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateOperatorsDefined {
                        scalar_type: scalar_type_name.clone(),
                        operator_name: "starts with".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ComparisonOperatorDefinition::StartsWithInsensitive => {
                if comparison_operators.istarts_with_operator.is_none() {
                    comparison_operators.istarts_with_operator = Some(
                        DataConnectorOperatorName::new(operator_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateOperatorsDefined {
                        scalar_type: scalar_type_name.clone(),
                        operator_name: "starts with insensitive".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ComparisonOperatorDefinition::EndsWith => {
                if comparison_operators.ends_with_operator.is_none() {
                    comparison_operators.ends_with_operator = Some(DataConnectorOperatorName::new(
                        operator_name.inner().clone(),
                    ));
                } else {
                    issues.push(ObjectTypesIssue::DuplicateOperatorsDefined {
                        scalar_type: scalar_type_name.clone(),
                        operator_name: "ends with".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ComparisonOperatorDefinition::EndsWithInsensitive => {
                if comparison_operators.iends_with_operator.is_none() {
                    comparison_operators.iends_with_operator = Some(
                        DataConnectorOperatorName::new(operator_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateOperatorsDefined {
                        scalar_type: scalar_type_name.clone(),
                        operator_name: "ends with insensitive".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }

            ndc_models::ComparisonOperatorDefinition::Custom { argument_type: _ } => {
                comparison_operators
                    .other_operators
                    .push(DataConnectorOperatorName::new(
                        operator_name.inner().clone(),
                    ));
            }
        };
    }
    (comparison_operators, issues)
}

pub(crate) fn make_aggregate_functions(
    scalar_type_name: &ndc_models::ScalarTypeName,
    scalar_type: &ndc_models::ScalarType,
    data_connector_name: &Qualified<DataConnectorName>,
) -> (AggregateFunctions, Vec<ObjectTypesIssue>) {
    let mut aggregate_functions = AggregateFunctions::default();
    let mut issues = Vec::new();

    for (function_name, definition) in &scalar_type.aggregate_functions {
        match definition {
            ndc_models::AggregateFunctionDefinition::Sum { result_type: _ } => {
                if aggregate_functions.sum_function.is_none() {
                    aggregate_functions.sum_function = Some(
                        DataConnectorAggregationFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateAggregateFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "sum".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::AggregateFunctionDefinition::Min => {
                if aggregate_functions.min_function.is_none() {
                    aggregate_functions.min_function = Some(
                        DataConnectorAggregationFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateAggregateFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "minimum".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::AggregateFunctionDefinition::Max => {
                if aggregate_functions.max_function.is_none() {
                    aggregate_functions.max_function = Some(
                        DataConnectorAggregationFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateAggregateFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "maximum".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::AggregateFunctionDefinition::Average { result_type: _ } => {
                if aggregate_functions.avg_function.is_none() {
                    aggregate_functions.avg_function = Some(
                        DataConnectorAggregationFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateAggregateFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "average".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::AggregateFunctionDefinition::Custom { result_type: _ } => {
                aggregate_functions.other_functions.push(
                    DataConnectorAggregationFunctionName::new(function_name.inner().clone()),
                );
            }
        }
    }

    (aggregate_functions, issues)
}

fn make_extraction_functions(
    scalar_type_name: &ndc_models::ScalarTypeName,
    scalar_type: &ndc_models::ScalarType,
    data_connector_name: &Qualified<DataConnectorName>,
) -> (ExtractionFunctions, Vec<ObjectTypesIssue>) {
    let mut extraction_functions = ExtractionFunctions::default();
    let mut issues = Vec::new();

    for (function_name, definition) in &scalar_type.extraction_functions {
        match definition {
            ndc_models::ExtractionFunctionDefinition::Year { result_type: _ } => {
                if extraction_functions.year_function.is_none() {
                    extraction_functions.year_function = Some(
                        DataConnectorExtractionFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateExtractionFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "year".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ExtractionFunctionDefinition::Month { result_type: _ } => {
                if extraction_functions.month_function.is_none() {
                    extraction_functions.month_function = Some(
                        DataConnectorExtractionFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateAggregateFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "month".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ExtractionFunctionDefinition::Day { result_type: _ } => {
                if extraction_functions.day_function.is_none() {
                    extraction_functions.day_function = Some(
                        DataConnectorExtractionFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateAggregateFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "day".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ExtractionFunctionDefinition::Nanosecond { result_type: _ } => {
                if extraction_functions.nanosecond_function.is_none() {
                    extraction_functions.nanosecond_function = Some(
                        DataConnectorExtractionFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateAggregateFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "nanosecond".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ExtractionFunctionDefinition::Microsecond { result_type: _ } => {
                if extraction_functions.microsecond_function.is_none() {
                    extraction_functions.microsecond_function = Some(
                        DataConnectorExtractionFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateAggregateFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "microsecond".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ExtractionFunctionDefinition::Millisecond { result_type: _ } => {
                if extraction_functions.millisecond_function.is_none() {
                    extraction_functions.millisecond_function = Some(
                        DataConnectorExtractionFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateAggregateFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "millisecond".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ExtractionFunctionDefinition::Second { result_type: _ } => {
                if extraction_functions.second_function.is_none() {
                    extraction_functions.second_function = Some(
                        DataConnectorExtractionFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateAggregateFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "second".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ExtractionFunctionDefinition::Minute { result_type: _ } => {
                if extraction_functions.minute_function.is_none() {
                    extraction_functions.minute_function = Some(
                        DataConnectorExtractionFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateAggregateFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "minute".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ExtractionFunctionDefinition::Hour { result_type: _ } => {
                if extraction_functions.hour_function.is_none() {
                    extraction_functions.hour_function = Some(
                        DataConnectorExtractionFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateAggregateFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "hour".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ExtractionFunctionDefinition::Week { result_type: _ } => {
                if extraction_functions.week_function.is_none() {
                    extraction_functions.week_function = Some(
                        DataConnectorExtractionFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateAggregateFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "week".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ExtractionFunctionDefinition::Quarter { result_type: _ } => {
                if extraction_functions.quarter_function.is_none() {
                    extraction_functions.quarter_function = Some(
                        DataConnectorExtractionFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateAggregateFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "quarter".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ExtractionFunctionDefinition::DayOfWeek { result_type: _ } => {
                if extraction_functions.day_of_week_function.is_none() {
                    extraction_functions.day_of_week_function = Some(
                        DataConnectorExtractionFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateAggregateFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "day_of_week".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ExtractionFunctionDefinition::DayOfYear { result_type: _ } => {
                if extraction_functions.day_of_year_function.is_none() {
                    extraction_functions.day_of_year_function = Some(
                        DataConnectorExtractionFunctionName::new(function_name.inner().clone()),
                    );
                } else {
                    issues.push(ObjectTypesIssue::DuplicateAggregateFunctionsDefined {
                        scalar_type: scalar_type_name.clone(),
                        function_name: "day_of_year".to_string(),
                        data_connector_name: data_connector_name.clone(),
                    });
                }
            }
            ndc_models::ExtractionFunctionDefinition::Custom { result_type: _ } => {
                extraction_functions.other_functions.push(
                    DataConnectorExtractionFunctionName::new(function_name.inner().clone()),
                );
            }
        }
    }

    (extraction_functions, issues)
}

fn get_column<'a>(
    ndc_type: &'a ndc_models::ObjectType,
    field_name: &open_dds::types::FieldName,
    column: &DataConnectorColumnName,
) -> Result<&'a ndc_models::ObjectField, TypeMappingValidationError> {
    ndc_type
        .fields
        .get(column.as_str())
        .ok_or(TypeMappingValidationError::UnknownTargetColumn {
            field_name: field_name.clone(),
            column_name: column.to_string(),
        })
}
