mod error;
pub mod types;

pub use error::{ObjectTypesError, TypeMappingValidationError};
use open_dds::identifier::SubgraphName;
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use types::ComparisonOperators;

use open_dds::commands::ArgumentMapping;
use open_dds::{data_connector::DataConnectorColumnName, types::CustomTypeName};
pub use types::{
    DataConnectorTypeMappingsForObject, FieldArgumentInfo, FieldDefinition, FieldMapping,
    ObjectTypeRepresentation, ObjectTypeWithTypeMappings, ObjectTypesOutput,
    ObjectTypesWithTypeMappings, ResolvedApolloFederationObjectKey,
    ResolvedObjectApolloFederationConfig, TypeMapping,
};

use crate::helpers::ndc_validation::get_underlying_named_type;
use crate::helpers::types::{mk_name, store_new_graphql_type};
use crate::stages::{apollo, data_connectors};

use crate::types::subgraph::{mk_qualified_type_reference, Qualified};

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;

use super::data_connector_scalar_types::get_comparison_operators;

/// resolve object types, matching them to that in the data connectors
pub(crate) fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connectors::DataConnectors,
) -> Result<ObjectTypesOutput, ObjectTypesError> {
    let mut object_types = BTreeMap::new();
    let mut graphql_types = BTreeSet::new();
    let mut global_id_enabled_types = BTreeMap::new();
    let mut apollo_federation_entity_enabled_types = BTreeMap::new();

    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: object_type_definition,
    } in &metadata_accessor.object_types
    {
        let qualified_object_type_name =
            Qualified::new(subgraph.clone(), object_type_definition.name.clone());

        let resolved_object_type = resolve_object_type(
            object_type_definition,
            &mut graphql_types,
            &qualified_object_type_name,
            subgraph,
            &mut global_id_enabled_types,
            &mut apollo_federation_entity_enabled_types,
        )?;

        let mut type_mappings = DataConnectorTypeMappingsForObject::new();

        // resolve object types' type mappings
        for dc_type_mapping in &object_type_definition.data_connector_type_mapping {
            let qualified_data_connector_name = Qualified::new(
                subgraph.clone(),
                dc_type_mapping.data_connector_name.clone(),
            );
            let type_mapping = resolve_data_connector_type_mapping(
                dc_type_mapping,
                &qualified_object_type_name,
                subgraph,
                &resolved_object_type,
                data_connectors,
            )
            .map_err(|type_validation_error| {
                ObjectTypesError::DataConnectorTypeMappingValidationError {
                    type_name: qualified_object_type_name.clone(),
                    error: type_validation_error,
                }
            })?;
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
                name: qualified_object_type_name,
            });
        }
    }

    Ok(ObjectTypesOutput {
        graphql_types,
        global_id_enabled_types,
        apollo_federation_entity_enabled_types,
        object_types: ObjectTypesWithTypeMappings(object_types),
    })
}

fn resolve_field(
    field: &open_dds::types::FieldDefinition,
    subgraph: &SubgraphName,
    qualified_type_name: &Qualified<CustomTypeName>,
) -> Result<FieldDefinition, ObjectTypesError> {
    let mut field_arguments = IndexMap::new();
    for argument in &field.arguments {
        let field_argument_definition = FieldArgumentInfo {
            argument_type: mk_qualified_type_reference(&argument.argument_type, subgraph),
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
        field_type: mk_qualified_type_reference(&field.field_type, subgraph),
        description: field.description.clone(),
        deprecated: field.deprecated.clone(),
        field_arguments,
    })
}

pub fn resolve_object_type(
    object_type_definition: &open_dds::types::ObjectTypeV1,
    existing_graphql_types: &mut BTreeSet<ast::TypeName>,
    qualified_type_name: &Qualified<CustomTypeName>,
    subgraph: &SubgraphName,
    global_id_enabled_types: &mut BTreeMap<
        Qualified<CustomTypeName>,
        Vec<Qualified<open_dds::models::ModelName>>,
    >,
    apollo_federation_entity_enabled_types: &mut BTreeMap<
        Qualified<CustomTypeName>,
        Option<Qualified<open_dds::models::ModelName>>,
    >,
) -> Result<ObjectTypeRepresentation, ObjectTypesError> {
    let mut resolved_fields = IndexMap::new();
    let mut resolved_global_id_fields = Vec::new();

    for field in &object_type_definition.fields {
        if resolved_fields
            .insert(
                field.name.clone(),
                resolve_field(field, subgraph, qualified_type_name)?,
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

    store_new_graphql_type(existing_graphql_types, graphql_type_name.as_ref())?;
    store_new_graphql_type(existing_graphql_types, graphql_input_type_name.as_ref())?;

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
    subgraph: &SubgraphName,
    type_representation: &ObjectTypeRepresentation,
    data_connectors: &data_connectors::DataConnectors,
) -> Result<TypeMapping, TypeMappingValidationError> {
    let qualified_data_connector_name = Qualified::new(
        subgraph.clone(),
        data_connector_type_mapping.data_connector_name.clone(),
    );

    let data_connector_context = data_connectors
        .0
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
    for field_name in type_representation.fields.keys() {
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
        let underlying_column_type = get_underlying_named_type(&source_column.r#type);
        let scalar_type = data_connector_context
            .schema
            .scalar_types
            .get(underlying_column_type.as_str());

        let column_type_representation = scalar_type.and_then(|ty| ty.representation.clone());

        let comparison_operators = scalar_type.map(|ty| {
            let c = get_comparison_operators(ty);
            ComparisonOperators {
                equality_operators: c.equal_operators,
                in_operators: c.in_operators,
                other_operators: c.other_operators,
            }
        });

        let resolved_field_mapping = FieldMapping {
            column: resolved_field_mapping_column.into_owned(),
            column_type: source_column.r#type.clone(),
            column_type_representation,
            comparison_operators,
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

    Ok(resolved_type_mapping)
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
