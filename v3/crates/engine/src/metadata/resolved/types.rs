use crate::metadata::resolved::error::{Error, TypeMappingValidationError};
use crate::metadata::resolved::relationship::Relationship;
use crate::metadata::resolved::subgraph::{
    mk_qualified_type_reference, Qualified, QualifiedBaseType, QualifiedTypeName,
    QualifiedTypeReference,
};

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use ndc_client::models as ndc_models;
use open_dds::data_connector::DataConnectorName;
use open_dds::identifier;
use open_dds::models::EnableAllOrSpecific;
use open_dds::permissions::{Role, TypeOutputPermission, TypePermissionsV1};
use open_dds::types::{
    self, CustomTypeName, DataConnectorTypeMapping, Deprecated, FieldName,
    ObjectBooleanExpressionTypeV1, ObjectTypeV1, TypeName,
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::str::FromStr;

use super::data_connector::DataConnectorContext;
use super::metadata::DataConnectorTypeMappings;
use super::ndc_validation::{get_underlying_named_type, NDCValidationError};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, derive_more::Display)]
pub enum TypeRepresentation {
    Object(ObjectTypeRepresentation),
    #[display(fmt = "ScalarType")]
    ScalarType(ScalarTypeRepresentation),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, derive_more::Display)]
#[display(fmt = "Display")]
pub struct ScalarTypeRepresentation {
    pub graphql_type_name: Option<ast::TypeName>,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, derive_more::Display)]
#[display(fmt = "Display")]
pub struct ObjectTypeRepresentation {
    pub fields: IndexMap<FieldName, FieldDefinition>,
    pub relationships: IndexMap<ast::Name, Relationship>,
    pub type_permissions: HashMap<Role, TypeOutputPermission>,
    pub global_id_fields: Vec<FieldName>,
    pub apollo_federation_config: Option<ResolvedObjectApolloFederationConfig>,
    pub graphql_output_type_name: Option<ast::TypeName>,
    pub graphql_input_type_name: Option<ast::TypeName>,
    pub description: Option<String>,
    // TODO: add graphql_output_type_kind if we support creating interfaces.
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct NdcColumnForComparison {
    pub column: String,
    pub equal_operator: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct ComparisonOperators {
    pub equal_operators: Vec<String>,
    pub in_operators: Vec<String>,
}

pub struct ScalarTypeInfo<'a> {
    pub scalar_type: &'a ndc_models::ScalarType,
    pub representation: Option<TypeName>,
    pub comparison_expression_name: Option<ast::TypeName>,
    pub comparison_operators: ComparisonOperators,
}

impl<'a> ScalarTypeInfo<'a> {
    pub(crate) fn new(source_scalar: &'a ndc_models::ScalarType) -> Self {
        let mut comparison_operators = ComparisonOperators::default();
        for (operator_name, operator_definition) in &source_scalar.comparison_operators {
            match operator_definition {
                ndc_models::ComparisonOperatorDefinition::Equal => {
                    comparison_operators
                        .equal_operators
                        .push(operator_name.clone());
                }
                ndc_models::ComparisonOperatorDefinition::In => {
                    comparison_operators
                        .in_operators
                        .push(operator_name.clone());
                }
                ndc_models::ComparisonOperatorDefinition::Custom { argument_type: _ } => {}
            };
        }
        ScalarTypeInfo {
            scalar_type: source_scalar,
            representation: None,
            comparison_expression_name: None,
            comparison_operators,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct FieldDefinition {
    pub field_type: QualifiedTypeReference,
    pub description: Option<String>,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FieldMapping {
    pub column: String,
    pub column_type: ndc_models::Type,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum TypeMapping {
    Object {
        ndc_object_type_name: String,
        field_mappings: BTreeMap<FieldName, FieldMapping>,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectBooleanExpressionType {
    pub name: Qualified<CustomTypeName>,
    pub object_type: Qualified<CustomTypeName>,
    pub data_connector_name: Qualified<DataConnectorName>,
    pub data_connector_object_type: String,
    pub graphql: Option<ObjectBooleanExpressionTypeGraphQlConfiguration>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectBooleanExpressionTypeGraphQlConfiguration {
    pub type_name: ast::TypeName,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, derive_more::Display)]
#[display(fmt = "Display")]
pub struct ResolvedObjectApolloFederationConfig {
    pub keys: nonempty::NonEmpty<ResolvedApolloFederationObjectKey>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, derive_more::Display)]
#[display(fmt = "Display")]
pub struct ResolvedApolloFederationObjectKey {
    pub fields: nonempty::NonEmpty<FieldName>,
}

/// try to add `new_graphql_type` to `existing_graphql_types`, returning an error
/// if there is a name conflict
pub fn store_new_graphql_type(
    existing_graphql_types: &mut HashSet<ast::TypeName>,
    new_graphql_type: Option<&ast::TypeName>,
) -> Result<(), Error> {
    if let Some(new_graphql_type) = new_graphql_type {
        // Fail on conflicting graphql type names
        if !(existing_graphql_types.insert(new_graphql_type.clone())) {
            return Err(Error::ConflictingGraphQlType {
                graphql_type_name: new_graphql_type.clone(),
            });
        }
    }
    Ok(())
}

pub fn resolve_field(
    field: &types::FieldDefinition,
    subgraph: &str,
) -> Result<FieldDefinition, Error> {
    Ok(FieldDefinition {
        field_type: mk_qualified_type_reference(&field.field_type, subgraph),
        description: field.description.clone(),
        deprecated: field.deprecated.clone(),
    })
}

pub fn resolve_object_type(
    object_type_definition: &ObjectTypeV1,
    existing_graphql_types: &mut HashSet<ast::TypeName>,
    qualified_type_name: &Qualified<CustomTypeName>,
    subgraph: &str,
    global_id_enabled_types: &mut HashMap<
        Qualified<CustomTypeName>,
        Vec<Qualified<open_dds::models::ModelName>>,
    >,
    apollo_federation_entity_enabled_types: &mut HashMap<
        Qualified<CustomTypeName>,
        Option<Qualified<open_dds::models::ModelName>>,
    >,
) -> Result<TypeRepresentation, Error> {
    let mut resolved_fields = IndexMap::new();
    let mut resolved_global_id_fields = Vec::new();

    for field in &object_type_definition.fields {
        if resolved_fields
            .insert(field.name.clone(), resolve_field(field, subgraph)?)
            .is_some()
        {
            return Err(Error::DuplicateFieldDefinition {
                type_name: qualified_type_name.clone(),
                field_name: field.name.clone(),
            });
        }
    }
    match &object_type_definition.global_id_fields {
        Some(global_id_fields) => {
            if !global_id_fields.is_empty() {
                // Throw error if the object type has a field called id" and has global fields configured.
                // Because, when the global id fields are configured, the `id` field will be auto-generated.
                if resolved_fields.contains_key(&FieldName(identifier!("id"))) {
                    return Err(Error::IdFieldConflictingGlobalId {
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
                if !resolved_fields.contains_key(global_id_field) {
                    return Err(Error::UnknownFieldInGlobalId {
                        field_name: global_id_field.clone(),
                        type_name: qualified_type_name.clone(),
                    });
                } else {
                    resolved_global_id_fields.push(global_id_field.clone())
                }
            }
        }
        None => {}
    }
    let (graphql_type_name, graphql_input_type_name, apollo_federation_config) =
        match object_type_definition.graphql.as_ref() {
            None => Ok::<_, Error>((None, None, None)),
            Some(graphql) => {
                let graphql_type_name = graphql
                    .type_name
                    .as_ref()
                    .map(|type_name| mk_name(type_name.0.as_ref()).map(ast::TypeName))
                    .transpose()?;
                let graphql_input_type_name = graphql
                    .input_type_name
                    .as_ref()
                    .map(|input_type_name| mk_name(input_type_name.0.as_ref()).map(ast::TypeName))
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
                                    return Err(Error::UnknownFieldInApolloFederationKey {
                                        field_name: field.clone(),
                                        object_type: qualified_type_name.clone(),
                                    });
                                }
                                resolved_key_fields.push(field.clone());
                            }
                            let resolved_key =
                                match nonempty::NonEmpty::from_vec(resolved_key_fields) {
                                    None => {
                                        return Err(
                                            Error::EmptyFieldsInApolloFederationConfigForObject {
                                                object_type: qualified_type_name.clone(),
                                            },
                                        )
                                    }
                                    Some(fields) => ResolvedApolloFederationObjectKey { fields },
                                };
                            resolved_keys.push(resolved_key);
                        }
                        apollo_federation_entity_enabled_types
                            .insert(qualified_type_name.clone(), None);
                        match nonempty::NonEmpty::from_vec(resolved_keys) {
                            None => Err(Error::EmptyKeysInApolloFederationConfigForObject {
                                object_type: qualified_type_name.clone(),
                            }),
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

    Ok(TypeRepresentation::Object(ObjectTypeRepresentation {
        fields: resolved_fields,
        relationships: IndexMap::new(),
        global_id_fields: resolved_global_id_fields,
        type_permissions: HashMap::new(),
        graphql_output_type_name: graphql_type_name,
        graphql_input_type_name,
        description: object_type_definition.description.clone(),
        apollo_federation_config,
    }))
}

pub fn get_column<'a>(
    ndc_type: &'a ndc_models::ObjectType,
    field_name: &FieldName,
    column: &str,
) -> Result<&'a ndc_models::ObjectField, TypeMappingValidationError> {
    ndc_type
        .fields
        .get(column)
        .ok_or(TypeMappingValidationError::UnknownTargetColumn {
            field_name: field_name.clone(),
            column_name: column.to_string(),
        })
}

/// Resolve a given data connector type mapping
pub fn resolve_data_connector_type_mapping(
    data_connector_type_mapping: &DataConnectorTypeMapping,
    qualified_type_name: &Qualified<CustomTypeName>,
    subgraph: &str,
    type_representation: &ObjectTypeRepresentation,
    data_connectors: &HashMap<Qualified<DataConnectorName>, DataConnectorContext>,
) -> Result<TypeMapping, TypeMappingValidationError> {
    let qualified_data_connector_name = Qualified::new(
        subgraph.to_string(),
        data_connector_type_mapping.data_connector_name.clone(),
    );
    let data_connector_context = data_connectors
        .get(&qualified_data_connector_name)
        .ok_or_else(|| TypeMappingValidationError::UnknownDataConnector {
            data_connector: qualified_data_connector_name.clone(),
            type_name: qualified_type_name.clone(),
        })?;
    let ndc_object_type = data_connector_context
        .schema
        .object_types
        .get(&data_connector_type_mapping.data_connector_object_type)
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
        .collect::<HashMap<_, _>>();
    let mut resolved_field_mappings = BTreeMap::new();
    for field_name in type_representation.fields.keys() {
        let resolved_field_mapping_column: &str =
            if let Some(field_mapping) = unconsumed_field_mappings.remove(field_name) {
                match field_mapping {
                    types::FieldMapping::Column(column_mapping) => &column_mapping.name,
                }
            } else {
                // If no mapping is defined for a field, implicitly create a mapping
                // with the same column name as the field.
                &field_name.0 .0
            };
        let source_column = get_column(ndc_object_type, field_name, resolved_field_mapping_column)?;
        let resolved_field_mapping = FieldMapping {
            column: resolved_field_mapping_column.to_string(),
            column_type: source_column.r#type.clone(),
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
            .to_string(),
        field_mappings: resolved_field_mappings,
    };

    Ok(resolved_type_mapping)
}

// Get the underlying object type by resolving Custom ObjectType, Array and
// Nullable container types
pub fn get_underlying_object_type(
    output_type: &QualifiedTypeReference,
    types: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
) -> Result<Option<Qualified<CustomTypeName>>, Error> {
    get_underlying_object_type_or_unknown_type(output_type, types)
        .map(|opt| opt.cloned())
        .map_err(|custom_type_name| Error::UnknownDataType {
            data_type: custom_type_name.clone(),
        })
}

pub fn get_underlying_object_type_or_unknown_type<'a>(
    output_type: &'a QualifiedTypeReference,
    types: &'a HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
) -> Result<Option<&'a Qualified<CustomTypeName>>, Qualified<CustomTypeName>> {
    match &output_type.underlying_type {
        QualifiedBaseType::List(output_type) => {
            get_underlying_object_type_or_unknown_type(output_type, types)
        }
        QualifiedBaseType::Named(type_name) => match type_name {
            QualifiedTypeName::Inbuilt(_) => Ok(None),
            QualifiedTypeName::Custom(custom_type_name) => {
                let type_representation = types
                    .get(custom_type_name)
                    .ok_or_else(|| custom_type_name.clone())?;
                match type_representation {
                    TypeRepresentation::ScalarType { .. } => Ok(None),
                    TypeRepresentation::Object { .. } => Ok(Some(custom_type_name)),
                }
            }
        },
    }
}

pub fn resolve_output_type_permission(
    type_representation: &mut TypeRepresentation,
    type_permissions: &TypePermissionsV1,
) -> Result<(), Error> {
    match type_representation {
        TypeRepresentation::ScalarType { .. } => Err(Error::UnsupportedTypeInOutputPermissions {
            type_name: type_permissions.type_name.clone(),
        }),
        TypeRepresentation::Object(object_type_representation) => {
            // validate all the fields definied in output permissions actually
            // exist in this type definition
            for type_permission in &type_permissions.permissions {
                if let Some(output) = &type_permission.output {
                    for field_name in output.allowed_fields.iter() {
                        if !object_type_representation.fields.contains_key(field_name) {
                            return Err(Error::UnknownFieldInOutputPermissionsDefinition {
                                field_name: field_name.clone(),
                                type_name: type_permissions.type_name.clone(),
                            });
                        }
                    }
                    if object_type_representation
                        .type_permissions
                        .insert(type_permission.role.clone(), output.clone())
                        .is_some()
                    {
                        return Err(Error::DuplicateOutputTypePermissions {
                            type_name: type_permissions.type_name.clone(),
                        });
                    }
                }
            }
            Ok(())
        }
    }
}

/// Resolves a given object boolean expression type
pub(crate) fn resolve_object_boolean_expression_type(
    object_boolean_expression: &ObjectBooleanExpressionTypeV1,
    subgraph: &str,
    data_connectors: &HashMap<Qualified<DataConnectorName>, DataConnectorContext>,
    types: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
    data_connector_type_mappings: &DataConnectorTypeMappings,
    existing_graphql_types: &mut HashSet<ast::TypeName>,
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
    let type_representation = types.get(&qualified_object_type_name).ok_or_else(|| {
        Error::UnknownTypeInObjectBooleanExpressionType {
            type_name: qualified_object_type_name.clone(),
        }
    })?;
    match type_representation {
        // validate it should only be an object type
        TypeRepresentation::ScalarType { .. } => {
            Err(Error::UnsupportedTypeInObjectBooleanExpressionType {
                type_name: qualified_name.clone(),
            })
        }
        TypeRepresentation::Object(object_type_representation) => {
            let qualified_data_connector_name = Qualified::new(
                subgraph.to_string(),
                object_boolean_expression.data_connector_name.to_owned(),
            );

            // validate data connector name
            let data_connector_context = data_connectors
                .get(&qualified_data_connector_name)
                .ok_or_else(
                    || Error::UnknownDataConnectorInObjectBooleanExpressionType {
                        data_connector: qualified_data_connector_name.clone(),
                        boolean_expression_type: qualified_name.clone(),
                    },
                )?;

            // validate data connector object type
            if !data_connector_context
                .schema
                .object_types
                .contains_key(&object_boolean_expression.data_connector_object_type)
            {
                return Err(
                    Error::UnknownDataConnectorTypeInObjectBooleanExpressionType {
                        data_connector: qualified_data_connector_name.clone(),
                        boolean_expression_type: qualified_name.clone(),
                        data_connector_object_type: object_boolean_expression
                            .data_connector_object_type
                            .clone(),
                    },
                );
            }

            data_connector_type_mappings
                .get(
                    &qualified_object_type_name,
                    &qualified_data_connector_name,
                    &object_boolean_expression.data_connector_object_type,
                )
                .ok_or_else(|| {
                    Error::NoDataConnectorTypeMappingForObjectTypeInBooleanExpression {
                        object_type: qualified_object_type_name.clone(),
                        boolean_expression_type: qualified_name.clone(),
                        data_connector_object_type: object_boolean_expression
                            .data_connector_object_type
                            .clone(),
                        data_connector: qualified_data_connector_name.clone(),
                    }
                })?;

            // validate comparable fields
            for comparable_field in object_boolean_expression.comparable_fields.iter() {
                if !object_type_representation
                    .fields
                    .contains_key(&comparable_field.field_name)
                {
                    return Err(Error::UnknownFieldInObjectBooleanExpressionType {
                        field_name: comparable_field.field_name.clone(),
                        boolean_expression_type: qualified_name.clone(),
                    });
                }

                // As of now, only `"enableAll": true` is allowed for field operators
                match &comparable_field.operators {
                    EnableAllOrSpecific::EnableAll(true) => {}
                    _ => {
                        return Err(Error::UnsupportedFeature {
                            message: "Field level comparison operator configuration is not fully supported yet. Please use \"enableAll\":true.".to_string(),
                        })
                    }
                }
            }

            // Comparable fields should have all type fields
            if object_boolean_expression.comparable_fields.len()
                != object_type_representation.fields.len()
            {
                return Err(Error::UnsupportedFeature {
                    message: "Field level comparison operator configuration is not fully supported yet. Please add all fields in filterable_fields.".to_string(),
                });
            }

            // validate graphql config
            let graphql_config = object_boolean_expression
                .graphql
                .as_ref()
                .map(|graphql_config| {
                    let graphql_type_name =
                        mk_name(graphql_config.type_name.0.as_ref()).map(ast::TypeName)?;
                    store_new_graphql_type(existing_graphql_types, Some(&graphql_type_name))?;
                    Ok::<_, Error>(ObjectBooleanExpressionTypeGraphQlConfiguration {
                        type_name: graphql_type_name,
                    })
                })
                .transpose()?;

            let resolved_boolean_expression = ObjectBooleanExpressionType {
                name: qualified_name.clone(),
                object_type: qualified_object_type_name.clone(),
                data_connector_name: qualified_data_connector_name,
                data_connector_object_type: object_boolean_expression
                    .data_connector_object_type
                    .clone(),
                graphql: graphql_config,
            };
            Ok(resolved_boolean_expression)
        }
    }
}

/// Helper function to create GraphQL compliant name
pub fn mk_name(name: &str) -> Result<ast::Name, Error> {
    ast::Name::from_str(name).map_err(|_| Error::InvalidGraphQlName {
        name: name.to_string(),
    })
}

pub struct TypeMappingToCollect<'a> {
    pub type_name: &'a Qualified<CustomTypeName>,
    pub ndc_object_type_name: &'a str,
}

#[derive(thiserror::Error, Debug)]
pub enum TypeMappingCollectionError {
    #[error("No mapping defined for type {type_name:} to object {ndc_type_name:} of data connector {data_connector:}")]
    MappingNotDefined {
        type_name: Qualified<CustomTypeName>,
        data_connector: Qualified<DataConnectorName>,
        ndc_type_name: String,
    },
    #[error("No support for using the same type {type_name:} against multiple data connector objects {ndc_type_1:} and {ndc_type_2:}")]
    MappingToMultipleDataConnectorObjectType {
        type_name: Qualified<CustomTypeName>,
        ndc_type_1: String,
        ndc_type_2: String,
    },
    #[error("Missing mapping for field {field_name:} when mapping type {type_name:} to object {ndc_type_name:} of data connector {data_connector:}")]
    MissingFieldMapping {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
        data_connector: Qualified<DataConnectorName>,
        ndc_type_name: String,
    },
    #[error("Internal Error: Unknown type {type_name:} when collecting type mappings")]
    InternalUnknownType {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("ndc validation error: {0}")]
    NDCValidationError(#[from] NDCValidationError),
}

pub(crate) fn collect_type_mapping_for_source(
    mapping_to_collect: &TypeMappingToCollect,
    data_connector_type_mappings: &DataConnectorTypeMappings,
    data_connector_name: &Qualified<DataConnectorName>,
    type_representations: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
    // ndc_object_types: &BTreeMap<String, ndc_models::ObjectType>,
    collected_mappings: &mut BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
) -> Result<(), TypeMappingCollectionError> {
    let type_mapping = data_connector_type_mappings
        .get(
            mapping_to_collect.type_name,
            data_connector_name,
            mapping_to_collect.ndc_object_type_name,
        )
        .ok_or_else(|| TypeMappingCollectionError::MappingNotDefined {
            type_name: mapping_to_collect.type_name.clone(),
            data_connector: data_connector_name.clone(),
            ndc_type_name: mapping_to_collect.ndc_object_type_name.to_string(),
        })?;

    // If there is an existing mapping, make sure it maps to the same NDC object type.
    if let Some(inserted_mapping) =
        collected_mappings.insert(mapping_to_collect.type_name.clone(), type_mapping.clone())
    {
        let TypeMapping::Object {
            ndc_object_type_name,
            ..
        } = inserted_mapping;
        if ndc_object_type_name != mapping_to_collect.ndc_object_type_name {
            return Err(
                TypeMappingCollectionError::MappingToMultipleDataConnectorObjectType {
                    type_name: mapping_to_collect.type_name.clone(),
                    ndc_type_1: ndc_object_type_name,
                    ndc_type_2: mapping_to_collect.ndc_object_type_name.to_string(),
                },
            );
        } else {
            return Ok(());
        }
    }

    let type_representation = type_representations
        .get(mapping_to_collect.type_name)
        .ok_or_else(|| TypeMappingCollectionError::InternalUnknownType {
            type_name: mapping_to_collect.type_name.clone(),
        })?;

    if let TypeRepresentation::Object(object_type_representation) = type_representation {
        let TypeMapping::Object { field_mappings, .. } = type_mapping;
        // For each field in the ObjectType, if that field is using an ObjectType in its type,
        // resolve the type mappings for that ObjectType too
        for (field_name, field_definition) in &object_type_representation.fields {
            let field_mapping = field_mappings.get(field_name).ok_or_else(|| {
                TypeMappingCollectionError::MissingFieldMapping {
                    type_name: mapping_to_collect.type_name.clone(),
                    field_name: field_name.clone(),
                    data_connector: data_connector_name.clone(),
                    ndc_type_name: mapping_to_collect.ndc_object_type_name.to_string(),
                }
            })?;
            if let Some(object_type_name) = get_underlying_object_type_or_unknown_type(
                &field_definition.field_type,
                type_representations,
            )
            .map_err(|unknown_type| {
                TypeMappingCollectionError::InternalUnknownType {
                    type_name: unknown_type,
                }
            })? {
                let underlying_ndc_field_named_type =
                    get_underlying_named_type(&field_mapping.column_type)?;

                let field_type_mapping_to_collect = TypeMappingToCollect {
                    type_name: object_type_name,
                    ndc_object_type_name: underlying_ndc_field_named_type,
                };
                collect_type_mapping_for_source(
                    &field_type_mapping_to_collect,
                    data_connector_type_mappings,
                    data_connector_name,
                    type_representations,
                    collected_mappings,
                )?;
            }
        }
    }

    Ok(())
}
