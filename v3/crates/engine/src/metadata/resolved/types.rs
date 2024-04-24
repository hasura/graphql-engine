use super::stages::{
    data_connector_scalar_types, data_connector_type_mappings, graphql_config, scalar_types,
    type_permissions,
};
use crate::metadata::resolved::boolean_expression;
use crate::metadata::resolved::data_connector;
use crate::metadata::resolved::error::{BooleanExpressionError, Error};

use crate::metadata::resolved::subgraph::{
    mk_qualified_type_reference, Qualified, QualifiedBaseType, QualifiedTypeName,
    QualifiedTypeReference,
};
use lang_graphql::ast::common as ast;
use ndc_models;
use open_dds::data_connector::DataConnectorName;
use open_dds::models::EnableAllOrSpecific;
use open_dds::types::{self, CustomTypeName, FieldName, ObjectBooleanExpressionTypeV1};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::str::FromStr;

use super::ndc_validation::{get_underlying_named_type, NDCValidationError};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct NdcColumnForComparison {
    pub column: String,
    pub equal_operator: String,
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
    pub data_connector_link: data_connector::DataConnectorLink,
    pub data_connector_object_type: String,
    pub type_mappings: BTreeMap<Qualified<types::CustomTypeName>, TypeMapping>,
    pub graphql: Option<boolean_expression::BooleanExpression>,
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
) -> Result<data_connector_type_mappings::FieldDefinition, Error> {
    Ok(data_connector_type_mappings::FieldDefinition {
        field_type: mk_qualified_type_reference(&field.field_type, subgraph),
        description: field.description.clone(),
        deprecated: field.deprecated.clone(),
    })
}

/*
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
) -> Result<data_connector_type_mappings::ObjectTypeRepresentation, Error> {
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
                        let mut resolved_keys: Vec<
                            data_connector_type_mappings::ResolvedApolloFederationObjectKey,
                        > = Vec::new();
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
                                    Some(fields) => data_connector_type_mappings::ResolvedApolloFederationObjectKey { fields },
                                };
                            resolved_keys.push(resolved_key);
                        }
                        apollo_federation_entity_enabled_types
                            .insert(qualified_type_name.clone(), None);
                        match nonempty::NonEmpty::from_vec(resolved_keys) {
                            None => Err(Error::EmptyKeysInApolloFederationConfigForObject {
                                object_type: qualified_type_name.clone(),
                            }),
                            Some(keys) => Ok(Some(data_connector_type_mappings
                                    ::ResolvedObjectApolloFederationConfig { keys })),
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

    Ok(data_connector_type_mappings::ObjectTypeRepresentation {
        fields: resolved_fields,
        relationships: IndexMap::new(),
        global_id_fields: resolved_global_id_fields,
        type_output_permissions: HashMap::new(),
        type_input_permissions: HashMap::new(),
        graphql_output_type_name: graphql_type_name,
        graphql_input_type_name,
        description: object_type_definition.description.clone(),
        apollo_federation_config,
    })
}
*/

#[derive(Debug)]
/// we do not want to store our types like this, but occasionally it is useful
/// for pattern matching
pub enum TypeRepresentation<'a> {
    Scalar(&'a scalar_types::ScalarTypeRepresentation),
    Object(&'a type_permissions::ObjectTypeWithPermissions),
}

/// validate whether a given CustomTypeName exists within `object_types` or `scalar_types`
pub fn get_type_representation<'a>(
    custom_type_name: &Qualified<CustomTypeName>,
    object_types: &'a HashMap<
        Qualified<CustomTypeName>,
        type_permissions::ObjectTypeWithPermissions,
    >,
    scalar_types: &'a HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
) -> Result<TypeRepresentation<'a>, Error> {
    match object_types.get(custom_type_name) {
        Some(object_type_representation) => {
            Ok(TypeRepresentation::Object(object_type_representation))
        }
        None => match scalar_types.get(custom_type_name) {
            Some(scalar_type_representation) => {
                Ok(TypeRepresentation::Scalar(scalar_type_representation))
            }
            None => Err(Error::UnknownType {
                data_type: custom_type_name.clone(),
            }),
        },
    }
}

// check that `custom_type_name` exists in `object_types`
pub fn get_underlying_object_type(
    custom_type_name: &Qualified<CustomTypeName>,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
) -> Result<Qualified<CustomTypeName>, Error> {
    object_types
        .get(custom_type_name)
        .map(|_| custom_type_name.clone())
        .ok_or_else(|| Error::UnknownObjectType {
            data_type: custom_type_name.clone(),
        })
}

// check that `custom_type_name` exists in `scalar_types`
pub fn get_underlying_scalar_type(
    custom_type_name: &Qualified<CustomTypeName>,
    scalar_types: &HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
) -> Result<Qualified<CustomTypeName>, Error> {
    scalar_types
        .get(custom_type_name)
        .map(|_| custom_type_name.clone())
        .ok_or_else(|| Error::UnknownScalarType {
            data_type: custom_type_name.clone(),
        })
}

/// given a type like `Thing!` or `[Thing!]!` - try and extract `Thing`
pub fn unwrap_custom_type_name(
    type_reference: &QualifiedTypeReference,
) -> Option<&Qualified<CustomTypeName>> {
    match &type_reference.underlying_type {
        QualifiedBaseType::List(inner_type) => unwrap_custom_type_name(inner_type),
        QualifiedBaseType::Named(type_name) => match type_name {
            QualifiedTypeName::Inbuilt(_) => None,
            QualifiedTypeName::Custom(custom_type_name) => Some(custom_type_name),
        },
    }
}

/// Resolves a given object boolean expression type
pub(crate) fn resolve_object_boolean_expression_type(
    object_boolean_expression: &ObjectBooleanExpressionTypeV1,
    subgraph: &str,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    data_connector_type_mappings: &data_connector_type_mappings::DataConnectorTypeMappings,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    existing_graphql_types: &mut HashSet<ast::TypeName>,
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
                    boolean_expression_type: qualified_name.clone(),
                },
            )
        })?;

    // validate data connector object type
    if !data_connector_context
        .inner
        .schema
        .object_types
        .contains_key(&object_boolean_expression.data_connector_object_type)
    {
        return Err(Error::from(
            BooleanExpressionError::UnknownDataConnectorTypeInObjectBooleanExpressionType {
                data_connector: qualified_data_connector_name.clone(),
                boolean_expression_type: qualified_name.clone(),
                data_connector_object_type: object_boolean_expression
                    .data_connector_object_type
                    .clone(),
            },
        ));
    }

    data_connector_type_mappings
                .get(
                    &qualified_object_type_name,
                    &qualified_data_connector_name,
                    &object_boolean_expression.data_connector_object_type,
                )
                .ok_or_else(|| {
                    Error::from(BooleanExpressionError::NoDataConnectorTypeMappingForObjectTypeInBooleanExpression {
                        object_type: qualified_object_type_name.clone(),
                        boolean_expression_type: qualified_name.clone(),
                        data_connector_object_type: object_boolean_expression
                            .data_connector_object_type
                            .clone(),
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
                    boolean_expression_type: qualified_name.clone(),
                }
                .into(),
            );
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
        != object_type_representation.object_type.fields.len()
    {
        return Err(Error::UnsupportedFeature {
                    message: "Field level comparison operator configuration is not fully supported yet. Please add all fields in filterable_fields.".to_string(),
                });
    }

    let boolean_expression_type =
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

    let type_mapping_to_collect = TypeMappingToCollect {
        type_name: &object_type,
        ndc_object_type_name: object_boolean_expression
            .data_connector_object_type
            .as_str(),
    };
    collect_type_mapping_for_source(
        &type_mapping_to_collect,
        data_connector_type_mappings,
        &qualified_data_connector_name,
        object_types,
        scalar_types,
        &mut type_mappings,
    )
    .map_err(|error| {
        Error::from(
            BooleanExpressionError::BooleanExpressionTypeMappingCollectionError {
                boolean_expression_type: boolean_expression_type.clone(),
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

            boolean_expression::resolve_boolean_expression(
                &boolean_expression_type,
                &data_connector_name,
                graphql_type_name.clone(),
                subgraph,
                data_connectors,
                type_mapping,
                graphql_config,
            )
        })
        .transpose()?;

    let data_connector_link = data_connector::DataConnectorLink::new(
        data_connector_name,
        data_connector_context.inner.url.clone(),
        data_connector_context.inner.headers,
    )?;

    let resolved_boolean_expression = ObjectBooleanExpressionType {
        name: qualified_name.clone(),
        type_mappings,
        object_type: qualified_object_type_name.clone(),
        data_connector_name: qualified_data_connector_name,
        data_connector_link,
        data_connector_object_type: object_boolean_expression.data_connector_object_type.clone(),
        graphql: boolean_expression_graphql_config,
    };
    Ok(resolved_boolean_expression)
}

/// Helper function to create GraphQL compliant name
pub fn mk_name(name: &str) -> Result<ast::Name, Error> {
    ast::Name::from_str(name).map_err(|_| Error::InvalidGraphQlName {
        name: name.to_string(),
    })
}

#[derive(Debug)]
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
    data_connector_type_mappings: &data_connector_type_mappings::DataConnectorTypeMappings,
    data_connector_name: &Qualified<DataConnectorName>,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
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

    match object_types.get(mapping_to_collect.type_name) {
        Some(object_type_representation) => {
            let TypeMapping::Object { field_mappings, .. } = type_mapping;
            // For each field in the ObjectType, if that field is using an ObjectType in its type,
            // resolve the type mappings for that ObjectType too
            for (field_name, field_definition) in &object_type_representation.object_type.fields {
                let field_mapping = field_mappings.get(field_name).ok_or_else(|| {
                    TypeMappingCollectionError::MissingFieldMapping {
                        type_name: mapping_to_collect.type_name.clone(),
                        field_name: field_name.clone(),
                        data_connector: data_connector_name.clone(),
                        ndc_type_name: mapping_to_collect.ndc_object_type_name.to_string(),
                    }
                })?;

                if let Some(object_type_name) =
                    unwrap_custom_type_name(&field_definition.field_type)
                {
                    match get_type_representation(object_type_name, object_types, scalar_types)
                        .map_err(|_| TypeMappingCollectionError::InternalUnknownType {
                            type_name: object_type_name.clone(),
                        })? {
                        TypeRepresentation::Object(_) => {
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
                                object_types,
                                scalar_types,
                                collected_mappings,
                            )?;
                        }
                        TypeRepresentation::Scalar(_) => {}
                    }
                }
            }
            Ok(())
        }
        None => match scalar_types.get(mapping_to_collect.type_name) {
            Some(_) => Ok(()),
            None => Err(TypeMappingCollectionError::InternalUnknownType {
                type_name: mapping_to_collect.type_name.clone(),
            }),
        },
    }?;

    Ok(())
}
