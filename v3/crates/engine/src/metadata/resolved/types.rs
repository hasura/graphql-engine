use super::stages::{
    boolean_expressions, data_connector_type_mappings, scalar_types, type_permissions,
};
use crate::metadata::resolved::error::{BooleanExpressionError, Error};

use crate::metadata::resolved::subgraph::{
    mk_qualified_type_reference, Qualified, QualifiedBaseType, QualifiedTypeName,
    QualifiedTypeReference,
};
use lang_graphql::ast::common as ast;
use open_dds::data_connector::DataConnectorName;
use open_dds::types::{self, CustomTypeName, FieldName};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::str::FromStr;

use super::ndc_validation::{get_underlying_named_type, NDCValidationError};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct NdcColumnForComparison {
    pub column: String,
    pub equal_operator: String,
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

#[derive(Debug)]
/// we do not want to store our types like this, but occasionally it is useful
/// for pattern matching
pub enum TypeRepresentation<'a> {
    Scalar(&'a scalar_types::ScalarTypeRepresentation),
    Object(&'a type_permissions::ObjectTypeWithPermissions),
    BooleanExpression(&'a boolean_expressions::ObjectBooleanExpressionType),
}

/// validate whether a given CustomTypeName exists within `object_types`, `scalar_types` or
/// `boolean_expression_types`
pub fn get_type_representation<'a>(
    custom_type_name: &Qualified<CustomTypeName>,
    object_types: &'a HashMap<
        Qualified<CustomTypeName>,
        type_permissions::ObjectTypeWithPermissions,
    >,
    scalar_types: &'a HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &'a HashMap<
        Qualified<CustomTypeName>,
        boolean_expressions::ObjectBooleanExpressionType,
    >,
) -> Result<TypeRepresentation<'a>, Error> {
    match object_types.get(custom_type_name) {
        Some(object_type_representation) => {
            Ok(TypeRepresentation::Object(object_type_representation))
        }
        None => match scalar_types.get(custom_type_name) {
            Some(scalar_type_representation) => {
                Ok(TypeRepresentation::Scalar(scalar_type_representation))
            }
            None => match boolean_expression_types.get(custom_type_name) {
                Some(boolean_expression_type) => Ok(TypeRepresentation::BooleanExpression(
                    boolean_expression_type,
                )),
                None => Err(Error::UnknownType {
                    data_type: custom_type_name.clone(),
                }),
            },
        },
    }
}

pub(crate) fn get_object_type_for_boolean_expression<'a>(
    boolean_expression_type: &boolean_expressions::ObjectBooleanExpressionType,
    object_types: &'a HashMap<
        Qualified<CustomTypeName>,
        type_permissions::ObjectTypeWithPermissions,
    >,
) -> Result<&'a type_permissions::ObjectTypeWithPermissions, Error> {
    object_types
        .get(&boolean_expression_type.object_type)
        .ok_or(Error::from(
            BooleanExpressionError::UnsupportedTypeInObjectBooleanExpressionType {
                type_name: boolean_expression_type.object_type.clone(),
            },
        ))
}

// Get the underlying object type by resolving Custom ObjectType, Array and
// Nullable container types
// check that `custom_type_name` exists in `object_types`
pub fn object_type_exists(
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
    collected_mappings: &mut BTreeMap<
        Qualified<CustomTypeName>,
        data_connector_type_mappings::TypeMapping,
    >,
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
        let data_connector_type_mappings::TypeMapping::Object {
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
            let data_connector_type_mappings::TypeMapping::Object { field_mappings, .. } =
                type_mapping;
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
                    if object_type_exists(object_type_name, object_types).is_ok() {
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
