use crate::stages::{object_types, scalar_types, type_permissions};

use crate::data_connectors::CommandsResponseConfig;
use crate::helpers::ndc_validation::{get_underlying_named_type, NDCValidationError};
use crate::helpers::types::{object_type_exists, unwrap_custom_type_name};
use crate::types::subgraph::Qualified;

use open_dds::data_connector::{DataConnectorName, DataConnectorObjectType};
use open_dds::relationships::RelationshipName;
use open_dds::types::{CustomTypeName, FieldName};

use std::collections::BTreeMap;

#[derive(Debug)]
pub struct TypeMappingToCollect<'a> {
    pub type_name: &'a Qualified<CustomTypeName>,
    pub ndc_object_type_name: &'a ndc_models::TypeName,
}

#[derive(thiserror::Error, Debug)]
pub enum TypeMappingCollectionError {
    #[error("No mapping defined for type {type_name:} to object {ndc_type_name:} of data connector {data_connector:}")]
    MappingNotDefined {
        type_name: Qualified<CustomTypeName>,
        data_connector: Qualified<DataConnectorName>,
        ndc_type_name: DataConnectorObjectType,
    },
    #[error("No support for using the same type {type_name:} against multiple data connector objects {ndc_type_1:} and {ndc_type_2:}")]
    MappingToMultipleDataConnectorObjectType {
        type_name: Qualified<CustomTypeName>,
        ndc_type_1: DataConnectorObjectType,
        ndc_type_2: DataConnectorObjectType,
    },
    #[error("Missing mapping for field {field_name:} when mapping type {type_name:} to object {ndc_type_name:} of data connector {data_connector:}")]
    MissingFieldMapping {
        type_name: Qualified<CustomTypeName>,
        field_name: FieldName,
        data_connector: Qualified<DataConnectorName>,
        ndc_type_name: DataConnectorObjectType,
    },
    #[error("Internal Error: Unknown type {type_name:} when collecting type mappings")]
    InternalUnknownType {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("ndc validation error: {0}")]
    NDCValidationError(#[from] NDCValidationError),
}

// Special case handling for commands with response config; as they don't match
// the ndc type mapping exactly.
#[derive(Debug)]
pub(crate) struct SpecialCaseTypeMapping<'a> {
    pub(crate) response_config: &'a CommandsResponseConfig,
    pub(crate) ndc_object_type: &'a ndc_models::ObjectType,
}

pub(crate) fn collect_type_mapping_for_source(
    mapping_to_collect: &TypeMappingToCollect,
    data_connector_name: &Qualified<DataConnectorName>,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    collected_mappings: &mut BTreeMap<Qualified<CustomTypeName>, object_types::TypeMapping>,
    special_case: Option<&SpecialCaseTypeMapping>,
) -> Result<(), TypeMappingCollectionError> {
    match object_types.get(mapping_to_collect.type_name).ok() {
        Some(object_type_representation) => {
            let type_mapping = match object_type_representation.type_mappings.get(
                data_connector_name,
                mapping_to_collect.ndc_object_type_name.as_str(),
            ) {
                Some(v) => Ok(v),
                None => handle_special_case_type_mapping(
                    mapping_to_collect,
                    data_connector_name,
                    object_type_representation,
                    special_case,
                ),
            }?;

            // If there is an existing mapping, make sure it maps to the same NDC object type.
            if let Some(inserted_mapping) = collected_mappings
                .insert(mapping_to_collect.type_name.clone(), type_mapping.clone())
            {
                let object_types::TypeMapping::Object {
                    ndc_object_type_name,
                    ..
                } = inserted_mapping;
                return if ndc_object_type_name.as_str()
                    == mapping_to_collect.ndc_object_type_name.as_str()
                {
                    Ok(())
                } else {
                    Err(
                        TypeMappingCollectionError::MappingToMultipleDataConnectorObjectType {
                            type_name: mapping_to_collect.type_name.clone(),
                            ndc_type_1: ndc_object_type_name,
                            ndc_type_2: DataConnectorObjectType::from(
                                mapping_to_collect.ndc_object_type_name.as_str(),
                            ),
                        },
                    )
                };
            }

            let object_types::TypeMapping::Object { field_mappings, .. } = type_mapping;
            // For each field in the ObjectType, if that field is using an ObjectType in its type,
            // resolve the type mappings for that ObjectType too
            for (field_name, field_definition) in &object_type_representation.object_type.fields {
                let field_mapping = field_mappings.get(field_name).ok_or_else(|| {
                    TypeMappingCollectionError::MissingFieldMapping {
                        type_name: mapping_to_collect.type_name.clone(),
                        field_name: field_name.clone(),
                        data_connector: data_connector_name.clone(),
                        ndc_type_name: DataConnectorObjectType::from(
                            mapping_to_collect.ndc_object_type_name.as_str(),
                        ),
                    }
                })?;

                if let Some(object_type_name) =
                    unwrap_custom_type_name(&field_definition.field_type)
                {
                    if object_type_exists(object_type_name, object_types).is_ok() {
                        let underlying_ndc_field_named_type =
                            get_underlying_named_type(&field_mapping.column_type);

                        let field_type_mapping_to_collect = TypeMappingToCollect {
                            type_name: object_type_name,
                            ndc_object_type_name: underlying_ndc_field_named_type,
                        };

                        collect_type_mapping_for_source(
                            &field_type_mapping_to_collect,
                            data_connector_name,
                            object_types,
                            scalar_types,
                            collected_mappings,
                            special_case,
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

fn handle_special_case_type_mapping<'a>(
    mapping_to_collect: &TypeMappingToCollect,
    data_connector_name: &Qualified<DataConnectorName>,
    object_type_representation: &'a type_permissions::ObjectTypeWithPermissions,
    special_case: Option<&SpecialCaseTypeMapping>,
) -> Result<&'a object_types::TypeMapping, TypeMappingCollectionError> {
    if let Some(SpecialCaseTypeMapping {
        response_config,
        ndc_object_type,
    }) = special_case
    {
        if ndc_object_type
            .fields
            .contains_key(response_config.headers_field.as_str())
            && ndc_object_type
                .fields
                .contains_key(response_config.result_field.as_str())
        {
            let ndc_object_type = &ndc_object_type
                .fields
                .get(response_config.result_field.as_str())
                .unwrap()
                .r#type;
            let ndc_object_type_name = unwrap_ndc_object_type_name(ndc_object_type);
            object_type_representation
                .type_mappings
                .get(data_connector_name, ndc_object_type_name.as_str())
                .ok_or_else(|| TypeMappingCollectionError::MappingNotDefined {
                    type_name: mapping_to_collect.type_name.clone(),
                    data_connector: data_connector_name.clone(),
                    ndc_type_name: DataConnectorObjectType::from(
                        mapping_to_collect.ndc_object_type_name.as_str(),
                    ),
                })
        } else {
            Err(TypeMappingCollectionError::MappingNotDefined {
                type_name: mapping_to_collect.type_name.clone(),
                data_connector: data_connector_name.clone(),
                ndc_type_name: DataConnectorObjectType::from(
                    mapping_to_collect.ndc_object_type_name.as_str(),
                ),
            })
        }
    } else {
        Err(TypeMappingCollectionError::MappingNotDefined {
            type_name: mapping_to_collect.type_name.clone(),
            data_connector: data_connector_name.clone(),
            ndc_type_name: DataConnectorObjectType::from(
                mapping_to_collect.ndc_object_type_name.as_str(),
            ),
        })
    }
}

fn unwrap_ndc_object_type_name(ndc_type: &ndc_models::Type) -> &ndc_models::TypeName {
    match ndc_type {
        ndc_models::Type::Named { name } => name,
        ndc_models::Type::Nullable { underlying_type } => {
            unwrap_ndc_object_type_name(underlying_type)
        }
        ndc_models::Type::Array { .. } | ndc_models::Type::Predicate { .. } => {
            panic!("unexpected ndc type; only object is supported")
        }
    }
}

// moved from `graphql_ir` to avoid cycles,
// not sure if this is the right home for it.
#[derive(Debug, thiserror::Error)]
pub enum RelationshipFieldMappingError {
    #[error("Type mapping not found for the type name {type_name:} while executing the relationship {relationship_name:}")]
    TypeMappingNotFoundForRelationship {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },

    #[error("Field mapping not found for the field {field_name:} of type {type_name:} while executing the relationship {relationship_name:}")]
    FieldMappingNotFoundForRelationship {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        field_name: FieldName,
    },
}

pub fn get_field_mapping_of_field_name(
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, object_types::TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    relationship_name: &RelationshipName,
    field_name: &FieldName,
) -> Result<object_types::FieldMapping, RelationshipFieldMappingError> {
    let type_mapping = type_mappings.get(type_name).ok_or_else(|| {
        RelationshipFieldMappingError::TypeMappingNotFoundForRelationship {
            type_name: type_name.clone(),
            relationship_name: relationship_name.clone(),
        }
    })?;
    match type_mapping {
        object_types::TypeMapping::Object { field_mappings, .. } => Ok(field_mappings
            .get(field_name)
            .ok_or_else(
                || RelationshipFieldMappingError::FieldMappingNotFoundForRelationship {
                    type_name: type_name.clone(),
                    relationship_name: relationship_name.clone(),
                    field_name: field_name.clone(),
                },
            )?
            .clone()),
    }
}
