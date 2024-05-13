use crate::stages::{object_types, scalar_types, type_permissions};

use crate::helpers::ndc_validation::{get_underlying_named_type, NDCValidationError};
use crate::helpers::types::{object_type_exists, unwrap_custom_type_name};
use crate::types::subgraph::Qualified;

use open_dds::data_connector::{DataConnectorName, DataConnectorObjectType};
use open_dds::types::{CustomTypeName, FieldName};

use ref_cast::RefCast;
use std::collections::BTreeMap;

#[derive(Debug)]
pub struct TypeMappingToCollect<'a> {
    pub type_name: &'a Qualified<CustomTypeName>,
    pub ndc_object_type_name: &'a DataConnectorObjectType,
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

pub(crate) fn collect_type_mapping_for_source(
    mapping_to_collect: &TypeMappingToCollect,
    data_connector_name: &Qualified<DataConnectorName>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    collected_mappings: &mut BTreeMap<Qualified<CustomTypeName>, object_types::TypeMapping>,
) -> Result<(), TypeMappingCollectionError> {
    match object_types.get(mapping_to_collect.type_name) {
        Some(object_type_representation) => {
            let type_mapping = object_type_representation
                .type_mappings
                .get(data_connector_name, mapping_to_collect.ndc_object_type_name)
                .ok_or_else(|| TypeMappingCollectionError::MappingNotDefined {
                    type_name: mapping_to_collect.type_name.clone(),
                    data_connector: data_connector_name.clone(),
                    ndc_type_name: mapping_to_collect.ndc_object_type_name.clone(),
                })?;

            // If there is an existing mapping, make sure it maps to the same NDC object type.
            if let Some(inserted_mapping) = collected_mappings
                .insert(mapping_to_collect.type_name.clone(), type_mapping.clone())
            {
                let object_types::TypeMapping::Object {
                    ndc_object_type_name,
                    ..
                } = inserted_mapping;
                if ndc_object_type_name != *mapping_to_collect.ndc_object_type_name {
                    return Err(
                        TypeMappingCollectionError::MappingToMultipleDataConnectorObjectType {
                            type_name: mapping_to_collect.type_name.clone(),
                            ndc_type_1: ndc_object_type_name,
                            ndc_type_2: DataConnectorObjectType(
                                mapping_to_collect.ndc_object_type_name.to_string(),
                            ),
                        },
                    );
                } else {
                    return Ok(());
                }
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
                        ndc_type_name: mapping_to_collect.ndc_object_type_name.clone(),
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
                            ndc_object_type_name: DataConnectorObjectType::ref_cast(
                                underlying_ndc_field_named_type,
                            ),
                        };

                        collect_type_mapping_for_source(
                            &field_type_mapping_to_collect,
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
