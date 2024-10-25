use crate::types::{FieldType, ModelWarning};
use hasura_authn_core::Role;
use indexmap::IndexMap;
use metadata_resolve::{
    ModelWithArgumentPresets, ObjectTypeWithRelationships, Qualified, QualifiedBaseType,
    QualifiedTypeName, QualifiedTypeReference, ScalarTypeRepresentation, ValueRepresentation,
};
use open_dds::{
    data_connector::DataConnectorName,
    types::{CustomTypeName, FieldName, InbuiltType},
};
use std::collections::BTreeMap;

// look at permissions and work out which fields we're allowed to see
// this is quite limited and leans to be overcautious
pub fn get_model_fields(
    model: &ModelWithArgumentPresets,
    role: &Role,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, ScalarTypeRepresentation>,
) -> Result<IndexMap<FieldName, FieldType>, ModelWarning> {
    // if we have no select permission for the model, ignore it
    if !model.select_permissions.contains_key(role) {
        return Err(ModelWarning::NoSelectPermission);
    }
    let underlying_object_type = object_types.get(&model.model.data_type).ok_or_else(|| {
        ModelWarning::NoObjectTypeFound {
            object_type_name: model.model.data_type.clone(),
        }
    })?;

    let model_source = model
        .model
        .source
        .as_ref()
        .ok_or_else(|| ModelWarning::NoModelSource)?;

    let output_permissions_for_role = underlying_object_type
        .type_output_permissions
        .get(role)
        .unwrap();

    // otherwise return all fields
    let mut type_fields = IndexMap::new();
    for (field_name, field_info) in
        model
            .model
            .type_fields
            .iter()
            .filter(|(field_name, _field_info)| {
                output_permissions_for_role
                    .allowed_fields
                    .contains(*field_name)
            })
    {
        let field_type = field_type_from_type_representation(
            &field_info.field_type,
            scalar_types,
            object_types,
            &model_source.data_connector.name,
        )?;

        type_fields.insert(field_name.clone(), field_type);
    }
    Ok(type_fields)
}

// turn an OpenDD type into a type representation
fn field_type_from_type_representation(
    qualified_type_reference: &QualifiedTypeReference,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, ScalarTypeRepresentation>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    data_connector_name: &Qualified<DataConnectorName>,
) -> Result<FieldType, ModelWarning> {
    // NOTE: currently we assume everything is nullable because a user might
    // not include a field in sparse fields
    match &qualified_type_reference.underlying_type {
        QualifiedBaseType::Named(name) => match name {
            QualifiedTypeName::Inbuilt(inbuilt) => {
                Ok(FieldType::TypeRepresentation(match inbuilt {
                    InbuiltType::String | InbuiltType::ID => ndc_models::TypeRepresentation::String,
                    InbuiltType::Int => ndc_models::TypeRepresentation::Int64,
                    InbuiltType::Float => ndc_models::TypeRepresentation::Float64,
                    InbuiltType::Boolean => ndc_models::TypeRepresentation::Boolean,
                }))
            }
            QualifiedTypeName::Custom(custom_type_name) => {
                match scalar_types.get(custom_type_name) {
                    Some(scalar_type) => {
                        match scalar_type.representations.get(data_connector_name) {
                            Some(value_representation) => {
                                Ok(FieldType::TypeRepresentation(match value_representation {
                                    ValueRepresentation::FromDataConnectorSchema(
                                        type_representation,
                                    ) => type_representation.clone(),
                                    ValueRepresentation::AssumeJson => {
                                        ndc_models::TypeRepresentation::JSON
                                    }
                                }))
                            }
                            None => Err(ModelWarning::NoTypeRepresentationFoundForDataConnector {
                                data_connector_name: data_connector_name.clone(),
                                object_type_name: custom_type_name.clone(),
                            }),
                        }
                    }
                    None => match object_types.get(custom_type_name) {
                        Some(object_type) => {
                            let mut items = IndexMap::new();
                            for (field_name, field) in &object_type.object_type.fields {
                                items.insert(
                                    field_name.clone(),
                                    field_type_from_type_representation(
                                        &field.field_type,
                                        scalar_types,
                                        object_types,
                                        data_connector_name,
                                    )?,
                                );
                            }
                            Ok(FieldType::Object(items))
                        }
                        None => Err(ModelWarning::NoTypeRepresentationFound {
                            object_type_name: custom_type_name.clone(),
                        }),
                    },
                }
            }
        },
        QualifiedBaseType::List(ty) => Ok(FieldType::List(Box::new(
            field_type_from_type_representation(
                ty,
                scalar_types,
                object_types,
                data_connector_name,
            )?,
        ))),
    }
}
