use crate::types::PlanError;
use hasura_authn_core::Session;
use indexmap::IndexMap;
use std::collections::BTreeMap;
use std::sync::Arc;

use metadata_resolve::{Metadata, Qualified, QualifiedTypeReference, TypeMapping};
use open_dds::{
    models::ModelName,
    permissions::TypeOutputPermission,
    query::ObjectFieldSelection,
    types::{CustomTypeName, FieldName},
};
use plan_types::{Field, NdcFieldAlias, NestedArray, NestedField, NestedObject};

pub fn from_field_selection(
    field_selection: &ObjectFieldSelection,
    session: &Arc<Session>,
    metadata: &Metadata,
    qualified_model_name: &Qualified<ModelName>,
    model: &metadata_resolve::ModelWithArgumentPresets,
    model_source: &Arc<metadata_resolve::ModelSource>,
    model_object_type: &metadata_resolve::ObjectTypeWithRelationships,
    field_mappings: &BTreeMap<FieldName, metadata_resolve::FieldMapping>,
    type_permissions: &TypeOutputPermission,
) -> Result<Field, PlanError> {
    if !type_permissions
        .allowed_fields
        .contains(&field_selection.target.field_name)
    {
        return Err(PlanError::Permission(format!(
            "role {} does not have permission to select the field {} from type {} of model {}",
            session.role,
            field_selection.target.field_name,
            model.model.data_type,
            qualified_model_name
        )));
    }

    let field_mapping = field_mappings
        .get(&field_selection.target.field_name)
        // .map(|field_mapping| field_mapping.column.clone())
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "couldn't fetch field mapping of field {} in type {} for model {}",
                field_selection.target.field_name, model.model.data_type, qualified_model_name
            ))
        })?;

    let field_type = &model_object_type
        .object_type
        .fields
        .get(&field_selection.target.field_name)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "could not look up type of field {}",
                field_selection.target.field_name
            ))
        })?
        .field_type;

    let fields = ndc_nested_field_selection_for(metadata, field_type, &model_source.type_mappings)?;

    let ndc_field = Field::Column {
        column: field_mapping.column.clone(),
        fields,
        arguments: BTreeMap::new(),
    };
    Ok(ndc_field)
}

pub fn ndc_nested_field_selection_for(
    metadata: &Metadata,
    column_type: &QualifiedTypeReference,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
) -> Result<Option<NestedField>, PlanError> {
    match &column_type.underlying_type {
        metadata_resolve::QualifiedBaseType::Named(name) => match name {
            metadata_resolve::QualifiedTypeName::Custom(name) => {
                if let Some(_scalar_type) = metadata.scalar_types.get(name) {
                    return Ok(None);
                }
                if let Some(object_type) = metadata.object_types.get(name) {
                    let TypeMapping::Object {
                        ndc_object_type_name: _,
                        field_mappings,
                    } = type_mappings.get(name).ok_or_else(|| {
                        PlanError::Internal(format!("can't find mapping object for type: {name}"))
                    })?;

                    let mut fields = IndexMap::new();

                    for (field_name, field_mapping) in field_mappings {
                        let field_def = object_type.object_type.fields.get(field_name).ok_or_else(|| PlanError::Internal(format!(
                            "can't find object field definition for field {field_name} in type: {name}"
                        )))?;
                        let nested_fields: Option<NestedField> = ndc_nested_field_selection_for(
                            metadata,
                            &field_def.field_type,
                            type_mappings,
                        )?;
                        fields.insert(
                            NdcFieldAlias::from(field_name.as_str()),
                            Field::Column {
                                column: field_mapping.column.clone(),
                                fields: nested_fields,
                                arguments: BTreeMap::new(),
                            },
                        );
                    }

                    return Ok(Some(NestedField::Object(NestedObject { fields })));
                }

                Err(PlanError::Internal(format!(
                    "named type was neither a scalar nor an object: {name}",
                )))
            }
            metadata_resolve::QualifiedTypeName::Inbuilt(_) => Ok(None),
        },
        metadata_resolve::QualifiedBaseType::List(list_type) => {
            let fields =
                ndc_nested_field_selection_for(metadata, list_type.as_ref(), type_mappings)?;

            Ok(fields.map(|fields| {
                NestedField::Array(NestedArray {
                    fields: Box::new(fields),
                })
            }))
        }
    }
}
