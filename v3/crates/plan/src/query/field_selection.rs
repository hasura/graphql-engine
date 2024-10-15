use crate::types::PlanError;
use indexmap::IndexMap;
use std::collections::BTreeMap;

use execute::plan::{
    field::{Field, NestedArray, NestedField},
    ResolvedFilterExpression,
};
use metadata_resolve::{Metadata, Qualified, QualifiedTypeReference, TypeMapping};
use open_dds::types::CustomTypeName;
use plan_types::NdcFieldAlias;

pub fn ndc_nested_field_selection_for(
    metadata: &Metadata,
    column_type: &QualifiedTypeReference,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
) -> Result<Option<NestedField<ResolvedFilterExpression>>, PlanError> {
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
                        let nested_fields: Option<NestedField<ResolvedFilterExpression>> =
                            ndc_nested_field_selection_for(
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

                    return Ok(Some(NestedField::Object(
                        execute::plan::field::NestedObject { fields },
                    )));
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
