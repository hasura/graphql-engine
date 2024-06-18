use crate::types::error::Error;

use crate::stages::object_types;

use indexmap::IndexMap;

use open_dds::{
    models::{EnableAllOrSpecific, ModelV1, OrderableField},
    types::FieldName,
};

pub(crate) fn resolve_orderable_fields(
    model: &ModelV1,
    type_fields: &IndexMap<FieldName, object_types::FieldDefinition>,
) -> Result<Vec<OrderableField>, Error> {
    for field in &model.orderable_fields {
        // Check for unknown orderable field
        if !type_fields.contains_key(&field.field_name) {
            return Err(Error::UnknownFieldInOrderableFields {
                model_name: model.name.clone(),
                field_name: field.field_name.clone(),
            });
        }
        match &field.order_by_directions {
            EnableAllOrSpecific::EnableAll(true) => {}
            _ => {
                return Err(Error::UnsupportedFeature {
                    message: "Field level order by configuration is not fully supported yet. Please use \"enableAll\":true.".to_string(),
                })
            }
        }
    }

    // Model orderable fields should have all type fields
    if model.orderable_fields.len() != type_fields.len() {
        return Err(Error::UnsupportedFeature {
            message: "Field level order by configuration is not fully supported yet. Please add all fields in orderable_fields.".to_string(),
        });
    }
    Ok(model.orderable_fields.clone())
}
