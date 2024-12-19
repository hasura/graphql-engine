use super::types::ModelSource;

use super::error::ModelsError;
use crate::helpers::types::NdcColumnForComparison;
use crate::stages::object_types;
use crate::types::subgraph::Qualified;
use open_dds::{
    models::ModelName,
    types::{CustomTypeName, FieldName},
};

pub fn get_ndc_column_for_comparison<F: Fn() -> String>(
    model_name: &Qualified<ModelName>,
    model_data_type: &Qualified<CustomTypeName>,
    model_source: &ModelSource,
    field: &FieldName,
    comparison_location: F,
) -> Result<NdcColumnForComparison, ModelsError> {
    // Get field mappings of model data type
    let object_types::TypeMapping::Object { field_mappings, .. } = model_source
        .type_mappings
        .get(model_data_type)
        .ok_or(ModelsError::TypeMappingRequired {
            model_name: model_name.clone(),
            type_name: model_data_type.clone(),
            data_connector: model_source.data_connector.name.clone(),
        })?;

    // Determine field_mapping for the given field
    let field_mapping =
        field_mappings
            .get(field)
            .ok_or_else(|| ModelsError::NoFieldMappingForComparedField {
                comparison_location: comparison_location(),
                field_name: field.clone(),
                model_name: model_name.clone(),
            })?;

    let equal_operator = field_mapping
        .comparison_operators
        .as_ref()
        .and_then(|ops| ops.eq_operator.as_ref())
        .ok_or_else(|| ModelsError::NoEqualOperatorForComparedField {
            comparison_location: comparison_location(),
            field_name: field.clone(),
            model_name: model_name.clone(),
        })?;

    Ok(NdcColumnForComparison {
        column: field_mapping.column.clone(),
        equal_operator: equal_operator.clone(),
    })
}
