use super::types::ModelSource;
use open_dds::data_connector::{DataConnectorName, DataConnectorOperatorName};

use crate::types::error::Error;

use crate::helpers::types::NdcColumnForComparison;
use crate::stages::{data_connector_scalar_types, object_types};
use crate::types::subgraph::Qualified;

use open_dds::{
    models::ModelName,
    types::{CustomTypeName, FieldName},
};

use std::collections::BTreeMap;

pub fn get_ndc_column_for_comparison<F: Fn() -> String>(
    model_name: &Qualified<ModelName>,
    model_data_type: &Qualified<CustomTypeName>,
    model_source: &ModelSource,
    field: &FieldName,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::ScalarTypeWithRepresentationInfoMap,
    >,
    comparison_location: F,
) -> Result<NdcColumnForComparison, Error> {
    // Get field mappings of model data type
    let object_types::TypeMapping::Object { field_mappings, .. } = model_source
        .type_mappings
        .get(model_data_type)
        .ok_or(Error::TypeMappingRequired {
            model_name: model_name.clone(),
            type_name: model_data_type.clone(),
            data_connector: model_source.data_connector.name.clone(),
        })?;

    // Determine field_mapping for the given field
    let field_mapping =
        field_mappings
            .get(field)
            .ok_or_else(|| Error::NoFieldMappingForComparedField {
                comparison_location: comparison_location(),
                field_name: field.clone(),
                model_name: model_name.clone(),
            })?;

    // Determine ndc type of the field
    let field_ndc_type = &field_mapping.column_type;

    // Get available scalars defined in the data connector
    let scalars = &data_connector_scalars
        .get(&model_source.data_connector.name)
        .ok_or(Error::UnknownModelDataConnector {
            model_name: model_name.clone(),
            data_connector: model_source.data_connector.name.clone(),
        })?;

    // Determine whether the ndc type is a simple scalar and get scalar type info
    let scalar_type_info =
        data_connector_scalar_types::get_simple_scalar(field_ndc_type.clone(), scalars)
            .ok_or_else(|| Error::UncomparableNonScalarFieldType {
                comparison_location: comparison_location(),
                field_name: field.clone(),
                model_name: model_name.clone(),
            })?;

    let equal_operator = match scalar_type_info
        .comparison_operators
        .equal_operators
        .as_slice()
    {
        [] => {
            return Err(Error::NoEqualOperatorForComparedField {
                comparison_location: comparison_location(),
                field_name: field.clone(),
                model_name: model_name.clone(),
            });
        }
        [equal_operator] => equal_operator,
        _ => {
            return Err(Error::MultipleEqualOperatorsForComparedField {
                comparison_location: comparison_location(),
                field_name: field.clone(),
                model_name: model_name.clone(),
            });
        }
    };

    Ok(NdcColumnForComparison {
        column: field_mapping.column.clone(),
        equal_operator: DataConnectorOperatorName::from(equal_operator.as_str()),
    })
}
