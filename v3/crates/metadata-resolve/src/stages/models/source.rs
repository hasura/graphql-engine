use super::types::{ConnectorArgumentName, Model, ModelSource};
use open_dds::data_connector::{DataConnectorName, DataConnectorObjectType};

use crate::helpers::argument::get_argument_mappings;
use crate::helpers::ndc_validation;
use crate::types::error::Error;

use crate::helpers::type_mappings;
use crate::helpers::types::NdcColumnForComparison;
use crate::stages::{
    data_connector_scalar_types, data_connectors, object_boolean_expressions, object_types,
    scalar_types, type_permissions,
};
use crate::types::subgraph::Qualified;

use open_dds::{
    models::{self, ModelName},
    types::{CustomTypeName, FieldName},
};

use std::collections::BTreeMap;
use std::iter;

pub(crate) fn resolve_model_source(
    model_source: &models::ModelSource,
    model: &mut Model,
    subgraph: &str,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::ScalarTypeWithRepresentationInfoMap,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
) -> Result<ModelSource, Error> {
    if model.source.is_some() {
        return Err(Error::DuplicateModelSourceDefinition {
            model_name: model.name.clone(),
        });
    }
    let qualified_data_connector_name = Qualified::new(
        subgraph.to_string(),
        model_source.data_connector_name.clone(),
    );

    let data_connector_context = data_connectors
        .0
        .get(&qualified_data_connector_name)
        .ok_or_else(|| Error::UnknownModelDataConnector {
            model_name: model.name.clone(),
            data_connector: qualified_data_connector_name.clone(),
        })?;

    let source_collection = data_connector_context
        .inner
        .schema
        .collections
        .get(&model_source.collection)
        .ok_or_else(|| Error::UnknownModelCollection {
            model_name: model.name.clone(),
            data_connector: qualified_data_connector_name.clone(),
            collection: model_source.collection.clone(),
        })?;
    let source_collection_type = DataConnectorObjectType(source_collection.collection_type.clone());

    let source_arguments = source_collection
        .clone()
        .arguments
        .into_iter()
        .map(|(k, v)| (ConnectorArgumentName(k), v.argument_type))
        .collect();

    // Get the mappings of arguments and any type mappings that need resolving from the arguments
    let (argument_mappings, argument_type_mappings_to_collect) = get_argument_mappings(
        &model.arguments,
        &model_source.argument_mapping,
        &source_arguments,
        object_types,
        scalar_types,
        object_boolean_expression_types,
    )
    .map_err(|err| Error::ModelCollectionArgumentMappingError {
        data_connector_name: qualified_data_connector_name.clone(),
        model_name: model.name.clone(),
        collection_name: model_source.collection.clone(),
        error: err,
    })?;

    // Collect type mappings.
    let mut type_mappings = BTreeMap::new();
    let source_collection_type_mapping_to_collect = type_mappings::TypeMappingToCollect {
        type_name: &model.data_type,
        ndc_object_type_name: &source_collection_type,
    };
    for type_mapping_to_collect in iter::once(&source_collection_type_mapping_to_collect)
        .chain(argument_type_mappings_to_collect.iter())
    {
        type_mappings::collect_type_mapping_for_source(
            type_mapping_to_collect,
            &qualified_data_connector_name,
            object_types,
            scalar_types,
            &mut type_mappings,
        )
        .map_err(|error| Error::ModelTypeMappingCollectionError {
            model_name: model.name.clone(),
            error,
        })?;
    }

    let resolved_model_source = ModelSource {
        data_connector: data_connectors::DataConnectorLink::new(
            qualified_data_connector_name,
            &data_connector_context.inner,
        )?,
        collection: model_source.collection.clone(),
        collection_type: source_collection_type,
        type_mappings,
        argument_mappings,
        source_arguments,
    };

    let model_object_type =
        get_model_object_type_representation(object_types, &model.data_type, &model.name)?;

    if let Some(global_id_source) = &mut model.global_id_source {
        for global_id_field in &model_object_type.object_type.global_id_fields {
            global_id_source.ndc_mapping.insert(
                global_id_field.clone(),
                get_ndc_column_for_comparison(
                    &model.name,
                    &model.data_type,
                    &resolved_model_source,
                    global_id_field,
                    data_connector_scalars,
                    || format!("the global ID fields of type {}", model.data_type),
                )?,
            );
        }
    }

    if let Some(apollo_federation_key_source) = &mut model.apollo_federation_key_source {
        if let Some(apollo_federation_config) =
            &model_object_type.object_type.apollo_federation_config
        {
            for key in &apollo_federation_config.keys {
                for field in &key.fields {
                    apollo_federation_key_source.ndc_mapping.insert(
                        field.clone(),
                        get_ndc_column_for_comparison(
                            &model.name,
                            &model.data_type,
                            &resolved_model_source,
                            field,
                            data_connector_scalars,
                            || {
                                format!(
                                    "the apollo federation key fields of type {}",
                                    model.data_type
                                )
                            },
                        )?,
                    );
                }
            }
        }
    }

    ndc_validation::validate_ndc(&model.name, model, &data_connector_context.inner.schema)?;
    Ok(resolved_model_source)
}

/// Gets the `type_permissions::ObjectTypeWithPermissions` of the type identified with the
/// `data_type`, it will throw an error if the type is not found to be an object
/// or if the model has an unknown data type.
pub(crate) fn get_model_object_type_representation<'s>(
    object_types: &'s BTreeMap<
        Qualified<CustomTypeName>,
        type_permissions::ObjectTypeWithPermissions,
    >,
    data_type: &Qualified<CustomTypeName>,
    model_name: &Qualified<ModelName>,
) -> Result<&'s type_permissions::ObjectTypeWithPermissions, crate::Error> {
    match object_types.get(data_type) {
        Some(object_type_representation) => Ok(object_type_representation),
        None => Err(Error::UnknownModelDataType {
            model_name: model_name.clone(),
            data_type: data_type.clone(),
        }),
    }
}

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
        equal_operator: equal_operator.clone(),
    })
}
