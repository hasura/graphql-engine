use super::types::{Model, ModelSource};
use open_dds::data_connector::{DataConnectorName, DataConnectorObjectType};
use open_dds::types::DataConnectorArgumentName;

use crate::helpers::argument::get_argument_mappings;
use crate::helpers::ndc_validation;
use crate::types::error::Error;

use super::helpers;
use crate::helpers::type_mappings;
use crate::stages::{
    boolean_expressions, data_connector_scalar_types, data_connectors, object_boolean_expressions,
    scalar_types, type_permissions,
};
use crate::types::subgraph::Qualified;

use open_dds::{
    models::{self, ModelName},
    types::CustomTypeName,
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
    object_types: &type_permissions::ObjectTypesWithPermissions,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
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
        .schema
        .collections
        .get(model_source.collection.as_str())
        .ok_or_else(|| Error::UnknownModelCollection {
            model_name: model.name.clone(),
            data_connector: qualified_data_connector_name.clone(),
            collection: model_source.collection.clone(),
        })?;
    let source_collection_type =
        DataConnectorObjectType::from(source_collection.collection_type.as_str());

    let source_arguments = source_collection
        .clone()
        .arguments
        .into_iter()
        .map(|(k, v)| (DataConnectorArgumentName::from(k.as_str()), v.argument_type))
        .collect();

    // Get the mappings of arguments and any type mappings that need resolving from the arguments
    let (argument_mappings, argument_type_mappings_to_collect) = get_argument_mappings(
        &model.arguments,
        &model_source.argument_mapping,
        &source_arguments,
        object_types,
        scalar_types,
        object_boolean_expression_types,
        boolean_expression_types,
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
        ndc_object_type_name: source_collection.collection_type.as_ref(),
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
            &None,
        )
        .map_err(|error| Error::ModelTypeMappingCollectionError {
            model_name: model.name.clone(),
            error,
        })?;
    }

    let resolved_model_source = ModelSource {
        data_connector: data_connectors::DataConnectorLink::new(
            qualified_data_connector_name,
            data_connector_context,
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
                helpers::get_ndc_column_for_comparison(
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
                        helpers::get_ndc_column_for_comparison(
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

    ndc_validation::validate_ndc(&model.name, model, &data_connector_context.schema)?;
    Ok(resolved_model_source)
}

/// Gets the `type_permissions::ObjectTypeWithPermissions` of the type identified with the
/// `data_type`, it will throw an error if the type is not found to be an object
/// or if the model has an unknown data type.
pub(crate) fn get_model_object_type_representation<'s>(
    object_types: &'s type_permissions::ObjectTypesWithPermissions,
    data_type: &Qualified<CustomTypeName>,
    model_name: &Qualified<ModelName>,
) -> Result<&'s type_permissions::ObjectTypeWithPermissions, crate::Error> {
    object_types
        .get(data_type)
        .map_err(|_| Error::UnknownModelDataType {
            model_name: model_name.clone(),
            data_type: data_type.clone(),
        })
}
