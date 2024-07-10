use super::types::{FilterPermission, ModelPredicate, SelectPermission};
use crate::helpers::typecheck;
use crate::stages::{
    boolean_expressions, data_connector_scalar_types, data_connectors, models, models_graphql,
    object_boolean_expressions, object_types, relationships, scalar_types,
};
use indexmap::IndexMap;
use open_dds::{data_connector::DataConnectorName, models::ModelName, types::CustomTypeName};
use std::collections::BTreeMap;

use crate::helpers::argument::{
    resolve_model_predicate_with_type, resolve_value_expression_for_argument,
};
use crate::types::error::{Error, TypePredicateError};

use crate::types::subgraph::Qualified;

use ndc_models;
use open_dds::permissions::NullableModelPredicate;
use open_dds::{
    arguments::ArgumentName,
    permissions::{ModelPermissionsV1, Role},
    types::FieldName,
};

fn resolve_model_predicate_with_model(
    model_predicate: &open_dds::permissions::ModelPredicate,
    model: &models::Model,
    subgraph: &str,
    boolean_expression_graphql: Option<&boolean_expressions::BooleanExpressionGraphqlConfig>,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::ScalarTypeWithRepresentationInfoMap,
    >,
    fields: &IndexMap<FieldName, object_types::FieldDefinition>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
) -> Result<ModelPredicate, Error> {
    let model_source = model
        .source
        .clone()
        .ok_or(Error::ModelSourceRequiredForPredicate {
            model_name: model.name.clone(),
        })?;

    let data_connector_name = &model_source.data_connector.name;

    // get available scalars defined for this data connector
    let scalars =
        data_connector_scalars
            .get(data_connector_name)
            .ok_or(Error::TypePredicateError {
                type_predicate_error: TypePredicateError::UnknownTypeDataConnector {
                    type_name: model.data_type.clone(),
                    data_connector: data_connector_name.clone(),
                },
            })?;

    // get the type that the expression is based on
    let object_type_representation =
        object_types
            .get(&model.data_type)
            .ok_or(Error::UnknownType {
                data_type: model.data_type.clone(),
            })?;

    // Get field mappings of model data type
    let object_types::TypeMapping::Object { field_mappings, .. } = model_source
        .type_mappings
        .get(&model.data_type)
        .ok_or(Error::TypeMappingRequired {
            model_name: model.name.clone(),
            type_name: model.data_type.clone(),
            data_connector: model_source.data_connector.name.clone(),
        })?;

    let data_connector_core_info = data_connectors.0.get(data_connector_name).ok_or_else(|| {
        Error::UnknownModelDataConnector {
            model_name: model.name.clone(),
            data_connector: data_connector_name.clone(),
        }
    })?;

    let data_connector_link = data_connectors::DataConnectorLink::new(
        data_connector_name.clone(),
        data_connector_core_info,
    )?;

    resolve_model_predicate_with_type(
        model_predicate,
        &model.data_type,
        object_type_representation,
        boolean_expression_graphql,
        field_mappings,
        &data_connector_link,
        subgraph,
        scalars,
        object_types,
        scalar_types,
        boolean_expression_types,
        models,
        fields,
    )
}

// get the ndc_models::Type for an argument if it is available
fn get_model_source_argument<'a>(
    argument_name: &'a ArgumentName,
    model: &'a models::Model,
) -> Option<&'a ndc_models::Type> {
    model
        .source
        .as_ref()
        .and_then(|source| {
            source
                .argument_mappings
                .get(argument_name)
                .map(|connector_argument_name| source.source_arguments.get(connector_argument_name))
        })
        .flatten()
}

pub fn resolve_model_select_permissions(
    model: &models::Model,
    subgraph: &str,
    model_permissions: &ModelPermissionsV1,
    boolean_expression_graphql: Option<&boolean_expressions::BooleanExpressionGraphqlConfig>,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::ScalarTypeWithRepresentationInfoMap,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
) -> Result<BTreeMap<Role, SelectPermission>, Error> {
    let mut validated_permissions = BTreeMap::new();
    for model_permission in &model_permissions.permissions {
        if let Some(select) = &model_permission.select {
            let resolved_predicate = match &select.filter {
                NullableModelPredicate::NotNull(model_predicate) => {
                    resolve_model_predicate_with_model(
                        model_predicate,
                        model,
                        subgraph,
                        boolean_expression_graphql,
                        data_connectors,
                        data_connector_scalars,
                        &model.type_fields,
                        object_types,
                        scalar_types,
                        boolean_expression_types,
                        models,
                    )
                    .map(FilterPermission::Filter)?
                }
                NullableModelPredicate::Null(()) => FilterPermission::AllowAll,
            };

            let mut argument_presets = BTreeMap::new();

            for argument_preset in &select.argument_presets {
                if argument_presets.contains_key(&argument_preset.argument) {
                    return Err(Error::DuplicateModelArgumentPreset {
                        model_name: model.name.clone(),
                        argument_name: argument_preset.argument.clone(),
                    });
                }

                let source_argument_type =
                    get_model_source_argument(&argument_preset.argument, model);

                let data_connector_name = model
                    .source
                    .as_ref()
                    .map(|source| &source.data_connector.name)
                    .ok_or(Error::ModelSourceRequiredForPredicate {
                        model_name: model.name.clone(),
                    })?;

                let data_connector_core_info = data_connectors.0.get(data_connector_name).unwrap();
                let data_connector_link = data_connectors::DataConnectorLink::new(
                    data_connector_name.clone(),
                    data_connector_core_info,
                )?;

                match model.arguments.get(&argument_preset.argument) {
                    Some(argument) => {
                        let value_expression = resolve_value_expression_for_argument(
                            &argument_preset.argument,
                            &argument_preset.value,
                            &argument.argument_type,
                            source_argument_type,
                            &data_connector_link,
                            subgraph,
                            object_types,
                            scalar_types,
                            object_boolean_expression_types,
                            boolean_expression_types,
                            models,
                            data_connector_scalars,
                        )?;

                        // additionally typecheck literals
                        // we do this outside the argument resolve so that we can emit a model-specific error
                        // on typechecking failure
                        typecheck::typecheck_value_expression_or_predicate(
                            &argument.argument_type,
                            &argument_preset.value,
                        )
                        .map_err(|type_error| {
                            Error::ModelArgumentPresetTypeError {
                                model_name: model.name.clone(),
                                argument_name: argument_preset.argument.clone(),
                                type_error,
                            }
                        })?;

                        argument_presets.insert(
                            argument_preset.argument.clone(),
                            (argument.argument_type.clone(), value_expression),
                        );
                    }
                    None => {
                        return Err(Error::ModelArgumentPresetMismatch {
                            model_name: model.name.clone(),
                            argument_name: argument_preset.argument.clone(),
                        });
                    }
                }
            }

            let resolved_permission = SelectPermission {
                filter: resolved_predicate.clone(),
                argument_presets,
            };
            validated_permissions.insert(model_permission.role.clone(), resolved_permission);
        }
    }
    Ok(validated_permissions)
}
