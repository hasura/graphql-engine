use open_dds::data_connector::DataConnectorObjectType;
pub use types::{
    ConnectorArgumentName, LimitFieldGraphqlConfig, Model, ModelGraphQlApi,
    ModelGraphqlApiArgumentsConfig, ModelOrderByExpression, ModelSource, ModelsOutput,
    NDCFieldSourceMapping, OffsetFieldGraphqlConfig, OrderByExpressionInfo,
    SelectManyGraphQlDefinition, SelectUniqueGraphQlDefinition, UniqueIdentifierField,
};
mod types;

use crate::helpers::argument::get_argument_mappings;
use crate::helpers::ndc_validation;
use crate::types::error::{BooleanExpressionError, Error, GraphqlConfigError};

use crate::helpers::type_mappings;
use crate::helpers::types::NdcColumnForComparison;
use crate::helpers::types::{mk_name, store_new_graphql_type};
use crate::stages::{
    data_connector_scalar_types, data_connectors, graphql_config, object_boolean_expressions,
    object_types, scalar_types, type_permissions,
};
use crate::types::subgraph::{mk_qualified_type_reference, ArgumentInfo, Qualified};

use indexmap::IndexMap;
use lang_graphql::ast::common::{self as ast};
use ref_cast::RefCast;

use open_dds::{
    models::{
        self, EnableAllOrSpecific, ModelGraphQlDefinition, ModelName, ModelV1, OrderableField,
    },
    types::{CustomTypeName, FieldName},
};

use std::collections::{BTreeMap, BTreeSet};
use std::iter;

/// resolve models
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    existing_graphql_types: &BTreeSet<ast::TypeName>,
    global_id_enabled_types: &BTreeMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    apollo_federation_entity_enabled_types: &BTreeMap<
        Qualified<CustomTypeName>,
        Option<Qualified<open_dds::models::ModelName>>,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<ModelsOutput, Error> {
    // resolve models
    // TODO: validate types
    let mut models = IndexMap::new();
    let mut global_id_models = BTreeMap::new();
    let mut graphql_types = existing_graphql_types.clone();
    let mut global_id_enabled_types = global_id_enabled_types.clone();
    let mut apollo_federation_entity_enabled_types = apollo_federation_entity_enabled_types.clone();

    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: model,
    } in &metadata_accessor.models
    {
        let mut resolved_model = resolve_model(
            subgraph,
            model,
            object_types,
            &mut global_id_enabled_types,
            &mut apollo_federation_entity_enabled_types,
            object_boolean_expression_types,
        )?;
        if resolved_model.global_id_source.is_some() {
            match global_id_models.insert(
                resolved_model.data_type.clone(),
                resolved_model.name.clone(),
            ) {
                None => {}
                Some(duplicate_model_name) => {
                    return Err(Error::DuplicateModelGlobalIdSource {
                        model_1: resolved_model.name,
                        model_2: duplicate_model_name,
                        object_type: resolved_model.data_type,
                    })
                }
            }
        }

        if let Some(model_source) = &model.source {
            resolve_model_source(
                model_source,
                &mut resolved_model,
                subgraph,
                data_connectors,
                object_types,
                scalar_types,
                object_boolean_expression_types,
            )?;
        }
        if let Some(model_graphql_definition) = &model.graphql {
            resolve_model_graphql_api(
                model_graphql_definition,
                &mut resolved_model,
                &mut graphql_types,
                data_connectors,
                &model.description,
                graphql_config,
            )?;
        }
        let qualified_model_name = Qualified::new(subgraph.to_string(), model.name.clone());
        if models
            .insert(qualified_model_name.clone(), resolved_model)
            .is_some()
        {
            return Err(Error::DuplicateModelDefinition {
                name: qualified_model_name,
            });
        }
    }
    Ok(ModelsOutput {
        models,
        graphql_types,
        apollo_federation_entity_enabled_types,
        global_id_enabled_types,
    })
}

fn resolve_filter_expression_type(
    model: &ModelV1,
    model_data_type: &Qualified<CustomTypeName>,
    subgraph: &str,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
) -> Result<Option<object_boolean_expressions::ObjectBooleanExpressionType>, Error> {
    model
        .filter_expression_type
        .as_ref()
        .map(|filter_expression_type| {
            let object_boolean_expression_type_name =
                Qualified::new(subgraph.to_string(), filter_expression_type.clone());
            let object_boolean_expression_type = object_boolean_expression_types
                .get(&object_boolean_expression_type_name)
                .ok_or_else(|| {
                    Error::from(
                        BooleanExpressionError::UnknownBooleanExpressionTypeInModel {
                            name: object_boolean_expression_type_name.clone(),
                            model: Qualified::new(subgraph.to_string(), model.name.clone()),
                        },
                    )
                })?;
            if object_boolean_expression_type.object_type != *model_data_type {
                return Err(Error::from(
                    BooleanExpressionError::BooleanExpressionTypeForInvalidObjectTypeInModel {
                        name: object_boolean_expression_type_name.clone(),
                        boolean_expression_object_type: object_boolean_expression_type
                            .object_type
                            .clone(),
                        model: Qualified::new(subgraph.to_string(), model.name.clone()),
                        model_object_type: model_data_type.clone(),
                    },
                ));
            }
            // This is also checked in resolve_model_graphql_api, but we want to disallow this even
            // if the model is not used in the graphql layer.
            if model.source.is_none() {
                return Err(Error::CannotUseFilterExpressionsWithoutSource {
                    model: Qualified::new(subgraph.to_string(), model.name.clone()),
                });
                // TODO: Compatibility of model source and the boolean expression type is checked in
                // resolve_model_source. Figure out a way to make this logic not scattered.
            }
            Ok(object_boolean_expression_type.clone())
        })
        .transpose()
}

fn resolve_orderable_fields(
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

fn resolve_model(
    subgraph: &str,
    model: &ModelV1,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    global_id_enabled_types: &mut BTreeMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    apollo_federation_entity_enabled_types: &mut BTreeMap<
        Qualified<CustomTypeName>,
        Option<Qualified<ModelName>>,
    >,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
) -> Result<Model, Error> {
    let qualified_object_type_name =
        Qualified::new(subgraph.to_string(), model.object_type.to_owned());
    let qualified_model_name = Qualified::new(subgraph.to_string(), model.name.clone());
    let object_type_representation = get_model_object_type_representation(
        object_types,
        &qualified_object_type_name,
        &qualified_model_name,
    )?;
    let mut global_id_source = None;
    if model.global_id_source {
        // Check if there are any global fields present in the related
        // object type, if the model is marked as a global source.
        if object_type_representation
            .object_type
            .global_id_fields
            .is_empty()
        {
            return Err(Error::NoGlobalFieldsPresentInGlobalIdSource {
                type_name: qualified_object_type_name,
                model_name: model.name.clone(),
            });
        }
        if !model.arguments.is_empty() {
            return Err(Error::ModelWithArgumentsAsGlobalIdSource {
                model_name: qualified_model_name,
            });
        }
        // model has `global_id_source`; insert into the BTreeMap of `global_id_enabled_types`
        match global_id_enabled_types.get_mut(&qualified_object_type_name) {
            None => {
                // this shouldn't happen; but for some reason the object type
                // containing globalIdFields is not inserted. Insert it now
                global_id_enabled_types.insert(
                    qualified_object_type_name.clone(),
                    vec![qualified_model_name.clone()],
                );
            }
            Some(model_names) => {
                model_names.push(qualified_model_name.clone());
            }
        }
        global_id_source = Some(NDCFieldSourceMapping {
            ndc_mapping: BTreeMap::new(),
        });
    };
    let mut apollo_federation_key_source = None;
    if model
        .graphql
        .as_ref()
        .and_then(|g| g.apollo_federation.as_ref().map(|a| a.entity_source))
        .unwrap_or_default()
    {
        // Check if there are any apollo federation keys present in the related
        // object type, if the model is marked as an apollo federation entity source.
        if object_type_representation
            .object_type
            .apollo_federation_config
            .is_some()
        {
            if !model.arguments.is_empty() {
                return Err(Error::ModelWithArgumentsAsApolloFederationEntitySource {
                    model_name: qualified_model_name,
                });
            }
            // model has `apollo_federation_entity_source`; insert into the BTreeMap of
            // `apollo_federation_entity_enabled_types`
            match apollo_federation_entity_enabled_types.get_mut(&qualified_object_type_name) {
                None => {
                    // the model's graphql configuration has `apollo_federation.entitySource` but the object type
                    // of the model doesn't have any apollo federation keys
                    return Err(Error::NoKeysFieldsPresentInEntitySource {
                        type_name: qualified_object_type_name,
                        model_name: model.name.clone(),
                    });
                }
                Some(type_name) => {
                    match type_name {
                        None => {
                            *type_name = Some(qualified_model_name.clone());
                        }
                        // Multiple models are marked as apollo federation entity source
                        Some(_) => {
                            return Err(Error::MultipleEntitySourcesForType {
                                type_name: qualified_object_type_name,
                            });
                        }
                    }
                }
            }
            apollo_federation_key_source = Some(NDCFieldSourceMapping {
                ndc_mapping: BTreeMap::new(),
            });
        }
    }

    let mut arguments = IndexMap::new();
    for argument in &model.arguments {
        if arguments
            .insert(
                argument.name.clone(),
                ArgumentInfo {
                    argument_type: mk_qualified_type_reference(&argument.argument_type, subgraph),
                    description: argument.description.clone(),
                },
            )
            .is_some()
        {
            return Err(Error::DuplicateModelArgumentDefinition {
                model_name: qualified_model_name,
                argument_name: argument.name.clone(),
            });
        }
    }

    let filter_expression_type = resolve_filter_expression_type(
        model,
        &qualified_object_type_name,
        subgraph,
        object_boolean_expression_types,
    )?;

    Ok(Model {
        name: qualified_model_name,
        data_type: qualified_object_type_name,
        type_fields: object_type_representation.object_type.fields.clone(),
        global_id_fields: object_type_representation
            .object_type
            .global_id_fields
            .clone(),
        arguments,
        graphql_api: ModelGraphQlApi::default(),
        source: None,
        global_id_source,
        apollo_federation_key_source,
        filter_expression_type,
        orderable_fields: resolve_orderable_fields(
            model,
            &object_type_representation.object_type.fields,
        )?,
    })
}

fn resolve_model_graphql_api(
    model_graphql_definition: &ModelGraphQlDefinition,
    model: &mut Model,
    existing_graphql_types: &mut BTreeSet<ast::TypeName>,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    model_description: &Option<String>,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<(), Error> {
    let model_name = &model.name;
    for select_unique in &model_graphql_definition.select_uniques {
        let mut unique_identifier_fields = IndexMap::new();
        for field_name in &select_unique.unique_identifier {
            let field_type = &model
                .type_fields
                .get(field_name)
                .ok_or_else(|| Error::UnknownFieldInUniqueIdentifier {
                    model_name: model_name.clone(),
                    field_name: field_name.clone(),
                })?
                .field_type;
            let ndc_column = model
                .source
                .as_ref()
                .map(|model_source| {
                    get_ndc_column_for_comparison(
                        &model.name,
                        &model.data_type,
                        model_source,
                        field_name,
                        data_connectors,
                        || "the unique identifier for select unique".to_string(),
                    )
                })
                .transpose()?;
            let unique_identifier_field = UniqueIdentifierField {
                field_type: field_type.clone(),
                ndc_column,
            };
            if unique_identifier_fields
                .insert(field_name.clone(), unique_identifier_field)
                .is_some()
            {
                return Err(Error::DuplicateFieldInUniqueIdentifier {
                    model_name: model_name.clone(),
                    field_name: field_name.clone(),
                });
            }
        }
        let select_unique_field_name = mk_name(&select_unique.query_root_field.0)?;
        let select_unique_description = if select_unique.description.is_some() {
            select_unique.description.clone()
        } else {
            model_description.as_ref().map(|description| {
                format!(
                    "Selects a single object from the model. Model description: {}",
                    description
                )
            })
        };
        model
            .graphql_api
            .select_uniques
            .push(SelectUniqueGraphQlDefinition {
                query_root_field: select_unique_field_name,
                unique_identifier: unique_identifier_fields,
                description: select_unique_description,
                deprecated: select_unique.deprecated.clone(),
            });
    }

    model.graphql_api.order_by_expression = model
        .source
        .as_ref()
        .map(
            |model_source: &ModelSource| -> Result<Option<ModelOrderByExpression>, Error> {
                let order_by_expression_type_name = match &model_graphql_definition
                    .order_by_expression_type
                {
                    None => Ok(None),
                    Some(type_name) => mk_name(type_name.0.as_str()).map(ast::TypeName).map(Some),
                }?;
                // TODO: (paritosh) should we check for conflicting graphql types for default order_by type name as well?
                store_new_graphql_type(
                    existing_graphql_types,
                    order_by_expression_type_name.as_ref(),
                )?;
                order_by_expression_type_name
                    .map(|order_by_type_name| {
                        let object_types::TypeMapping::Object { field_mappings, .. } = model_source
                            .type_mappings
                            .get(&model.data_type)
                            .ok_or(Error::TypeMappingRequired {
                                model_name: model_name.clone(),
                                type_name: model.data_type.clone(),
                                data_connector: model_source.data_connector.name.clone(),
                            })?;

                        let mut order_by_fields = BTreeMap::new();
                        for (field_name, field_mapping) in field_mappings.iter() {
                            order_by_fields.insert(
                                field_name.clone(),
                                OrderByExpressionInfo {
                                    ndc_column: field_mapping.column.clone(),
                                },
                            );
                        }

                        match &graphql_config.query.order_by_field_name {
                            None => Err(Error::GraphqlConfigError {
                                graphql_config_error:
                                    GraphqlConfigError::MissingOrderByInputFieldInGraphqlConfig,
                            }),
                            Some(order_by_field_name) => Ok(ModelOrderByExpression {
                                data_connector_name: model_source.data_connector.name.clone(),
                                order_by_type_name,
                                order_by_fields,
                                order_by_field_name: order_by_field_name.clone(),
                            }),
                        }
                    })
                    .transpose()
            },
        )
        .transpose()?
        .flatten();

    // record select_many root field
    model.graphql_api.select_many = match &model_graphql_definition.select_many {
        None => Ok(None),
        Some(gql_definition) => mk_name(&gql_definition.query_root_field.0).map(|f: ast::Name| {
            let select_many_description = if gql_definition.description.is_some() {
                gql_definition.description.clone()
            } else {
                model_description.as_ref().map(|description| {
                    format!(
                        "Selects multiple objects from the model. Model description: {}",
                        description
                    )
                })
            };
            Some(SelectManyGraphQlDefinition {
                query_root_field: f,
                description: select_many_description,
                deprecated: gql_definition.deprecated.clone(),
            })
        }),
    }?;

    // record limit and offset field names
    model.graphql_api.limit_field =
        graphql_config
            .query
            .limit_field_name
            .as_ref()
            .map(|limit_field| LimitFieldGraphqlConfig {
                field_name: limit_field.clone(),
            });

    model.graphql_api.offset_field =
        graphql_config
            .query
            .offset_field_name
            .as_ref()
            .map(|offset_field| OffsetFieldGraphqlConfig {
                field_name: offset_field.clone(),
            });

    if model.arguments.is_empty() {
        if model_graphql_definition.arguments_input_type.is_some() {
            return Err(Error::UnnecessaryModelArgumentsGraphQlInputConfiguration {
                model_name: model_name.clone(),
            });
        }
    } else {
        let arguments_input_type_name = match &model_graphql_definition.arguments_input_type {
            None => Ok(None),
            Some(type_name) => mk_name(type_name.0.as_str()).map(ast::TypeName).map(Some),
        }?;
        store_new_graphql_type(existing_graphql_types, arguments_input_type_name.as_ref())?;

        if let Some(type_name) = arguments_input_type_name {
            let argument_input_field_name = graphql_config
                .query
                .arguments_field_name
                .as_ref()
                .ok_or_else(|| Error::GraphqlConfigError {
                    graphql_config_error:
                        GraphqlConfigError::MissingArgumentsInputFieldInGraphqlConfig,
                })?;
            model.graphql_api.arguments_input_config = Some(ModelGraphqlApiArgumentsConfig {
                field_name: argument_input_field_name.clone(),
                type_name,
            });
        }
    }

    Ok(())
}

fn resolve_model_source(
    model_source: &models::ModelSource,
    model: &mut Model,
    subgraph: &str,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
) -> Result<(), Error> {
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
        .data_connectors_with_scalars
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
        ndc_object_type_name: DataConnectorObjectType::ref_cast(&source_collection.collection_type),
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

    if let Some(filter_expression) = &model.filter_expression_type {
        if filter_expression.data_connector_name != qualified_data_connector_name {
            return Err(Error::DifferentDataConnectorInFilterExpression {
                model: model.name.clone(),
                model_data_connector: qualified_data_connector_name.clone(),
                filter_expression_type: filter_expression.name.clone(),
                filter_expression_data_connector: filter_expression.data_connector_name.clone(),
            });
        }

        // The `ObjectBooleanExpressionType` allows specifying the `DataConnectorObjectType`, so we
        // must check it against the type specified in the models source. In future, we'll use
        // `BooleanExpressionType` which will defer to the model's choice of object by default, and
        // we can delete this check
        if let Some(data_connector_object_type) =
            &filter_expression.data_connector_object_type_dont_use_please
        {
            let collection_type =
                DataConnectorObjectType::ref_cast(&source_collection.collection_type);

            if data_connector_object_type != collection_type {
                return Err(Error::DifferentDataConnectorObjectTypeInFilterExpression {
                    model: model.name.clone(),
                    model_data_connector_object_type: collection_type.clone(),
                    filter_expression_type: filter_expression.name.clone(),
                    filter_expression_data_connector_object_type: data_connector_object_type
                        .clone(),
                });
            }
        }
    }

    let resolved_model_source = ModelSource {
        data_connector: data_connectors::DataConnectorLink::new(
            qualified_data_connector_name,
            data_connector_context.inner.url.clone(),
            data_connector_context.inner.headers,
        )?,
        collection: model_source.collection.clone(),
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
                    data_connectors,
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
                            data_connectors,
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

    model.source = Some(resolved_model_source);
    ndc_validation::validate_ndc(&model.name, model, &data_connector_context.inner.schema)?;
    Ok(())
}

/// Gets the `type_permissions::ObjectTypeWithPermissions` of the type identified with the
/// `data_type`, it will throw an error if the type is not found to be an object
/// or if the model has an unknown data type.
fn get_model_object_type_representation<'s>(
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

pub(crate) fn get_ndc_column_for_comparison<F: Fn() -> String>(
    model_name: &Qualified<ModelName>,
    model_data_type: &Qualified<CustomTypeName>,
    model_source: &ModelSource,
    field: &FieldName,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
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
    let scalars = &data_connectors
        .data_connectors_with_scalars
        .get(&model_source.data_connector.name)
        .ok_or(Error::UnknownModelDataConnector {
            model_name: model_name.clone(),
            data_connector: model_source.data_connector.name.clone(),
        })?
        .scalars;
    // Determine whether the ndc type is a simple scalar and get scalar type info
    let (_field_ndc_type_scalar, scalar_type_info) =
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
