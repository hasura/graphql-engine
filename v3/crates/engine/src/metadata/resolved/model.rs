use super::permission::{resolve_value_expression, ValueExpression};
use super::relationship::RelationshipTarget;
use super::stages::{
    data_connector_scalar_types, data_connector_type_mappings, graphql_config, scalar_types,
    type_permissions,
};
use super::typecheck;
use super::types::{
    collect_type_mapping_for_source, NdcColumnForComparison, ObjectBooleanExpressionType,
    TypeMappingToCollect,
};
use crate::metadata::resolved::argument::get_argument_mappings;

use crate::metadata::resolved::data_connector;
use crate::metadata::resolved::data_connector::DataConnectorLink;
use crate::metadata::resolved::error::{
    BooleanExpressionError, Error, GraphqlConfigError, RelationshipError,
};
use crate::metadata::resolved::ndc_validation;
use crate::metadata::resolved::subgraph::{
    deserialize_qualified_btreemap, mk_qualified_type_name, mk_qualified_type_reference,
    serialize_qualified_btreemap, ArgumentInfo, Qualified, QualifiedBaseType,
    QualifiedTypeReference,
};
use crate::metadata::resolved::types::store_new_graphql_type;
use crate::metadata::resolved::types::{mk_name, TypeMapping};
use crate::schema::types::output_type::relationship::{
    ModelTargetSource, PredicateRelationshipAnnotation,
};
use indexmap::IndexMap;
use lang_graphql::ast::common::{self as ast, Name};
use ndc_models;
use open_dds::permissions::{FieldIsNullPredicate, NullableModelPredicate, RelationshipPredicate};
use open_dds::types::Deprecated;
use open_dds::{
    arguments::ArgumentName,
    data_connector::DataConnectorName,
    models::{
        self, EnableAllOrSpecific, ModelGraphQlDefinition, ModelName, ModelV1, OrderableField,
    },
    permissions::{self, ModelPermissionsV1, Role},
    types::{CustomTypeName, FieldName, OperatorName},
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct UniqueIdentifierField {
    pub field_type: QualifiedTypeReference,
    pub ndc_column: Option<NdcColumnForComparison>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct SelectUniqueGraphQlDefinition {
    pub query_root_field: ast::Name,
    pub unique_identifier: IndexMap<FieldName, UniqueIdentifierField>,
    pub description: Option<String>,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct SelectManyGraphQlDefinition {
    pub query_root_field: ast::Name,
    pub description: Option<String>,
    pub deprecated: Option<Deprecated>,
}

// TODO: add support for aggregates
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct OrderByExpressionInfo {
    pub ndc_column: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelOrderByExpression {
    pub data_connector_name: Qualified<DataConnectorName>,
    pub order_by_type_name: ast::TypeName,
    pub order_by_fields: HashMap<FieldName, OrderByExpressionInfo>,
    pub order_by_field_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelGraphqlApiArgumentsConfig {
    pub field_name: Name,
    pub type_name: ast::TypeName,
}
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct LimitFieldGraphqlConfig {
    pub field_name: Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct OffsetFieldGraphqlConfig {
    pub field_name: Name,
}
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Default)]
pub struct ModelGraphQlApi {
    pub arguments_input_config: Option<ModelGraphqlApiArgumentsConfig>,
    pub select_uniques: Vec<SelectUniqueGraphQlDefinition>,
    pub select_many: Option<SelectManyGraphQlDefinition>,
    pub order_by_expression: Option<ModelOrderByExpression>,
    pub limit_field: Option<LimitFieldGraphqlConfig>,
    pub offset_field: Option<OffsetFieldGraphqlConfig>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelSource {
    pub data_connector: DataConnectorLink,
    pub collection: String,
    #[serde(
        serialize_with = "serialize_qualified_btreemap",
        deserialize_with = "deserialize_qualified_btreemap"
    )]
    pub type_mappings: BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    pub argument_mappings: HashMap<ArgumentName, String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum FilterPermission {
    AllowAll,
    Filter(ModelPredicate),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct SelectPermission {
    pub filter: FilterPermission,
    // pub allow_aggregations: bool,
    pub argument_presets: BTreeMap<ArgumentName, (QualifiedTypeReference, ValueExpression)>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ModelPredicate {
    UnaryFieldComparison {
        field: FieldName,
        ndc_column: String,
        operator: ndc_models::UnaryComparisonOperator,
    },
    BinaryFieldComparison {
        field: FieldName,
        ndc_column: String,
        operator: String,
        argument_type: QualifiedTypeReference,
        value: ValueExpression,
    },
    Relationship {
        relationship_info: PredicateRelationshipAnnotation,
        predicate: Box<ModelPredicate>,
    },
    And(Vec<ModelPredicate>),
    Or(Vec<ModelPredicate>),
    Not(Box<ModelPredicate>),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Model {
    pub name: Qualified<ModelName>,
    pub data_type: Qualified<CustomTypeName>,
    pub type_fields: IndexMap<FieldName, data_connector_type_mappings::FieldDefinition>,
    pub global_id_fields: Vec<FieldName>,
    pub arguments: IndexMap<ArgumentName, ArgumentInfo>,
    pub graphql_api: ModelGraphQlApi,
    pub source: Option<ModelSource>,
    pub select_permissions: Option<HashMap<Role, SelectPermission>>,
    pub global_id_source: Option<NDCFieldSourceMapping>,
    pub apollo_federation_key_source: Option<NDCFieldSourceMapping>,
    pub filter_expression_type: Option<ObjectBooleanExpressionType>,
    pub orderable_fields: Vec<OrderableField>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct NDCFieldSourceMapping {
    pub ndc_mapping: HashMap<FieldName, NdcColumnForComparison>,
}

fn resolve_filter_expression_type(
    model: &ModelV1,
    model_data_type: &Qualified<CustomTypeName>,
    subgraph: &str,
    boolean_expression_types: &HashMap<Qualified<CustomTypeName>, ObjectBooleanExpressionType>,
) -> Result<Option<ObjectBooleanExpressionType>, Error> {
    model
        .filter_expression_type
        .as_ref()
        .map(|filter_expression_type| {
            let boolean_expression_type_name =
                Qualified::new(subgraph.to_string(), filter_expression_type.clone());
            let boolean_expression_type = boolean_expression_types
                .get(&boolean_expression_type_name)
                .ok_or_else(|| {
                    Error::from(
                        BooleanExpressionError::UnknownBooleanExpressionTypeInModel {
                            name: boolean_expression_type_name.clone(),
                            model: Qualified::new(subgraph.to_string(), model.name.clone()),
                        },
                    )
                })?;
            if boolean_expression_type.object_type != *model_data_type {
                return Err(Error::from(
                    BooleanExpressionError::BooleanExpressionTypeForInvalidObjectTypeInModel {
                        name: boolean_expression_type_name.clone(),
                        boolean_expression_object_type: boolean_expression_type.object_type.clone(),
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
            Ok(boolean_expression_type.clone())
        })
        .transpose()
}

fn resolve_orderable_fields(
    model: &ModelV1,
    type_fields: &IndexMap<FieldName, data_connector_type_mappings::FieldDefinition>,
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

pub fn resolve_model(
    subgraph: &str,
    model: &ModelV1,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    global_id_enabled_types: &mut HashMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    apollo_federation_entity_enabled_types: &mut HashMap<
        Qualified<CustomTypeName>,
        Option<Qualified<ModelName>>,
    >,
    boolean_expression_types: &HashMap<Qualified<CustomTypeName>, ObjectBooleanExpressionType>,
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
        // model has `global_id_source`; insert into the hashmap of `global_id_enabled_types`
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
            ndc_mapping: HashMap::new(),
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
            // model has `apollo_federation_entity_source`; insert into the hashmap of
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
                ndc_mapping: HashMap::new(),
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
        boolean_expression_types,
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
        select_permissions: None,
        global_id_source,
        apollo_federation_key_source,
        filter_expression_type,
        orderable_fields: resolve_orderable_fields(
            model,
            &object_type_representation.object_type.fields,
        )?,
    })
}

// helper function to resolve ndc types to dds type based on scalar type representations
pub(crate) fn resolve_ndc_type(
    data_connector: &Qualified<DataConnectorName>,
    source_type: &ndc_models::Type,
    scalars: &HashMap<&str, data_connector_scalar_types::ScalarTypeWithRepresentationInfo>,
    subgraph: &str,
) -> Result<QualifiedTypeReference, Error> {
    match source_type {
        ndc_models::Type::Named { name } => {
            let scalar_type =
                scalars
                    .get(name.as_str())
                    .ok_or(Error::UnknownScalarTypeInDataConnector {
                        data_connector: data_connector.clone(),
                        scalar_type: name.clone(),
                    })?;
            scalar_type
                .representation
                .clone()
                .ok_or(Error::DataConnectorScalarRepresentationRequired {
                    data_connector: data_connector.clone(),
                    scalar_type: name.clone(),
                })
                .map(|ty| QualifiedTypeReference {
                    underlying_type: QualifiedBaseType::Named(mk_qualified_type_name(
                        &ty, subgraph,
                    )),
                    nullable: false,
                })
        }
        ndc_models::Type::Nullable { underlying_type } => {
            resolve_ndc_type(data_connector, underlying_type, scalars, subgraph).map(|ty| {
                QualifiedTypeReference {
                    underlying_type: ty.underlying_type,
                    nullable: true,
                }
            })
        }
        ndc_models::Type::Array { element_type } => {
            resolve_ndc_type(data_connector, element_type, scalars, subgraph).map(|ty| {
                QualifiedTypeReference {
                    underlying_type: QualifiedBaseType::List(Box::new(ty)),
                    nullable: false,
                }
            })
        }
        ndc_models::Type::Predicate { .. } => Err(Error::PredicateTypesUnsupported),
    }
}

#[allow(clippy::too_many_arguments)]
fn resolve_binary_operator(
    operator: &OperatorName,
    model_name: &Qualified<ModelName>,
    data_connector: &Qualified<DataConnectorName>,
    field_name: &FieldName,
    fields: &IndexMap<FieldName, data_connector_type_mappings::FieldDefinition>,
    scalars: &HashMap<&str, data_connector_scalar_types::ScalarTypeWithRepresentationInfo>,
    ndc_scalar_type: &ndc_models::ScalarType,
    subgraph: &str,
) -> Result<(String, QualifiedTypeReference), Error> {
    let field_definition =
        fields
            .get(field_name)
            .ok_or_else(|| Error::UnknownFieldInSelectPermissionsDefinition {
                field_name: field_name.clone(),
                model_name: model_name.clone(),
            })?;
    let comparison_operator_definition = &ndc_scalar_type
        .comparison_operators
        .get(&operator.0)
        .ok_or_else(|| Error::InvalidOperator {
            model_name: model_name.clone(),
            operator_name: operator.clone(),
        })?;
    match comparison_operator_definition {
        ndc_models::ComparisonOperatorDefinition::Equal => {
            Ok((operator.0.clone(), field_definition.field_type.clone()))
        }
        ndc_models::ComparisonOperatorDefinition::In => Ok((
            operator.0.clone(),
            QualifiedTypeReference {
                underlying_type: QualifiedBaseType::List(Box::new(
                    field_definition.field_type.clone(),
                )),
                nullable: true,
            },
        )),
        ndc_models::ComparisonOperatorDefinition::Custom { argument_type } => Ok((
            operator.0.clone(),
            resolve_ndc_type(data_connector, argument_type, scalars, subgraph)?,
        )),
    }
}

fn resolve_model_predicate(
    model_predicate: &permissions::ModelPredicate,
    model: &Model,
    subgraph: &str,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    fields: &IndexMap<FieldName, data_connector_type_mappings::FieldDefinition>,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    models: &IndexMap<Qualified<ModelName>, Model>,
    // type_representation: &TypeRepresentation,
) -> Result<ModelPredicate, Error> {
    match model_predicate {
        permissions::ModelPredicate::FieldComparison(permissions::FieldComparisonPredicate {
            field,
            operator,
            value,
        }) => {
            // TODO: (anon) typecheck the value expression with the field
            // TODO: resolve the "in" operator too (ndc_models::BinaryArrayComparisonOperator)
            if let Some(model_source) = &model.source {
                // Get field mappings of model data type
                let TypeMapping::Object { field_mappings, .. } = model_source
                    .type_mappings
                    .get(&model.data_type)
                    .ok_or(Error::TypeMappingRequired {
                        model_name: model.name.clone(),
                        type_name: model.data_type.clone(),
                        data_connector: model_source.data_connector.name.clone(),
                    })?;

                // Determine field_mapping for the predicate field
                let field_mapping = field_mappings.get(field).ok_or_else(|| {
                    Error::UnknownFieldInSelectPermissionsDefinition {
                        field_name: field.clone(),
                        model_name: model.name.clone(),
                    }
                })?;
                // Determine ndc type of the field
                let field_ndc_type = &field_mapping.column_type;

                // Determine whether the ndc type is a simple scalar
                // Get available scalars defined in the data connector
                let scalars = &data_connectors
                    .data_connectors_with_scalars
                    .get(&model_source.data_connector.name)
                    .ok_or(Error::UnknownModelDataConnector {
                        model_name: model.name.clone(),
                        data_connector: model_source.data_connector.name.clone(),
                    })?
                    .scalars;

                // Get scalar type info from the data connector
                let (_, scalar_type_info) =
                    data_connector::get_simple_scalar(field_ndc_type.clone(), scalars).ok_or_else(
                        || Error::UnsupportedFieldInSelectPermissionsPredicate {
                            field_name: field.clone(),
                            model_name: model.name.clone(),
                        },
                    )?;

                let (resolved_operator, argument_type) = resolve_binary_operator(
                    operator,
                    &model.name,
                    &model_source.data_connector.name,
                    field,
                    fields,
                    scalars,
                    scalar_type_info.scalar_type,
                    subgraph,
                )?;
                Ok(ModelPredicate::BinaryFieldComparison {
                    field: field.clone(),
                    ndc_column: field_mapping.column.clone(),
                    operator: resolved_operator,
                    argument_type,
                    value: resolve_value_expression(value.clone()),
                })
            } else {
                Err(Error::ModelSourceRequiredForPredicate {
                    model_name: model.name.clone(),
                })
            }
        }
        permissions::ModelPredicate::FieldIsNull(FieldIsNullPredicate { field }) => {
            if let Some(model_source) = &model.source {
                // Get field mappings of model data type
                let TypeMapping::Object { field_mappings, .. } = model_source
                    .type_mappings
                    .get(&model.data_type)
                    .ok_or(Error::TypeMappingRequired {
                        model_name: model.name.clone(),
                        type_name: model.data_type.clone(),
                        data_connector: model_source.data_connector.name.clone(),
                    })?;
                // Determine field_mapping for the predicate field
                let field_mapping = field_mappings.get(field).ok_or_else(|| {
                    Error::UnknownFieldInSelectPermissionsDefinition {
                        field_name: field.clone(),
                        model_name: model.name.clone(),
                    }
                })?;

                Ok(ModelPredicate::UnaryFieldComparison {
                    field: field.clone(),
                    ndc_column: field_mapping.column.clone(),
                    operator: ndc_models::UnaryComparisonOperator::IsNull,
                })
            } else {
                Err(Error::ModelSourceRequiredForPredicate {
                    model_name: model.name.clone(),
                })
            }
        }
        permissions::ModelPredicate::Relationship(RelationshipPredicate { name, predicate }) => {
            if let Some(nested_predicate) = predicate {
                let object_type_representation = get_model_object_type_representation(
                    object_types,
                    &model.data_type,
                    &model.name,
                )?;
                let relationship_field_name = mk_name(&name.0)?;
                let relationship = &object_type_representation
                    .object_type
                    .relationships
                    .get(&relationship_field_name)
                    .ok_or_else(|| Error::UnknownRelationshipInSelectPermissionsPredicate {
                        relationship_name: name.clone(),
                        model_name: model.name.clone(),
                        type_name: model.data_type.clone(),
                    })?;

                match &relationship.target {
                    RelationshipTarget::Command { .. } => Err(Error::UnsupportedFeature {
                        message: "Predicate cannot be built using command relationships"
                            .to_string(),
                    }),
                    RelationshipTarget::Model {
                        model_name,
                        relationship_type,
                        target_typename,
                        mappings,
                    } => {
                        let target_model = models.get(model_name).ok_or_else(|| {
                            Error::UnknownModelUsedInRelationshipSelectPermissionsPredicate {
                                model_name: model.name.clone(),
                                target_model_name: model_name.clone(),
                                relationship_name: name.clone(),
                            }
                        })?;

                        // predicates with relationships is currently only supported for local relationships
                        if let (Some(target_model_source), Some(model_source)) =
                            (&target_model.source, &model.source)
                        {
                            if target_model_source.data_connector.name
                                == model_source.data_connector.name
                            {
                                let target_source = ModelTargetSource::from_model_source(
                                    target_model_source,
                                    relationship,
                                )
                                .map_err(|_| Error::RelationshipError {
                                    relationship_error:
                                        RelationshipError::NoRelationshipCapabilitiesDefined {
                                            relationship_name: relationship.name.clone(),
                                            type_name: model.data_type.clone(),
                                            data_connector_name: target_model_source
                                                .data_connector
                                                .name
                                                .clone(),
                                        },
                                })?;

                                let annotation = PredicateRelationshipAnnotation {
                                    source_type: relationship.source.clone(),
                                    relationship_name: relationship.name.clone(),
                                    target_model_name: model_name.clone(),
                                    target_source: target_source.clone(),
                                    target_type: target_typename.clone(),
                                    relationship_type: relationship_type.clone(),
                                    mappings: mappings.clone(),
                                    source_data_connector: model_source.data_connector.clone(),
                                    source_type_mappings: model_source.type_mappings.clone(),
                                };

                                let target_model_predicate = resolve_model_predicate(
                                    nested_predicate,
                                    target_model,
                                    // local relationships exists in the same subgraph as the source model
                                    subgraph,
                                    data_connectors,
                                    &target_model.type_fields,
                                    object_types,
                                    models,
                                )?;

                                Ok(ModelPredicate::Relationship {
                                    relationship_info: annotation,
                                    predicate: Box::new(target_model_predicate),
                                })
                            } else {
                                Err(Error::UnsupportedFeature {
                                    message: "Predicate cannot be built using remote relationships"
                                        .to_string(),
                                })
                            }
                        } else {
                            Err(
                                Error::ModelAndTargetSourceRequiredForRelationshipPredicate {
                                    source_model_name: model.name.clone(),
                                    target_model_name: target_model.name.clone(),
                                },
                            )
                        }
                    }
                }
            } else {
                Err(Error::NoPredicateDefinedForRelationshipPredicate {
                    model_name: model.name.clone(),
                    relationship_name: name.clone(),
                })
            }
        }
        permissions::ModelPredicate::Not(predicate) => {
            let resolved_predicate = resolve_model_predicate(
                predicate,
                model,
                subgraph,
                data_connectors,
                fields,
                object_types,
                models,
            )?;
            Ok(ModelPredicate::Not(Box::new(resolved_predicate)))
        }
        permissions::ModelPredicate::And(predicates) => {
            let mut resolved_predicates = Vec::new();
            for predicate in predicates {
                resolved_predicates.push(resolve_model_predicate(
                    predicate,
                    model,
                    subgraph,
                    data_connectors,
                    fields,
                    object_types,
                    models,
                )?);
            }
            Ok(ModelPredicate::And(resolved_predicates))
        }
        permissions::ModelPredicate::Or(predicates) => {
            let mut resolved_predicates = Vec::new();
            for predicate in predicates {
                resolved_predicates.push(resolve_model_predicate(
                    predicate,
                    model,
                    subgraph,
                    data_connectors,
                    fields,
                    object_types,
                    models,
                )?);
            }
            Ok(ModelPredicate::Or(resolved_predicates))
        }
    }
}

pub fn resolve_model_select_permissions(
    model: &Model,
    subgraph: &str,
    model_permissions: &ModelPermissionsV1,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    models: &IndexMap<Qualified<ModelName>, Model>,
) -> Result<HashMap<Role, SelectPermission>, Error> {
    let mut validated_permissions = HashMap::new();
    for model_permission in &model_permissions.permissions {
        if let Some(select) = &model_permission.select {
            let resolved_predicate = match &select.filter {
                NullableModelPredicate::NotNull(model_predicate) => resolve_model_predicate(
                    model_predicate,
                    model,
                    subgraph,
                    data_connectors,
                    &model.type_fields,
                    object_types,
                    models,
                )
                .map(FilterPermission::Filter)?,
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

                match model.arguments.get(&argument_preset.argument) {
                    Some(argument) => {
                        // if our value is a literal, typecheck it against expected type
                        typecheck::typecheck_value_expression(
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

                        let resolved_argument_value =
                            resolve_value_expression(argument_preset.value.clone());

                        argument_presets.insert(
                            argument_preset.argument.clone(),
                            (argument.argument_type.clone(), resolved_argument_value),
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

pub(crate) fn get_ndc_column_for_comparison<F: Fn() -> String>(
    model_name: &Qualified<ModelName>,
    model_data_type: &Qualified<CustomTypeName>,
    model_source: &ModelSource,
    field: &FieldName,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    comparison_location: F,
) -> Result<NdcColumnForComparison, Error> {
    // Get field mappings of model data type
    let TypeMapping::Object { field_mappings, .. } = model_source
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
        data_connector::get_simple_scalar(field_ndc_type.clone(), scalars).ok_or_else(|| {
            Error::UncomparableNonScalarFieldType {
                comparison_location: comparison_location(),
                field_name: field.clone(),
                model_name: model_name.clone(),
            }
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

pub fn resolve_model_graphql_api(
    model_graphql_definition: &ModelGraphQlDefinition,
    model: &mut Model,
    existing_graphql_types: &mut HashSet<ast::TypeName>,
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
                        let TypeMapping::Object { field_mappings, .. } = model_source
                            .type_mappings
                            .get(&model.data_type)
                            .ok_or(Error::TypeMappingRequired {
                                model_name: model_name.clone(),
                                type_name: model.data_type.clone(),
                                data_connector: model_source.data_connector.name.clone(),
                            })?;

                        let mut order_by_fields = HashMap::new();
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

pub fn resolve_model_source(
    model_source: &models::ModelSource,
    model: &mut Model,
    subgraph: &str,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_types: &HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    data_connector_type_mappings: &data_connector_type_mappings::DataConnectorTypeMappings,
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
        .iter()
        .find(|collection_info| collection_info.name == *model_source.collection)
        .ok_or_else(|| Error::UnknownModelCollection {
            model_name: model.name.clone(),
            data_connector: qualified_data_connector_name.clone(),
            collection: model_source.collection.clone(),
        })?;

    // Get the mappings of arguments and any type mappings that need resolving from the arguments
    let (argument_mappings, argument_type_mappings_to_collect) = get_argument_mappings(
        &model.arguments,
        &model_source.argument_mapping,
        &source_collection.arguments,
        object_types,
        scalar_types,
    )
    .map_err(|err| Error::ModelCollectionArgumentMappingError {
        data_connector_name: qualified_data_connector_name.clone(),
        model_name: model.name.clone(),
        collection_name: model_source.collection.clone(),
        error: err,
    })?;

    // Collect type mappings.
    let mut type_mappings = BTreeMap::new();
    let source_collection_type_mapping_to_collect = TypeMappingToCollect {
        type_name: &model.data_type,
        ndc_object_type_name: source_collection.collection_type.as_str(),
    };
    for type_mapping_to_collect in iter::once(&source_collection_type_mapping_to_collect)
        .chain(argument_type_mappings_to_collect.iter())
    {
        collect_type_mapping_for_source(
            type_mapping_to_collect,
            data_connector_type_mappings,
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

        if filter_expression.data_connector_object_type != source_collection.collection_type {
            return Err(Error::DifferentDataConnectorObjectTypeInFilterExpression {
                model: model.name.clone(),
                model_data_connector_object_type: source_collection.collection_type.clone(),
                filter_expression_type: filter_expression.name.clone(),
                filter_expression_data_connector_object_type: filter_expression
                    .data_connector_object_type
                    .clone(),
            });
        }
    }

    let resolved_model_source = ModelSource {
        data_connector: DataConnectorLink::new(
            qualified_data_connector_name,
            data_connector_context.inner.url.clone(),
            data_connector_context.inner.headers,
        )?,
        collection: model_source.collection.clone(),
        type_mappings,
        argument_mappings,
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
    ndc_validation::validate_ndc(&model.name, model, data_connector_context.inner.schema)?;
    Ok(())
}

/// Gets the `type_permissions::ObjectTypeWithPermissions` of the type identified with the
/// `data_type`, it will throw an error if the type is not found to be an object
/// or if the model has an unknown data type.
pub(crate) fn get_model_object_type_representation<'s>(
    object_types: &'s HashMap<
        Qualified<CustomTypeName>,
        type_permissions::ObjectTypeWithPermissions,
    >,
    data_type: &Qualified<CustomTypeName>,
    model_name: &Qualified<ModelName>,
) -> Result<&'s type_permissions::ObjectTypeWithPermissions, crate::metadata::resolved::error::Error>
{
    match object_types.get(data_type) {
        Some(object_type_representation) => Ok(object_type_representation),
        None => Err(Error::UnknownModelDataType {
            model_name: model_name.clone(),
            data_type: data_type.clone(),
        }),
    }
}
