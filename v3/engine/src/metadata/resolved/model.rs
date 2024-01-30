use crate::metadata::resolved::argument::get_argument_mappings;
use crate::metadata::resolved::data_connector::get_simple_scalar;
use crate::metadata::resolved::data_connector::{DataConnector, DataConnectorContext};
use crate::metadata::resolved::error::Error;
use crate::metadata::resolved::graphql_config::GraphqlConfig;
use crate::metadata::resolved::ndc_validation;
use crate::metadata::resolved::subgraph::{
    deserialize_qualified_btreemap, mk_qualified_type_name, mk_qualified_type_reference,
    serialize_qualified_btreemap, ArgumentInfo, Qualified, QualifiedBaseType,
    QualifiedTypeReference,
};
use crate::metadata::resolved::types::check_conflicting_graphql_types;
use crate::metadata::resolved::types::{mk_name, FieldDefinition, TypeMapping};
use crate::metadata::resolved::types::{
    resolve_type_mappings, ScalarTypeInfo, TypeMappingToResolve, TypeRepresentation,
};
use crate::schema::types::output_type::relationship::{
    ModelTargetSource, PredicateRelationshipAnnotation,
};
use indexmap::IndexMap;
use lang_graphql::ast::common::{self as ast, Name};
use ndc_client as ndc;
use open_dds::permissions::{NullableModelPredicate, RelationshipPredicate};
use open_dds::{
    arguments::ArgumentName,
    data_connector::DataConnectorName,
    models::{
        self, EnableAllOrSpecific, FilterableField, ModelGraphQlDefinition, ModelName, ModelV1,
        OperatorName, OrderableField,
    },
    permissions::{self, ModelPermissionsV1, Role, ValueExpression},
    types::{CustomTypeName, FieldName},
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter;

use super::relationship::RelationshipTarget;
use super::types::ObjectTypeRepresentation;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct SelectUniqueGraphQlDefinition {
    pub query_root_field: ast::Name,
    pub unique_identifier: IndexMap<FieldName, QualifiedTypeReference>,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct SelectManyGraphQlDefinition {
    pub query_root_field: ast::Name,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ComparisonExpressionInfo {
    pub data_connector_name: Qualified<DataConnectorName>,
    pub scalar_type_name: String,
    pub type_name: ast::TypeName,
    pub ndc_column: String,
    pub operators: BTreeMap<String, QualifiedTypeReference>,
    pub is_null_operator_name: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelFilterExpressionGraphqlConfig {
    pub where_field_name: ast::Name,
    pub and_operator_name: ast::Name,
    pub or_operator_name: ast::Name,
    pub not_operator_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelFilterExpression {
    pub where_type_name: ast::TypeName,
    pub scalar_fields: HashMap<FieldName, ComparisonExpressionInfo>,
    pub filter_graphql_config: ModelFilterExpressionGraphqlConfig,
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
    pub filter_expression: Option<ModelFilterExpression>,
    pub order_by_expression: Option<ModelOrderByExpression>,
    pub limit_field: Option<LimitFieldGraphqlConfig>,
    pub offset_field: Option<OffsetFieldGraphqlConfig>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelSource {
    pub data_connector: DataConnector,
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
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ModelPredicate {
    UnaryFieldComparison {
        field: FieldName,
        ndc_column: String,
        operator: ndc_client::models::UnaryComparisonOperator,
    },
    BinaryFieldComparison {
        field: FieldName,
        ndc_column: String,
        operator: ndc_client::models::BinaryComparisonOperator,
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
    pub type_fields: IndexMap<FieldName, FieldDefinition>,
    pub global_id_fields: Vec<FieldName>,
    pub arguments: IndexMap<ArgumentName, ArgumentInfo>,
    pub graphql_api: ModelGraphQlApi,
    pub source: Option<ModelSource>,
    pub select_permissions: Option<HashMap<Role, SelectPermission>>,
    pub global_id_source: bool,
    pub filterable_fields: Vec<FilterableField>,
    pub orderable_fields: Vec<OrderableField>,
}

fn resolve_filterable_fields(
    model: &ModelV1,
    type_fields: &IndexMap<FieldName, FieldDefinition>,
) -> Result<Vec<FilterableField>, Error> {
    for field in &model.filterable_fields {
        // Check for unknown filterable field
        if !type_fields.contains_key(&field.field_name) {
            return Err(Error::UnknownFieldInFilterableFields {
                model_name: model.name.clone(),
                field_name: field.field_name.clone(),
            });
        }
        // As of now, only `"enableAll": true` is allowed for field operators
        match &field.operators {
            EnableAllOrSpecific::EnableAll(true) => {}
            _ => {
                return Err(Error::UnsupportedFeature {
                    message: "Field level comparison operator configuration is not fully supported yet. Please use \"enableAll\":true.".to_string(),
                })
            }
        }
    }
    // Model filterable fields should have all type fields
    if model.filterable_fields.len() != type_fields.len() {
        return Err(Error::UnsupportedFeature {
            message: "Field level comparison operator configuration is not fully supported yet. Please add all fields in filterable_fields.".to_string(),
        });
    }
    Ok(model.filterable_fields.clone())
}

fn resolve_orderable_fields(
    model: &ModelV1,
    type_fields: &IndexMap<FieldName, FieldDefinition>,
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
    types: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
    global_id_enabled_types: &mut HashMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
) -> Result<Model, Error> {
    let qualified_object_type_name =
        Qualified::new(subgraph.to_string(), model.object_type.to_owned());
    let qualified_model_name = Qualified::new(subgraph.to_string(), model.name.clone());
    let object_type_representation = get_model_object_type_representation(
        types,
        &qualified_object_type_name,
        &qualified_model_name,
    )?;
    if model.global_id_source {
        // Check if there are any global fields present in the related
        // object type, if the model is marked as a global source.
        if object_type_representation.global_id_fields.is_empty() {
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
    };

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

    Ok(Model {
        name: qualified_model_name,
        data_type: qualified_object_type_name,
        type_fields: object_type_representation.fields.clone(),
        global_id_fields: object_type_representation.global_id_fields.clone(),
        arguments,
        graphql_api: ModelGraphQlApi::default(),
        source: None,
        select_permissions: None,
        global_id_source: model.global_id_source,
        filterable_fields: resolve_filterable_fields(model, &object_type_representation.fields)?,
        orderable_fields: resolve_orderable_fields(model, &object_type_representation.fields)?,
    })
}

// helper function to resolve ndc types to dds type based on scalar type representations
fn resolve_ndc_type(
    data_connector: &Qualified<DataConnectorName>,
    source_type: &ndc::models::Type,
    scalars: &HashMap<&str, ScalarTypeInfo>,
    subgraph: &str,
) -> Result<QualifiedTypeReference, Error> {
    match source_type {
        ndc::models::Type::Named { name } => {
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
        ndc::models::Type::Nullable { underlying_type } => {
            resolve_ndc_type(data_connector, underlying_type, scalars, subgraph).map(|ty| {
                QualifiedTypeReference {
                    underlying_type: ty.underlying_type,
                    nullable: true,
                }
            })
        }
        ndc::models::Type::Array { element_type } => {
            resolve_ndc_type(data_connector, element_type, scalars, subgraph).map(|ty| {
                QualifiedTypeReference {
                    underlying_type: QualifiedBaseType::List(Box::new(ty)),
                    nullable: false,
                }
            })
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn resolve_binary_operator(
    operator: &OperatorName,
    model_name: &Qualified<ModelName>,
    data_connector: &Qualified<DataConnectorName>,
    field_name: &FieldName,
    fields: &IndexMap<FieldName, FieldDefinition>,
    scalars: &HashMap<&str, ScalarTypeInfo>,
    ndc_scalar_type: &ndc_client::models::ScalarType,
    subgraph: &str,
) -> Result<
    (
        ndc_client::models::BinaryComparisonOperator,
        QualifiedTypeReference,
    ),
    Error,
> {
    match operator.0.as_str() {
        "_eq" => {
            let field_definition = fields.get(field_name).ok_or_else(|| {
                Error::UnknownFieldInSelectPermissionsDefinition {
                    field_name: field_name.clone(),
                    model_name: model_name.clone(),
                }
            })?;
            Ok((
                ndc_client::models::BinaryComparisonOperator::Equal,
                field_definition.field_type.clone(),
            ))
        }
        _ => {
            let argument_ndc_type = &ndc_scalar_type
                .comparison_operators
                .get(&operator.0)
                .ok_or_else(|| Error::InvalidOperator {
                    model_name: model_name.clone(),
                    operator_name: operator.clone(),
                })?
                .argument_type;
            Ok((
                ndc_client::models::BinaryComparisonOperator::Other {
                    name: operator.0.clone(),
                },
                resolve_ndc_type(data_connector, argument_ndc_type, scalars, subgraph)?,
            ))
        }
    }
}

fn resolve_model_predicate(
    model_predicate: &permissions::ModelPredicate,
    model: &Model,
    subgraph: &str,
    data_connectors: &HashMap<Qualified<DataConnectorName>, DataConnectorContext>,
    fields: &IndexMap<FieldName, FieldDefinition>,
    types: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
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
            // TODO: resolve the "in" operator too (ndc_client::models::BinaryArrayComparisonOperator)
            if let Some(model_source) = &model.source {
                // Get field mappings of model data type
                let TypeMapping::Object { field_mappings } = model_source
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
                let field_ndc_type_scalar =
                    get_simple_scalar(field_ndc_type.clone()).ok_or_else(|| {
                        Error::UnsupportedFieldInSelectPermissionsPredicate {
                            field_name: field.clone(),
                            model_name: model.name.clone(),
                        }
                    })?;
                // Get available scalars defined in the data connector
                let scalars = &data_connectors
                    .get(&model_source.data_connector.name)
                    .ok_or(Error::UnknownModelDataConnector {
                        model_name: model.name.clone(),
                        data_connector: model_source.data_connector.name.clone(),
                    })?
                    .scalars;
                // Get scalar type info from the data connector
                let scalar_type_info =
                    scalars.get(field_ndc_type_scalar.as_str()).ok_or_else(|| {
                        Error::UnknownScalarTypeInDataConnector {
                            scalar_type: field_ndc_type_scalar.clone(),
                            data_connector: model_source.data_connector.name.clone(),
                        }
                    })?;

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
                    value: value.clone(),
                })
            } else {
                Err(Error::ModelSourceRequiredForPredicate {
                    model_name: model.name.clone(),
                })
            }
        }
        permissions::ModelPredicate::FieldIsNull { field } => {
            if let Some(model_source) = &model.source {
                // Get field mappings of model data type
                let TypeMapping::Object { field_mappings } = model_source
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
                    operator: ndc_client::models::UnaryComparisonOperator::IsNull,
                })
            } else {
                Err(Error::ModelSourceRequiredForPredicate {
                    model_name: model.name.clone(),
                })
            }
        }
        permissions::ModelPredicate::Relationship(RelationshipPredicate { name, predicate }) => {
            if let Some(nested_predicate) = predicate {
                let object_type_representation =
                    get_model_object_type_representation(types, &model.data_type, &model.name)?;
                let relationship_field_name = mk_name(&name.0)?;
                let relationship = &object_type_representation
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
                                .map_err(|_| Error::NoRelationshipCapabilitiesDefined {
                                    relationship_name: relationship.name.clone(),
                                    type_name: model.data_type.clone(),
                                    data_connector_name: target_model_source
                                        .data_connector
                                        .name
                                        .clone(),
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
                                    types,
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
                types,
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
                    types,
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
                    types,
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
    data_connectors: &HashMap<Qualified<DataConnectorName>, DataConnectorContext>,
    types: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
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
                    types,
                    models,
                )
                .map(FilterPermission::Filter)?,
                NullableModelPredicate::Null(()) => FilterPermission::AllowAll,
            };
            let resolved_permission = SelectPermission {
                filter: resolved_predicate.clone(),
            };
            validated_permissions.insert(model_permission.role.clone(), resolved_permission);
        }
    }
    Ok(validated_permissions)
}

pub fn resolve_model_graphql_api(
    model_graphql_definition: &ModelGraphQlDefinition,
    model: &mut Model,
    subgraph: &str,
    existing_graphql_types: &mut HashSet<ast::TypeName>,
    data_connectors: &HashMap<Qualified<DataConnectorName>, DataConnectorContext>,
    model_description: &Option<String>,
    graphql_config: &GraphqlConfig,
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
            if unique_identifier_fields
                .insert(field_name.clone(), field_type.clone())
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
                check_conflicting_graphql_types(
                    existing_graphql_types,
                    order_by_expression_type_name.as_ref(),
                )?;
                order_by_expression_type_name
                    .map(|order_by_type_name| {
                        let TypeMapping::Object { field_mappings } = model_source
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
                            None => Err(Error::MissingOrderByInputFieldInGraphqlConfig),
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

    // record filter expression info
    model.graphql_api.filter_expression = model
        .source
        .as_ref()
        .map(
            |model_source: &ModelSource| -> Result<Option<ModelFilterExpression>, Error> {
                let filter_expression_type_name = match &model_graphql_definition
                    .filter_expression_type
                {
                    None => Ok(None),
                    Some(type_name) => mk_name(type_name.0.as_str()).map(ast::TypeName).map(Some),
                }?;
                check_conflicting_graphql_types(
                    existing_graphql_types,
                    filter_expression_type_name.as_ref(),
                )?;
                filter_expression_type_name
                    .map(|where_type_name| {
                        let mut scalar_fields = HashMap::new();
                        let scalar_types = &data_connectors
                            .get(&model_source.data_connector.name)
                            .ok_or(Error::UnknownModelDataConnector {
                                model_name: model_name.clone(),
                                data_connector: model_source.data_connector.name.clone(),
                            })?
                            .scalars;

                        let TypeMapping::Object { field_mappings } = model_source
                            .type_mappings
                            .get(&model.data_type)
                            .ok_or(Error::TypeMappingRequired {
                                model_name: model_name.clone(),
                                type_name: model.data_type.clone(),
                                data_connector: model_source.data_connector.name.clone(),
                            })?;

                        let filter_graphql_config = graphql_config
                            .query
                            .filter_input_config
                            .as_ref()
                            .ok_or_else(|| Error::MissingFilterInputFieldInGraphqlConfig)?;

                        for (field_name, field_mapping) in field_mappings.iter() {
                            // Generate comparison expression for fields mapped to simple scalar type
                            if let Some(scalar_type_name) =
                                get_simple_scalar(field_mapping.column_type.clone())
                            {
                                let scalar_type_info = scalar_types
                                    .get(scalar_type_name.as_str())
                                    .ok_or(Error::UnknownScalarTypeInDataConnector {
                                        scalar_type: scalar_type_name.clone(),
                                        data_connector: model_source.data_connector.name.clone(),
                                    })?;

                                if let Some(graphql_type_name) =
                                    &scalar_type_info.comparison_expression_name.clone()
                                {
                                    let mut operators = BTreeMap::new();
                                    for (op_name, op_definition) in
                                        scalar_type_info.scalar_type.comparison_operators.iter()
                                    {
                                        operators.insert(
                                            op_name.clone(),
                                            resolve_ndc_type(
                                                &model_source.data_connector.name,
                                                &op_definition.argument_type,
                                                scalar_types,
                                                subgraph,
                                            )?,
                                        );
                                    }
                                    // equal operator
                                    let eq_scalar_type_name = scalar_type_info
                                        .representation
                                        .as_ref()
                                        .ok_or(Error::DataConnectorScalarRepresentationRequired {
                                            data_connector: model_source
                                                .data_connector
                                                .name
                                                .clone(),
                                            scalar_type: scalar_type_name.clone(),
                                        })?
                                        .clone();
                                    operators.insert(
                                        "_eq".to_string(),
                                        QualifiedTypeReference {
                                            underlying_type: QualifiedBaseType::Named(
                                                mk_qualified_type_name(
                                                    &eq_scalar_type_name,
                                                    subgraph,
                                                ),
                                            ),
                                            nullable: false,
                                        },
                                    );
                                    // Register scalar comparison field only if it contains non-zero operators.
                                    if !operators.is_empty() {
                                        scalar_fields.insert(
                                            field_name.clone(),
                                            ComparisonExpressionInfo {
                                                data_connector_name: model_source
                                                    .data_connector
                                                    .name
                                                    .clone(),
                                                scalar_type_name: scalar_type_name.clone(),
                                                type_name: graphql_type_name.clone(),
                                                ndc_column: field_mapping.column.clone(),
                                                operators,
                                                is_null_operator_name: filter_graphql_config
                                                    .operator_names
                                                    .is_null
                                                    .to_string(),
                                            },
                                        );
                                    };
                                }
                            }
                        }

                        Ok(ModelFilterExpression {
                            where_type_name,
                            scalar_fields,
                            filter_graphql_config: (ModelFilterExpressionGraphqlConfig {
                                where_field_name: filter_graphql_config.where_field_name.clone(),
                                and_operator_name: filter_graphql_config.operator_names.and.clone(),
                                or_operator_name: filter_graphql_config.operator_names.or.clone(),
                                not_operator_name: filter_graphql_config.operator_names.not.clone(),
                            }),
                        })
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
        check_conflicting_graphql_types(
            existing_graphql_types,
            arguments_input_type_name.as_ref(),
        )?;

        if let Some(type_name) = arguments_input_type_name {
            let argument_input_field_name = graphql_config
                .query
                .arguments_field_name
                .as_ref()
                .ok_or_else(|| Error::MissingArgumentsInputFieldInGraphqlConfig)?;
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
    data_connectors: &HashMap<Qualified<DataConnectorName>, DataConnectorContext>,
    types: &HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
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
        .get(&qualified_data_connector_name)
        .ok_or_else(|| Error::UnknownModelDataConnector {
            model_name: model.name.clone(),
            data_connector: qualified_data_connector_name.clone(),
        })?;

    let source_collection = data_connector_context
        .schema
        .collections
        .iter()
        .find(|collection_info| collection_info.name == *model_source.collection)
        .ok_or_else(|| Error::UnknownModelCollection {
            model_name: model.name.clone(),
            data_connector: qualified_data_connector_name.clone(),
            collection: model_source.collection.clone(),
        })?;

    let source_collection_object_type = data_connector_context
        .schema
        .object_types
        .get(&source_collection.collection_type)
        .ok_or_else(|| {
            ndc_validation::NDCValidationError::NoSuchType(
                source_collection.collection_type.clone(),
            )
        })?;

    let source_collection_type_mapping_to_resolve = TypeMappingToResolve {
        type_name: &model.data_type,
        ndc_object_type_name: source_collection.collection_type.as_str(),
        ndc_object_type: source_collection_object_type,
    };

    // Get the mappings of arguments and any type mappings that need resolving from the arguments
    let (argument_mappings, argument_type_mappings_to_resolve) = get_argument_mappings(
        &model.arguments,
        &model_source.argument_mapping,
        &source_collection.arguments,
        &data_connector_context.schema.object_types,
        types,
    )
    .map_err(|err| Error::ModelCollectionArgumentMappingError {
        data_connector_name: qualified_data_connector_name.clone(),
        model_name: model.name.clone(),
        collection_name: model_source.collection.clone(),
        error: err,
    })?;

    // Resolve all the type mappings (types from the arguments and the result type)
    let namespaced_type_mappings = model_source
        .type_mapping
        .iter()
        .map(|(type_name, mapping)| {
            (
                Qualified::new(subgraph.to_string(), type_name.clone()),
                mapping,
            )
        })
        .collect();
    let mappings_to_resolve = iter::once(&source_collection_type_mapping_to_resolve)
        .chain(argument_type_mappings_to_resolve.iter());
    let type_mappings = resolve_type_mappings(
        mappings_to_resolve,
        &namespaced_type_mappings,
        types,
        &data_connector_context.schema.object_types,
    )
    .map_err(
        |type_validation_error| Error::ModelTypeMappingValidationError {
            model_name: model.name.clone(),
            error: type_validation_error,
        },
    )?;

    model.source = Some(ModelSource {
        data_connector: DataConnector::new(
            qualified_data_connector_name,
            data_connector_context.url.clone(),
            data_connector_context.headers,
        )?,
        collection: model_source.collection.clone(),
        type_mappings,
        argument_mappings,
    });

    ndc_validation::validate_ndc(&model.name, model, data_connector_context.schema)?;
    Ok(())
}

/// Gets the `ObjectTypeRepresentation` of the type identified with the
/// `data_type`, it will throw an error if the type is not found to be an object
/// or if the model has an unknown data type.
pub(crate) fn get_model_object_type_representation<'s>(
    types: &'s HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
    data_type: &Qualified<CustomTypeName>,
    model_name: &Qualified<ModelName>,
) -> Result<&'s ObjectTypeRepresentation, crate::metadata::resolved::error::Error> {
    let object_type_representation = match types.get(data_type) {
        Some(TypeRepresentation::Object(object_type_representation)) => {
            Ok(object_type_representation)
        }
        Some(type_rep) => Err(Error::InvalidTypeRepresentation {
            model_name: model_name.clone(),
            type_representation: type_rep.clone(),
        }),
        None => Err(Error::UnknownModelDataType {
            model_name: model_name.clone(),
            data_type: data_type.clone(),
        }),
    }?;
    Ok(object_type_representation)
}
