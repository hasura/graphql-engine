use std::collections::BTreeMap;
use std::ops::Deref;

use crate::{error, permissions};
use graphql_schema::OrderByRelationshipAnnotation;
use graphql_schema::GDS;
use graphql_schema::{Annotation, InputAnnotation, ModelInputAnnotation};
use hasura_authn_core::SessionVariables;
use indexmap::IndexMap;
use lang_graphql::normalized_ast::{self as normalized_ast, Value};
use metadata_resolve::{ObjectTypeWithRelationships, Qualified};
use open_dds::{data_connector::DataConnectorColumnName, types::CustomTypeName};
use plan::count_model;
use plan_types::{
    Expression, LocalModelRelationshipInfo, NdcRelationshipName, OrderByDirection, OrderByElement,
    OrderByTarget, RelationshipPathElement, UsagesCounts,
};
use serde::Serialize;

#[derive(Debug, Serialize, Clone, PartialEq)]
pub struct OrderBy<'s> {
    pub order_by_elements: Vec<OrderByElement<Expression<'s>>>,
    // relationships that were used in the order_by expression. This is helpful
    // for collecting relationships and sending collection_relationships
    pub relationships: BTreeMap<NdcRelationshipName, LocalModelRelationshipInfo<'s>>,
}

pub fn build_ndc_order_by<'s>(
    value: &Value<'s, GDS>,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
    type_mappings: &'s BTreeMap<
        metadata_resolve::Qualified<open_dds::types::CustomTypeName>,
        metadata_resolve::TypeMapping,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    data_connector_link: &'s metadata_resolve::DataConnectorLink,
) -> Result<OrderBy<'s>, error::Error> {
    match value {
        normalized_ast::Value::List(arguments) => {
            let mut order_by_elements = Vec::new();
            let mut relationships = BTreeMap::new();

            for argument in arguments {
                let order_by_element = build_ndc_order_by_element(
                    argument,
                    metadata_resolve::MultipleOrderByInputObjectFields::Disallow,
                    &[],
                    &[],
                    &mut relationships,
                    session_variables,
                    usage_counts,
                    object_types,
                    type_mappings,
                    data_connector_link,
                )?;
                order_by_elements.extend(order_by_element);
            }
            Ok(OrderBy {
                order_by_elements,
                relationships,
            })
        }
        _ => Err(error::InternalEngineError::InternalGeneric {
            description: "Expected list of input objects value for order_by".into(),
        })?,
    }
}

// Build the NDC OrderByElement by traversing the relationships when present
// For eg: If we have the following order_by query:
//      Track(order_by: {Album: {Artist: {ArtistId: Asc}, AlbumId: Asc}}, limit: 15)
// where, '{Album: {Artist: {ArtistId: Asc}, AlbumId: Asc}}' will be annotated as 'ModelOrderByRelationshipArgument'
// the `OrderByElement` will be:
//      [
//          ndc_models::OrderByElement {
//              order_direction: Asc,
//              target: ndc_models::OrderByTarget::Column {
//                  name: "ArtistId",
//                  path: ["TrackAlbum", "AlbumArtist"]
//              }
//          },
//          ndc_models::OrderByElement {
//              order_direction: Asc,
//              target: ndc_models::OrderByTarget::Column {
//                  name: "AlbumId",
//                  path: ["TrackAlbum"]
//              }
//          }
//      ]

pub fn build_ndc_order_by_element<'s>(
    input_field_value: &Value<'s, GDS>,
    multiple_input_fields: metadata_resolve::MultipleOrderByInputObjectFields,
    column_path: &[&'s DataConnectorColumnName],
    // The path to access the relationship column. If the column is a
    // non-relationship column, this will be empty. The paths contains
    // the names of relationships (in order) that needs to be traversed
    // to access the column.
    relationship_path: &[&RelationshipPathElement<Expression<'s>>],
    relationships: &mut BTreeMap<NdcRelationshipName, LocalModelRelationshipInfo<'s>>,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    type_mappings: &'s BTreeMap<
        metadata_resolve::Qualified<open_dds::types::CustomTypeName>,
        metadata_resolve::TypeMapping,
    >,
    data_connector_link: &'s metadata_resolve::DataConnectorLink,
) -> Result<Vec<OrderByElement<Expression<'s>>>, error::Error> {
    let input_object_fields = input_field_value.as_object()?;
    let mut order_by_elements = Vec::new();

    // Technically this function should only ever return a single order by element, but thanks to a bug
    // that we have to retain the behaviour of for backwards compatibility, we sometimes need to allow
    // returning multiple order by elements.
    // We don't actually want to support returning multiple order by elements based on a user having
    // multiple input object fields in a single order_by object, because the order of these fields is
    // undefined in GraphQL. So we need to force them to specify all ordering elements in a top level list.
    if multiple_input_fields == metadata_resolve::MultipleOrderByInputObjectFields::Disallow
        && input_object_fields.len() > 1
    {
        return Err(error::Error::OrderByObjectShouldExactlyHaveOneKeyValuePair);
    }

    for object_field in input_object_fields.values() {
        match object_field.info.generic {
            // The column that we want to use for ordering. If the column happens to be
            // a relationship column, we'll have to join all the paths to specify NDC,
            // what relationships needs to be traversed to access this column
            Annotation::Input(InputAnnotation::Model(
                graphql_schema::ModelInputAnnotation::ModelOrderByArgument {
                    parent_type,
                    field_name,
                    ..
                },
            )) => {
                let order_by_value = object_field.value.as_enum()?;
                let order_direction = match &order_by_value.info.generic {
                    Annotation::Input(InputAnnotation::Model(
                        ModelInputAnnotation::ModelOrderByDirection { direction },
                    )) => direction,
                    &annotation => {
                        return Err(error::InternalEngineError::UnexpectedAnnotation {
                            annotation: annotation.clone(),
                        })?
                    }
                };
                let field_mappings = type_mappings
                    .get(parent_type)
                    .map(
                        |metadata_resolve::TypeMapping::Object { field_mappings, .. }| {
                            field_mappings
                        },
                    )
                    .ok_or_else(|| error::Error::InternalTypeMappingNotFound {
                        type_name: parent_type.clone(),
                    })?;

                let ndc_column = field_mappings
                    .get(field_name)
                    .map(|field_mapping| field_mapping.column.clone())
                    .ok_or_else(|| error::Error::InternalMappingNotFound {
                        type_name: parent_type.clone(),
                        field_name: field_name.clone(),
                    })?;

                let order_element = OrderByElement {
                    order_direction: match order_direction {
                        graphql_schema::ModelOrderByDirection::Asc => OrderByDirection::Asc,
                        graphql_schema::ModelOrderByDirection::Desc => OrderByDirection::Desc,
                    },
                    target: OrderByTarget::Column {
                        relationship_path: relationship_path.iter().copied().cloned().collect(),
                        // The column name is the root column
                        name: column_path
                            .first()
                            .map_or(&ndc_column, Deref::deref)
                            .clone(),
                        // The field path is the nesting path inside the root column, if any
                        field_path: column_path
                            .iter()
                            .copied()
                            .chain([&ndc_column])
                            .skip(1)
                            .cloned()
                            .collect(),
                    },
                };

                order_by_elements.push(order_element);
            }
            // A relationship is being used to order the results. This relationship can
            // either point to another relationship or a column.
            Annotation::Input(InputAnnotation::Model(
                graphql_schema::ModelInputAnnotation::ModelOrderByRelationshipArgument(
                    OrderByRelationshipAnnotation {
                        relationship_name,
                        relationship_type,
                        source_type,
                        object_type_name: _,
                        target_source,
                        target_type,
                        target_model_name,
                        mappings,
                        deprecated: _,
                        multiple_input_properties,
                    },
                ),
            )) => {
                let relationship_field_nestedness = if column_path.is_empty() {
                    metadata_resolve::OrderableFieldNestedness::NotNested
                } else {
                    metadata_resolve::OrderableFieldNestedness::ObjectNested
                };

                metadata_resolve::validate_orderable_relationship(
                    source_type,
                    relationship_name,
                    relationship_field_nestedness,
                    data_connector_link,
                    &target_source.model.data_connector.name,
                )
                .map_err(crate::InternalEngineError::OrderableRelationshipError)?;

                let ndc_relationship_name =
                    NdcRelationshipName::new(source_type, relationship_name);
                relationships.insert(
                    ndc_relationship_name.clone(),
                    LocalModelRelationshipInfo {
                        relationship_name,
                        relationship_type,
                        source_type,
                        source_data_connector: data_connector_link,
                        source_type_mappings: type_mappings,
                        target_model_name,
                        target_source: &target_source.model,
                        target_type,
                        mappings,
                    },
                );

                // Add the target model being used in the usage counts
                count_model(target_model_name, usage_counts);

                let filter_permission =
                    permissions::get_select_filter_predicate(&object_field.info)?;
                let filter_predicate = permissions::build_model_permissions_filter_predicate(
                    &target_source.model.data_connector,
                    &target_source.model.type_mappings,
                    filter_permission,
                    session_variables,
                    object_types,
                    usage_counts,
                )?;

                // Add the current relationship to the relationship path.
                let relationship_path_element = RelationshipPathElement {
                    field_path: column_path.iter().copied().cloned().collect(),
                    relationship_name: ndc_relationship_name,
                    filter_predicate,
                };
                let new_relationship_path = relationship_path
                    .iter()
                    .copied()
                    .chain([&relationship_path_element])
                    .collect::<Vec<_>>();

                let new_order_by_elements = build_ndc_order_by_element(
                    &object_field.value,
                    *multiple_input_properties,
                    &[], // Field path resets as we pass through a relationship
                    &new_relationship_path,
                    relationships,
                    session_variables,
                    usage_counts,
                    object_types,
                    &target_source.model.type_mappings,
                    data_connector_link,
                )?;
                order_by_elements.extend(new_order_by_elements);
            }
            Annotation::Input(InputAnnotation::Model(
                graphql_schema::ModelInputAnnotation::ModelOrderByNestedExpression {
                    parent_type,
                    field_name,
                    multiple_input_properties,
                },
            )) => {
                let field_mappings = type_mappings
                    .get(parent_type)
                    .map(
                        |metadata_resolve::TypeMapping::Object { field_mappings, .. }| {
                            field_mappings
                        },
                    )
                    .ok_or_else(|| error::Error::InternalTypeMappingNotFound {
                        type_name: parent_type.clone(),
                    })?;

                let ndc_column = field_mappings
                    .get(field_name)
                    .map(|field_mapping| &field_mapping.column)
                    .ok_or_else(|| error::Error::InternalMappingNotFound {
                        type_name: parent_type.clone(),
                        field_name: field_name.clone(),
                    })?;
                let new_column_path = column_path
                    .iter()
                    .copied()
                    .chain([ndc_column])
                    .collect::<Vec<_>>();

                match multiple_input_properties {
                    metadata_resolve::MultipleOrderByInputObjectFields::Disallow => {
                        let order_by_element = build_ndc_order_by_element(
                            &object_field.value,
                            *multiple_input_properties,
                            &new_column_path,
                            relationship_path,
                            relationships,
                            session_variables,
                            usage_counts,
                            object_types,
                            type_mappings,
                            data_connector_link,
                        )?;
                        order_by_elements.extend(order_by_element);
                    }
                    metadata_resolve::MultipleOrderByInputObjectFields::Allow => {
                        // When multiple input properties are allowed for backwards compatibility purposes
                        // we expect nested properties to be (incorrectly!) typed as a list in graphql
                        let argument_list = object_field.value.as_list()?;
                        for argument in argument_list {
                            let order_by_element = build_ndc_order_by_element(
                                argument,
                                *multiple_input_properties,
                                &new_column_path,
                                relationship_path,
                                relationships,
                                session_variables,
                                usage_counts,
                                object_types,
                                type_mappings,
                                data_connector_link,
                            )?;
                            order_by_elements.extend(order_by_element);
                        }
                    }
                }
            }
            annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                annotation: annotation.clone(),
            })?,
        }
    }

    Ok(order_by_elements)
}

pub fn build_order_by_open_dd_ir<'s>(
    args_input: &Value<'s, GDS>,
    usage_counts: &mut UsagesCounts,
    data_connector_link: &'s metadata_resolve::DataConnectorLink,
) -> Result<Vec<open_dds::query::OrderByElement>, error::Error> {
    match args_input {
        normalized_ast::Value::List(arguments) => {
            let mut order_by_elements = Vec::new();
            for argument in arguments {
                let order_by_element = build_order_by_element_open_dd_ir(
                    argument,
                    metadata_resolve::OrderableFieldNestedness::NotNested,
                    metadata_resolve::MultipleOrderByInputObjectFields::Disallow,
                    usage_counts,
                    data_connector_link,
                )?;
                order_by_elements.extend(order_by_element);
            }
            Ok(order_by_elements)
        }
        _ => Err(error::InternalEngineError::InternalGeneric {
            description: "Expected list of input objects value for order_by".into(),
        })?,
    }
}

// Build the OpenDD OrderByElement by traversing the relationships when present
// For eg: If we have the following order_by query:
//      Track(order_by: {Album: {Artist: {ArtistId: Asc}, AlbumId: Asc}}, limit: 15)
// where, '{Album: {Artist: {ArtistId: Asc}, AlbumId: Asc}}' will be annotated as 'ModelOrderByRelationshipArgument'
// the `OrderByElement` will be:
//      [
//          open_dds::query::OrderByElement {
//              direction: Asc,
//              operand: Relationship(
//                  RelationshipOperand {
//                      target: RelationshipTarget {
//                          relationship_name: "Album",
//                          arguments: {},
//                          filter: None,
//                          order_by: [],
//                          limit: None,
//                          offset: None,
//                      },
//                      nested: Some(Relationship(
//                          RelationshipOperand {
//                              target: RelationshipTarget {
//                                  relationship_name: "Artist",
//                                  arguments: {},
//                                  filter: None,
//                                  order_by: [],
//                                  limit: None,
//                                  offset: None,
//                              },
//                              nested: Some(Field(
//                                  ObjectFieldOperand {
//                                      target: ObjectFieldTarget {
//                                          field_name: "ArtistId",
//                                          arguments: {},
//                                      },
//                                      nested: None,
//                                  }
//                              )),
//                          }
//                      )),
//                  }
//              ),
//          },
//          open_dds::query::OrderByElement {
//              direction: Asc,
//              operand: Relationship(
//                  RelationshipOperand {
//                      target: RelationshipTarget {
//                          relationship_name: "Album",
//                          arguments: {},
//                          filter: None,
//                          order_by: [],
//                          limit: None,
//                          offset: None,
//                      },
//                      nested: Some(Field(
//                          ObjectFieldOperand {
//                              target: ObjectFieldTarget {
//                                  field_name: "AlbumId",
//                                  arguments: {},
//                              },
//                              nested: None,
//                          }
//                      )),
//                  }
//              ),
//          },
//      ]

pub fn build_order_by_element_open_dd_ir<'s>(
    input_field_value: &Value<'s, GDS>,
    field_nestedness: metadata_resolve::OrderableFieldNestedness,
    multiple_input_fields: metadata_resolve::MultipleOrderByInputObjectFields,
    usage_counts: &mut UsagesCounts,
    data_connector_link: &'s metadata_resolve::DataConnectorLink,
) -> Result<Vec<open_dds::query::OrderByElement>, error::Error> {
    let input_object_fields = input_field_value.as_object()?;
    let mut order_by_elements = Vec::new();

    // Technically this function should only ever return a single order by element, but thanks to a bug
    // that we have to retain the behaviour of for backwards compatibility, we sometimes need to allow
    // returning multiple order by elements.
    // We don't actually want to support returning multiple order by elements based on a user having
    // multiple input object fields in a single order_by object, because the order of these fields is
    // undefined in GraphQL. So we need to force them to specify all ordering elements in a top level list.
    if multiple_input_fields == metadata_resolve::MultipleOrderByInputObjectFields::Disallow
        && input_object_fields.len() > 1
    {
        return Err(error::Error::OrderByObjectShouldExactlyHaveOneKeyValuePair);
    }

    for object_field in input_object_fields.values() {
        match object_field.info.generic {
            // The column that we want to use for ordering. If the column happens to be
            // a relationship column, we'll have to join all the paths to specify NDC,
            // what relationships needs to be traversed to access this column
            Annotation::Input(InputAnnotation::Model(
                graphql_schema::ModelInputAnnotation::ModelOrderByArgument {
                    parent_type: _,
                    field_name,
                    ..
                },
            )) => {
                let order_by_value = object_field.value.as_enum()?;
                let order_direction = match &order_by_value.info.generic {
                    Annotation::Input(InputAnnotation::Model(
                        ModelInputAnnotation::ModelOrderByDirection { direction },
                    )) => direction,
                    &annotation => {
                        return Err(error::InternalEngineError::UnexpectedAnnotation {
                            annotation: annotation.clone(),
                        })?
                    }
                };
                let operand =
                    open_dds::query::Operand::Field(open_dds::query::ObjectFieldOperand {
                        target: Box::new(open_dds::query::ObjectFieldTarget {
                            field_name: field_name.clone(),
                            arguments: IndexMap::new(),
                        }),
                        nested: None,
                    });
                let direction = match order_direction {
                    graphql_schema::ModelOrderByDirection::Asc => {
                        open_dds::models::OrderByDirection::Asc
                    }
                    graphql_schema::ModelOrderByDirection::Desc => {
                        open_dds::models::OrderByDirection::Desc
                    }
                };

                let order_element = open_dds::query::OrderByElement { direction, operand };

                order_by_elements.push(order_element);
            }
            // A relationship is being used to order the results. This relationship can
            // either point to another relationship or a column.
            Annotation::Input(InputAnnotation::Model(
                graphql_schema::ModelInputAnnotation::ModelOrderByRelationshipArgument(
                    OrderByRelationshipAnnotation {
                        relationship_name,
                        relationship_type: _,
                        source_type,
                        object_type_name: _,
                        target_source,
                        target_type: _,
                        target_model_name,
                        mappings: _,
                        deprecated: _,
                        multiple_input_properties,
                    },
                ),
            )) => {
                metadata_resolve::validate_orderable_relationship(
                    source_type,
                    relationship_name,
                    field_nestedness,
                    data_connector_link,
                    &target_source.model.data_connector.name,
                )
                .map_err(crate::InternalEngineError::OrderableRelationshipError)?;

                // Add the target model being used in the usage counts
                count_model(target_model_name, usage_counts);

                let relationship_target = open_dds::query::RelationshipTarget {
                    relationship_name: relationship_name.clone(),
                    arguments: IndexMap::new(),
                    // Following parameters are not applicable in the order_by input.
                    filter: None, // NOTE: Permission filters are handled during OpenDd query planning.
                    order_by: vec![],
                    limit: None,
                    offset: None,
                };

                let new_order_by_elements = build_order_by_element_open_dd_ir(
                    &object_field.value,
                    metadata_resolve::OrderableFieldNestedness::NotNested, // Field nestedness resets as we pass through a relationship
                    *multiple_input_properties,
                    usage_counts,
                    data_connector_link,
                )?;

                for element in new_order_by_elements {
                    let operand = open_dds::query::Operand::Relationship(
                        open_dds::query::RelationshipOperand {
                            target: Box::new(relationship_target.clone()),
                            nested: Some(Box::new(element.operand)),
                        },
                    );
                    order_by_elements.push(open_dds::query::OrderByElement {
                        direction: element.direction,
                        operand,
                    });
                }
            }
            Annotation::Input(InputAnnotation::Model(
                graphql_schema::ModelInputAnnotation::ModelOrderByNestedExpression {
                    parent_type: _,
                    field_name,
                    multiple_input_properties,
                },
            )) => {
                match multiple_input_properties {
                    metadata_resolve::MultipleOrderByInputObjectFields::Disallow => {
                        let new_order_by_elements = build_order_by_element_open_dd_ir(
                            &object_field.value,
                            metadata_resolve::OrderableFieldNestedness::ObjectNested,
                            *multiple_input_properties,
                            usage_counts,
                            data_connector_link,
                        )?;

                        for element in new_order_by_elements {
                            let operand = open_dds::query::Operand::Field(
                                open_dds::query::ObjectFieldOperand {
                                    target: Box::new(open_dds::query::ObjectFieldTarget {
                                        field_name: field_name.clone(),
                                        arguments: IndexMap::new(),
                                    }),
                                    nested: Some(Box::new(element.operand)),
                                },
                            );
                            order_by_elements.push(open_dds::query::OrderByElement {
                                direction: element.direction,
                                operand,
                            });
                        }
                    }
                    metadata_resolve::MultipleOrderByInputObjectFields::Allow => {
                        // When multiple input properties are allowed for backwards compatibility purposes
                        // we expect nested properties to be (incorrectly!) typed as a list in graphql
                        let argument_list = object_field.value.as_list()?;
                        for argument in argument_list {
                            let new_order_by_elements = build_order_by_element_open_dd_ir(
                                argument,
                                metadata_resolve::OrderableFieldNestedness::ObjectNested,
                                *multiple_input_properties,
                                usage_counts,
                                data_connector_link,
                            )?;
                            for element in new_order_by_elements {
                                let operand = open_dds::query::Operand::Field(
                                    open_dds::query::ObjectFieldOperand {
                                        target: Box::new(open_dds::query::ObjectFieldTarget {
                                            field_name: field_name.clone(),
                                            arguments: IndexMap::new(),
                                        }),
                                        nested: Some(Box::new(element.operand)),
                                    },
                                );
                                order_by_elements.push(open_dds::query::OrderByElement {
                                    direction: element.direction,
                                    operand,
                                });
                            }
                        }
                    }
                }
            }
            annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
                annotation: annotation.clone(),
            })?,
        }
    }
    Ok(order_by_elements)
}
