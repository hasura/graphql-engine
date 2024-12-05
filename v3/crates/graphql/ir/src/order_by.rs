use std::collections::BTreeMap;
use std::ops::Deref;

use crate::model_tracking::count_model;
use graphql_schema::OrderByRelationshipAnnotation;
use graphql_schema::{Annotation, InputAnnotation, ModelInputAnnotation};
use hasura_authn_core::SessionVariables;
use lang_graphql::normalized_ast::{self as normalized_ast, InputField, Value};
use open_dds::data_connector::DataConnectorColumnName;
use plan_types::{
    Expression, LocalModelRelationshipInfo, NdcRelationshipName, OrderByDirection, OrderByElement,
    OrderByTarget, RelationshipPathElement, UsagesCounts,
};
use serde::Serialize;

use crate::{error, permissions};
use graphql_schema::GDS;

#[derive(Debug, Serialize, Clone, PartialEq)]
pub struct OrderBy<'s> {
    pub order_by_elements: Vec<OrderByElement<Expression<'s>>>,
    // relationships that were used in the order_by expression. This is helpful
    // for collecting relationships and sending collection_relationships
    pub relationships: BTreeMap<NdcRelationshipName, LocalModelRelationshipInfo<'s>>,
}

pub fn build_ndc_order_by<'s>(
    args_field: &InputField<'s, GDS>,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<OrderBy<'s>, error::Error> {
    match &args_field.value {
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
                graphql_schema::ModelInputAnnotation::ModelOrderByArgument { ndc_column, .. },
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

                let order_element = OrderByElement {
                    order_direction: match order_direction {
                        graphql_schema::ModelOrderByDirection::Asc => OrderByDirection::Asc,
                        graphql_schema::ModelOrderByDirection::Desc => OrderByDirection::Desc,
                    },
                    target: OrderByTarget::Column {
                        relationship_path: relationship_path.iter().copied().cloned().collect(),
                        // The column name is the root column
                        name: column_path.first().map_or(ndc_column, Deref::deref).clone(),
                        // The field path is the nesting path inside the root column, if any
                        field_path: column_path
                            .iter()
                            .copied()
                            .chain([ndc_column])
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
                        source_data_connector,
                        source_type_mappings,
                        target_source,
                        target_type,
                        target_model_name,
                        mappings,
                        deprecated: _,
                        multiple_input_properties,
                    },
                ),
            )) => {
                let ndc_relationship_name =
                    NdcRelationshipName::new(source_type, relationship_name);

                relationships.insert(
                    ndc_relationship_name.clone(),
                    LocalModelRelationshipInfo {
                        relationship_name,
                        relationship_type,
                        source_type,
                        source_data_connector,
                        source_type_mappings,
                        target_source,
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
                )?;
                order_by_elements.extend(new_order_by_elements);
            }
            Annotation::Input(InputAnnotation::Model(
                graphql_schema::ModelInputAnnotation::ModelOrderByNestedExpression {
                    ndc_column,
                    multiple_input_properties,
                },
            )) => {
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
