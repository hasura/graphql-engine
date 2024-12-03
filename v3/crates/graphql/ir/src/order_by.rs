use std::collections::BTreeMap;
use std::ops::Deref;

use crate::model_tracking::count_model;
use graphql_schema::OrderByRelationshipAnnotation;
use graphql_schema::{Annotation, InputAnnotation, ModelInputAnnotation};
use hasura_authn_core::SessionVariables;
use lang_graphql::normalized_ast::{self as normalized_ast, InputField};
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

            for v in arguments {
                match v {
                    normalized_ast::Value::Object(arguments) => {
                        // Check if the input object contains exactly one key-value pair.
                        // This is done because the users might provide multiple key-value pairs
                        // in a single input object and the server might interpret it arbitrarily
                        // since input objects values are unordered key-value pair lists.
                        if arguments.len() == 1 {
                            let argument = arguments
                                    .first()
                                    .ok_or_else(|| error::InternalEngineError::InternalGeneric {
                                        description: "unexpected: could not find the first key-value pair of arguments"
                                            .into(),
                                    })?
                                    .1;
                            let order_by_element = build_ndc_order_by_element(
                                argument,
                                &[],
                                &[],
                                &mut relationships,
                                session_variables,
                                usage_counts,
                            )?;
                            order_by_elements.extend(order_by_element);
                        } else {
                            Err(error::Error::OrderByObjectShouldExactlyHaveOneKeyValuePair)?;
                        }
                    }
                    _ => Err(error::InternalEngineError::InternalGeneric {
                        description: "Expected list of input objects value for order_by".into(),
                    })?,
                }
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
    argument: &InputField<'s, GDS>,
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
    match argument.info.generic {
        // The column that we want to use for ordering. If the column happens to be
        // a relationship column, we'll have to join all the paths to specify NDC,
        // what relationships needs to be traversed to access this column
        Annotation::Input(InputAnnotation::Model(
            graphql_schema::ModelInputAnnotation::ModelOrderByArgument { ndc_column, .. },
        )) => {
            let order_by_value = argument.value.as_enum()?;
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

            Ok(vec![order_element])
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
                },
            ),
        )) => {
            let ndc_relationship_name = NdcRelationshipName::new(source_type, relationship_name);

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

            // This map contains the relationships or the columns of the relationship that needs to be used for ordering.
            let argument_value_map = argument.value.as_object()?;
            let mut order_by_elements = Vec::new();

            let filter_permission = permissions::get_select_filter_predicate(&argument.info)?;
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

            for argument in argument_value_map.values() {
                let order_by_element = build_ndc_order_by_element(
                    argument,
                    &[], // Field path resets as we pass through a relationship
                    &new_relationship_path,
                    relationships,
                    session_variables,
                    usage_counts,
                )?;
                order_by_elements.extend(order_by_element);
            }
            Ok(order_by_elements)
        }
        Annotation::Input(InputAnnotation::Model(
            graphql_schema::ModelInputAnnotation::ModelOrderByNestedExpression { ndc_column },
        )) => {
            let new_column_path = column_path
                .iter()
                .copied()
                .chain([ndc_column])
                .collect::<Vec<_>>();

            let argument_list = argument.value.as_list()?;
            let mut order_by_elements = Vec::new();
            for argument in argument_list {
                let argument_value_map = argument.as_object()?;
                for argument in argument_value_map.values() {
                    let order_by_element = build_ndc_order_by_element(
                        argument,
                        &new_column_path,
                        relationship_path,
                        relationships,
                        session_variables,
                        usage_counts,
                    )?;
                    order_by_elements.extend(order_by_element);
                }
            }
            Ok(order_by_elements)
        }
        annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
            annotation: annotation.clone(),
        })?,
    }
}
