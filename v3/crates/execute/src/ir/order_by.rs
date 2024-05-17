use std::collections::BTreeMap;

use lang_graphql::normalized_ast::{self as normalized_ast, InputField};
use ndc_models;
use serde::Serialize;

use crate::model_tracking::{count_model, UsagesCounts};
use schema::OrderByRelationshipAnnotation;
use schema::{Annotation, InputAnnotation, ModelInputAnnotation};

use super::relationship::LocalModelRelationshipInfo;
use super::selection_set::NDCRelationshipName;

use crate::ir::error;
use schema;
use schema::GDS;

#[derive(Debug, Serialize)]
pub(crate) struct ResolvedOrderBy<'s> {
    pub(crate) order_by: ndc_models::OrderBy,
    // relationships that were used in the order_by expression. This is helpful
    // for collecting relatinships and sending collection_relationships
    pub(crate) relationships: BTreeMap<NDCRelationshipName, LocalModelRelationshipInfo<'s>>,
}

pub(crate) fn build_ndc_order_by<'s>(
    args_field: &InputField<'s, GDS>,
    usage_counts: &mut UsagesCounts,
) -> Result<ResolvedOrderBy<'s>, error::Error> {
    match &args_field.value {
        normalized_ast::Value::List(arguments) => {
            let mut ndc_order_elements = Vec::new();
            let mut relationships = BTreeMap::new();

            for v in arguments.iter() {
                match v {
                    normalized_ast::Value::Object(arguments) => {
                        // Check if the input object contains exactly one key-value pair.
                        // This is done because the users might provide multiple key-value pairs
                        // in a single input object and the server might interpret it arbitrarily
                        // since input objects values are unordered key-value pair lists.
                        if arguments.len() != 1 {
                            Err(error::Error::OrderByObjectShouldExactlyHaveOneKeyValuePair)?
                        } else {
                            let argument = arguments
                                    .first()
                                    .ok_or(error::InternalEngineError::InternalGeneric {
                                        description: "unexpected: could not find the first key-value pair of arguments"
                                            .into(),
                                    })?
                                    .1;
                            let relationship_paths = Vec::new();
                            let order_by_element = build_ndc_order_by_element(
                                argument,
                                relationship_paths,
                                &mut relationships,
                                usage_counts,
                            )?;
                            ndc_order_elements.extend(order_by_element);
                        }
                    }
                    _ => Err(error::InternalEngineError::InternalGeneric {
                        description: "Expected list of input objects value for order_by".into(),
                    })?,
                }
            }
            Ok(ResolvedOrderBy {
                order_by: ndc_models::OrderBy {
                    elements: ndc_order_elements,
                },
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

pub(crate) fn build_ndc_order_by_element<'s>(
    argument: &InputField<'s, GDS>,
    // The path to access the relationship column. If the column is a
    // non-relationship column, this will be empty. The paths contains
    // the names of relationships (in order) that needs to be traversed
    // to access the column.
    mut relationship_paths: Vec<NDCRelationshipName>,
    relationships: &mut BTreeMap<NDCRelationshipName, LocalModelRelationshipInfo<'s>>,
    usage_counts: &mut UsagesCounts,
) -> Result<Vec<ndc_models::OrderByElement>, error::Error> {
    match argument.info.generic {
        // The column that we want to use for ordering. If the column happens to be
        // a relationship column, we'll have to join all the paths to specify NDC,
        // what relationships needs to be traversed to access this column
        Annotation::Input(InputAnnotation::Model(
            schema::ModelInputAnnotation::ModelOrderByArgument { ndc_column },
        )) => {
            let order_by_value = argument.value.as_enum()?;
            let order_direction = match &order_by_value.info.generic {
                Annotation::Input(InputAnnotation::Model(
                    ModelInputAnnotation::ModelOrderByDirection { direction },
                )) => match &direction {
                    schema::ModelOrderByDirection::Asc => ndc_models::OrderDirection::Asc,
                    schema::ModelOrderByDirection::Desc => ndc_models::OrderDirection::Desc,
                },
                &annotation => {
                    return Err(error::InternalEngineError::UnexpectedAnnotation {
                        annotation: annotation.clone(),
                    })?
                }
            };

            let mut order_by_element_path = Vec::new();
            // When using a nested relationship column, you'll have to provide all the relationships(paths)
            // NDC has to traverse to access the column. The ordering of that paths is important.
            // The order decides how to access the column.
            //
            // For example, if you have a model called `User` with a relationship column called `Posts`
            // which has a relationship column called `Comments` which has a non-relationship column
            // called `text`, you'll have to provide the following paths to access the `text` column:
            // ["UserPosts", "PostsComments"]
            for path in relationship_paths.iter() {
                order_by_element_path.push(ndc_models::PathElement {
                    relationship: path.0.clone(),
                    arguments: BTreeMap::new(),
                    // 'AND' predicate indicates that the column can be accessed
                    // by joining all the relationships paths provided
                    predicate: Some(Box::new(ndc_models::Expression::And {
                        // TODO(naveen): Add expressions here, when we support sorting with predicates.
                        //
                        // There are two types of sorting:
                        //     1. Sorting without predicates
                        //     2. Sorting with predicates
                        //
                        // In the 1st sort, we sort all the elements of the results either in ascending
                        // or descing order based on the order_by argument.
                        //
                        // In the 2nd sort, we want fetch the entire result but only sort a subset
                        // of result and put those sorted set either at the beginning or at the end of the
                        // result.
                        //
                        // Currently we only support the 1st type of sort. Hence we don't have any expressions/predicate.
                        expressions: Vec::new(),
                    })),
                })
            }

            let order_element = ndc_models::OrderByElement {
                order_direction,
                // TODO(naveen): When aggregates are supported, extend this to support other ndc_models::OrderByTarget
                target: ndc_models::OrderByTarget::Column {
                    name: ndc_column.clone(),
                    path: order_by_element_path,
                },
            };

            Ok(vec![order_element])
        }
        // A relationship is being used to order the results. This relationship can
        // either point to another relationship or a column.
        Annotation::Input(InputAnnotation::Model(
            schema::ModelInputAnnotation::ModelOrderByRelationshipArgument(
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
                },
            ),
        )) => {
            let ndc_relationship_name = NDCRelationshipName::new(source_type, relationship_name)?;

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

            // Add the current relationship to the relationship paths.
            relationship_paths.push(ndc_relationship_name);

            for argument in argument_value_map.values() {
                let order_by_element = build_ndc_order_by_element(
                    argument,
                    relationship_paths.clone(),
                    relationships,
                    usage_counts,
                )?;
                order_by_elements.extend(order_by_element);
            }
            Ok(order_by_elements)
        }
        annotation => Err(error::InternalEngineError::UnexpectedAnnotation {
            annotation: annotation.clone(),
        })?,
    }
}
