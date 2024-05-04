use crate::helpers::types::mk_name;
/// this is where we will resolve graphql configuration
use crate::types::error::{Error, GraphqlConfigError};
use lang_graphql::ast::common as ast;
use open_dds::graphql_config::{self, OrderByDirection};
use serde::{Deserialize, Serialize};

lazy_static::lazy_static! {
    static ref FALLBACK_GRAPHQL_CONFIG: graphql_config::GraphqlConfig = graphql_config::GraphqlConfig::V1(graphql_config::GraphqlConfigV1 {
        query: graphql_config::QueryGraphqlConfig {
            root_operation_type_name: "Query".to_string(),
            arguments_input: Some (graphql_config::ArgumentsInputGraphqlConfig {
                field_name: "args".to_string(),
            }),
            limit_input: Some(graphql_config::LimitInputGraphqlConfig {
                field_name: "limit".to_string(),
            }),
            offset_input: Some(graphql_config::OffsetInputGraphqlConfig {
                field_name: "offset".to_string(),
            }),
            filter_input: Some(graphql_config::FilterInputGraphqlConfig {
                field_name: "where".to_string(),
                operator_names: graphql_config::FilterInputOperatorNames {
                    and: "_and".to_string(),
                    or: "_or".to_string(),
                    not: "_not".to_string(),
                    is_null: "_is_null".to_string(),
                },
            }),
            order_by_input: Some(graphql_config::OrderByInputGraphqlConfig {
                field_name:"order_by".to_string(),
                enum_direction_values: graphql_config::OrderByDirectionValues {
                    asc: "Asc".to_string(),
                    desc: "Desc".to_string(),
                },
                 enum_type_names: vec![graphql_config::OrderByEnumTypeName{
                    type_name: "order_by".to_string(),
                    directions: vec![graphql_config::OrderByDirection::Asc, graphql_config::OrderByDirection::Desc],
                 }] })
        },
        mutation: graphql_config::MutationGraphqlConfig{
            root_operation_type_name: "Mutation".to_string(),
        },
        apollo_federation: None,
    });
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct GraphqlConfig {
    // The graphql configuration that needs to be applied to each model, depending on it's conditions
    pub query: QueryGraphqlConfig,
    // The grapqhl configuration that is global across the schema
    pub global: GlobalGraphqlConfig,
}

/// Resolve and validate the GraphQL configuration.
/// For example, make sure all names are valid GraphQL names.
///
/// `GraphqlConfig` is an optional metadata object introduced in V2 metadata
/// that is associated with the flag `require_graphql_config`. This is done
/// to ensure that we still accept older metadata which did not have this
/// object present.
///
/// The logic to generate a new GraphqlConfig object is as follows:
/// If `graphql_config` metadata object is present use that object
/// If it is not present,
///     * check if the `require_graphql_config` flag is set (which means
///       that that object is mandatory) throw an error
///     * if the flag is not set, use the fallback object
pub fn resolve(
    graphql_configs: &Vec<graphql_config::GraphqlConfig>,
    flags: &open_dds::flags::Flags,
) -> Result<GraphqlConfig, Error> {
    if graphql_configs.is_empty() {
        if flags.require_graphql_config {
            return Err(Error::GraphqlConfigError {
                graphql_config_error: GraphqlConfigError::MissingGraphqlConfig,
            });
        }
        let graphql_config = resolve_graphql_config(&FALLBACK_GRAPHQL_CONFIG)?;
        Ok(graphql_config)
    } else {
        match graphql_configs.as_slice() {
            // There should only be one graphql config in supergraph
            [graphql_config] => resolve_graphql_config(graphql_config),
            _ => Err(Error::GraphqlConfigError {
                graphql_config_error: GraphqlConfigError::MultipleGraphqlConfigDefinition,
            }),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct GlobalGraphqlConfig {
    pub query_root_type_name: ast::TypeName,
    pub mutation_root_type_name: ast::TypeName,
    pub order_by_input: Option<OrderByInputGraphqlConfig>,
    pub enable_apollo_federation_fields: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct OrderByInputGraphqlConfig {
    pub asc_direction_field_value: ast::Name,
    pub desc_direction_field_value: ast::Name,
    pub enum_type_name: ast::TypeName,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct QueryGraphqlConfig {
    pub arguments_field_name: Option<ast::Name>,
    pub limit_field_name: Option<ast::Name>,
    pub offset_field_name: Option<ast::Name>,
    pub filter_input_config: Option<FilterInputGraphqlConfig>,
    pub order_by_field_name: Option<ast::Name>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]

pub struct FilterInputGraphqlConfig {
    pub where_field_name: ast::Name,
    pub operator_names: FilterInputOperatorNames,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct FilterInputOperatorNames {
    pub and: ast::Name,
    pub or: ast::Name,
    pub not: ast::Name,
    pub is_null: ast::Name,
}

/// Resolve and validate the GraphQL configuration.
/// For example, make sure all names are valid GraphQL names.
pub fn resolve_graphql_config(
    graphql_config: &open_dds::graphql_config::GraphqlConfig,
) -> Result<GraphqlConfig, Error> {
    match graphql_config {
        open_dds::graphql_config::GraphqlConfig::V1(graphql_config_metadata) => {
            let arguments_field_name = graphql_config_metadata
                .query
                .arguments_input
                .as_ref()
                .map(|arguments_input| mk_name(arguments_input.field_name.as_str()))
                .transpose()?;

            let limit_field_name = graphql_config_metadata
                .query
                .limit_input
                .as_ref()
                .map(|limit_input| mk_name(limit_input.field_name.as_str()))
                .transpose()?;

            let offset_field_name = graphql_config_metadata
                .query
                .offset_input
                .as_ref()
                .map(|offset_input| mk_name(offset_input.field_name.as_str()))
                .transpose()?;

            let filter_input_config = match &graphql_config_metadata.query.filter_input {
                Some(filter_input) => Some(FilterInputGraphqlConfig {
                    where_field_name: mk_name(filter_input.field_name.as_str())?,
                    operator_names: FilterInputOperatorNames {
                        and: mk_name(filter_input.operator_names.and.as_str())?,
                        or: mk_name(filter_input.operator_names.or.as_str())?,
                        not: mk_name(filter_input.operator_names.not.as_str())?,
                        is_null: mk_name(filter_input.operator_names.is_null.as_str())?,
                    },
                }),
                None => None,
            };

            let order_by_field_name = graphql_config_metadata
                .query
                .order_by_input
                .as_ref()
                .map(|order_by_input| mk_name(order_by_input.field_name.as_str()))
                .transpose()?;

            let query_root_type_name = ast::TypeName(mk_name(
                graphql_config_metadata
                    .query
                    .root_operation_type_name
                    .as_str(),
            )?);
            let mutation_root_type_name = ast::TypeName(mk_name(
                graphql_config_metadata
                    .mutation
                    .root_operation_type_name
                    .as_str(),
            )?);

            let order_by_input = match &graphql_config_metadata.query.order_by_input {
                None => None,
                Some(order_by_input) => {
                    let order_by_enum_type_name = match order_by_input.enum_type_names.as_slice() {
                        [] => Err(Error::GraphqlConfigError {
                            graphql_config_error:
                                GraphqlConfigError::MissingOrderByEnumTypeNamesInGraphqlConfig,
                        }),
                        [order_by_enum_type] => Ok({
                            // TODO: Naveen: Currently we do not allow enabling a specific direction
                            // for orderableField. In future when we support this, we would like to
                            // build different enum types for different variations of directions.
                            if order_by_enum_type.directions
                                != vec![OrderByDirection::Asc, OrderByDirection::Desc]
                            {
                                let invalid_directions = order_by_enum_type
                                    .directions
                                    .iter()
                                    .map(std::string::ToString::to_string)
                                    .collect::<Vec<_>>()
                                    .join(",");
                                Err(Error::GraphqlConfigError {
                                    graphql_config_error:
                                        GraphqlConfigError::InvalidOrderByDirection {
                                            directions: invalid_directions,
                                        },
                                })
                            } else {
                                mk_name(&order_by_enum_type.type_name)
                            }
                        }),
                        _ => Err(Error::GraphqlConfigError {
                            graphql_config_error:
                                GraphqlConfigError::MultipleOrderByEnumTypeNamesInGraphqlConfig,
                        }),
                    }?;

                    Some(OrderByInputGraphqlConfig {
                        asc_direction_field_value: mk_name(
                            &order_by_input.enum_direction_values.asc,
                        )?,
                        desc_direction_field_value: mk_name(
                            &order_by_input.enum_direction_values.desc,
                        )?,
                        enum_type_name: ast::TypeName(order_by_enum_type_name?),
                    })
                }
            };

            let enable_apollo_federation_fields = graphql_config_metadata
                .apollo_federation
                .as_ref()
                .map(|federation_config| federation_config.enable_root_fields)
                .unwrap_or_default();

            Ok(GraphqlConfig {
                query: QueryGraphqlConfig {
                    arguments_field_name,
                    limit_field_name,
                    offset_field_name,
                    filter_input_config,
                    order_by_field_name,
                },
                global: GlobalGraphqlConfig {
                    query_root_type_name,
                    mutation_root_type_name,
                    order_by_input,
                    enable_apollo_federation_fields,
                },
            })
        }
    }
}
