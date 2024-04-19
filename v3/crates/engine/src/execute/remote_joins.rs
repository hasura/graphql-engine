//! Implements execution of Remote Joins
//!
//! # Introduction
//! "Remote Join" is a feature where engine can fetch data from different data
//! connectors, and based on some field mapping, can
//! [join](https://cghlewis.com/blog/joins/) the dataset from the two different
//! data sources.
//!
//! Let's consider the following simple example. Consider there are two data
//! connectors -
//!   1. Geographical information about cities in `connector A`
//!
//! `cities (name text, code text)`
//!
//!   2. Weather data about cities in `connector B`
//!
//! `weather (city_code text, temperature float, wind_speed float, humidity float)`
//!
//!
//! Given the above, and assuming there are corresponding models defined for
//! these two tables in the metadata, one can define a [remote
//! relationship](https://hasura.io/docs/3.0/supergraph-modeling/relationships/).
//! In the above two tables, the "city code" is guaranteed to be a unique field.
//! So one can use that as the mapping field and define a relationship.
//!
//! Once a relationship is defined, one can make a GraphQL query like -
//!
//! ```graphql
//! city {
//!   name
//!   weather {
//!     temperature
//!     humidity
//!   }
//! }
//! ```
//! #### Note
//! There has to be one or more fields across the two datasets which are unique.
//! The remote relationship mapping should be defined using those fields. If
//! there are no collection of unique fields, then a remote relationship cannot
//! be defined.
//!
//! # How it works
//!
//! ## IR generation
//! - Selection set IR generation function - [generate_selection_set_ir]
//! - Model Remote relationship - [build_remote_relationship]
//! - Command Remote relationship - [build_remote_command_relationship]
//!
//! ## Join Tree generation
//! - The join tree is generated as part of query plan. See [process_selection_set_ir] function.
//! - To know about the types for remote joins, see the [types] module.
//!
//! ## Execution
//! Following is the high-level algorithm how remote joins execution is performed.
//!
//! 1. Make the first top-level NDC query, and call the response as LHS response.
//!
//! 2. Traverse the join tree to get to the next remote join. Iterate through
//! all the rows in the LHS response, use the field mapping in the remote join node
//! to collect the values of those fields. In the above example, these would be
//! collecting the city codes from the LHS city query.
//!
//! 3. Get the NDC query from the remote join node, and attach the values in the
//! above step as variables in the NDC query. This NDC query already has a
//! "where" filter clause with a variable on the join mapping field. Make the
//! NDC query, and call the response as RHS response.
//!
//! 4. If there is a sub-tree from this remote join node, recursively perform
//! this algorithm.
//!
//! 5. Perform join on LHS response and RHS response
//!
//! [process_selection_set_ir]: crate::execute::plan::selection_set::process_selection_set_ir
//! [generate_selection_set_ir]: crate::execute::ir::selection_set::generate_selection_set_ir
//! [build_remote_relationship]: crate::execute::ir::relationship::build_remote_relationship
//! [build_remote_command_relationship]: crate::execute::ir::relationship::build_remote_command_relationship

use async_recursion::async_recursion;
use ndc_models;
use serde_json as json;
use std::collections::{BTreeMap, HashMap};
use tracing_util::SpanVisibility;

use super::ndc::execute_ndc_query;
use super::plan::ProcessResponseAs;
use super::{error, HttpContext, ProjectId};

use self::collect::CollectArgumentResult;
use types::{Argument, JoinId, JoinLocations, RemoteJoin};

pub(crate) mod collect;
pub(crate) mod join;
pub(crate) mod types;

/// Execute remote joins. As an entry-point it assumes the response is available
/// for the top-level query, and executes further remote joins recursively.
#[async_recursion]
pub(crate) async fn execute_join_locations<'ir>(
    http_context: &HttpContext,
    execution_span_attribute: &'static str,
    lhs_response: &mut Vec<ndc_models::RowSet>,
    lhs_response_type: &ProcessResponseAs,
    join_locations: &JoinLocations<(RemoteJoin<'async_recursion, 'ir>, JoinId)>,
    project_id: Option<&ProjectId>,
) -> Result<(), error::Error>
where
    'ir: 'async_recursion,
{
    let tracer = tracing_util::global_tracer();
    for (key, location) in &join_locations.locations {
        // collect the join column arguments from the LHS response, also get
        // the replacement tokens
        let collect_arg_res = tracer.in_span(
            "collect_arguments",
            "Collect arguments for join".into(),
            SpanVisibility::Internal,
            || collect::collect_arguments(lhs_response, lhs_response_type, key, location),
        )?;
        if let Some(CollectArgumentResult {
            arguments,
            mut join_node,
            sub_tree,
            remote_alias,
        }) = collect_arg_res
        {
            // patch the target/RHS IR with variable values
            let (join_variables_, _argument_ids): (Vec<_>, Vec<_>) = arguments.into_iter().unzip();
            let join_variables: Vec<BTreeMap<String, json::Value>> = join_variables_
                .iter()
                .map(|bmap| bmap.iter().map(|(k, v)| (k.clone(), v.0.clone())).collect())
                .collect();
            join_node.target_ndc_ir.variables = Some(join_variables);

            // execute the remote query
            let mut target_response = tracer
                .in_span_async(
                    "execute_remote_join_query",
                    "Execute remote query for join".to_string(),
                    SpanVisibility::Internal,
                    || {
                        Box::pin(execute_ndc_query(
                            http_context,
                            &join_node.target_ndc_ir,
                            join_node.target_data_connector,
                            execution_span_attribute,
                            remote_alias.clone(),
                            project_id,
                        ))
                    },
                )
                .await?;

            // if there is a `location.rest`, recursively process the tree; which
            // will modify the `target_response` with all joins down the tree
            if !location.rest.locations.is_empty() {
                execute_join_locations(
                    http_context,
                    execution_span_attribute,
                    &mut target_response,
                    &join_node.process_response_as,
                    &sub_tree,
                    project_id,
                )
                .await?;
            }

            tracer.in_span(
                "response_join",
                "Join responses for remote query".into(),
                SpanVisibility::Internal,
                || {
                    // from `Vec<RowSet>` create `HashMap<Argument, RowSet>`
                    let rhs_response: HashMap<Argument, ndc_models::RowSet> =
                        join_variables_.into_iter().zip(target_response).collect();

                    join::join_responses(key, &remote_alias, location, lhs_response, &rhs_response)
                },
            )?;
        }
    }
    Ok(())
}
