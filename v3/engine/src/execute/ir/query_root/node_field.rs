//! IR of the relay according to https://relay.dev/graphql/objectidentification.htm

use base64::{engine::general_purpose, Engine};
use hasura_authn_core::SessionVariables;
use lang_graphql::{ast::common as ast, normalized_ast};
use ndc_client as ndc;
use open_dds::permissions::Role;
use serde::Serialize;
use std::collections::{BTreeMap, HashMap};

use crate::execute::error;
use crate::execute::ir::model_selection;
use crate::execute::model_tracking::UsagesCounts;
use crate::metadata::resolved;
use crate::schema::types::{GlobalID, NodeFieldTypeNameMapping};
use crate::schema::GDS;

/// IR for the 'select_one' operation on a model
#[derive(Serialize, Debug)]
pub struct NodeSelect<'n, 's> {
    // The name of the field as published in the schema
    pub field_name: &'n ast::Name,

    /// Model Selection IR fragment
    pub model_selection: model_selection::ModelSelection<'s>,

    // We need this to post process the response for `__typename` fields and for
    // validating the response from the data connector. This is not a reference
    // as it is constructed from the original selection set by filtering fields
    // that are relevant.
    pub selection_set: normalized_ast::SelectionSet<'s, GDS>,

    // All the models/commands used in this operation. This includes the models/commands
    // used via relationships. And in future, the models/commands used in the filter clause
    pub(crate) usage_counts: UsagesCounts,
}

/// Generate the NDC IR for the node root field.

/// This function, decodes the value of the `id`
/// argument and then looks the `typename` up in the
/// `typename_mappings`. A successful lookup will yield the
/// `data_specification::TypeName` and the `ModelSource`
/// associated with the typename and a Hashset of roles that
/// can access the Object coresponding to the type name.
/// If the role, doesn't have model select permissions
/// to the model that is the global ID source for the
/// object type that was decoded, then this function
/// returns `None`.
pub(crate) fn relay_node_ir<'n, 's>(
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    typename_mappings: &'s HashMap<ast::TypeName, NodeFieldTypeNameMapping>,
    role: &Role,
    session_variables: &SessionVariables,
) -> Result<Option<NodeSelect<'n, 's>>, error::Error> {
    let id_arg_value = field_call
        .expected_argument(&lang_graphql::mk_name!("id"))?
        .value
        .as_id()?;
    let decoded_id_value = general_purpose::STANDARD
        .decode(id_arg_value.clone())
        .map_err(|e| error::Error::ErrorInDecodingGlobalId {
            encoded_value: id_arg_value.clone(),
            decoding_error: e.to_string(),
        })?;
    let global_id: GlobalID = serde_json::from_slice(decoded_id_value.as_slice())?;
    let typename_mapping = typename_mappings.get(&global_id.typename).ok_or(
        error::InternalDeveloperError::GlobalIDTypenameMappingNotFound {
            type_name: global_id.typename.clone(),
        },
    )?;
    let role_model_select_permission = typename_mapping.model_select_permissions.get(role);
    match role_model_select_permission {
        // When a role doesn't have any model select permissions on the model
        // that is the Global ID source for the object type, we just return `null`.
        None => Ok(None),
        Some(role_model_select_permission) => {
            let model_source = typename_mapping.model_source.as_ref().ok_or(
                error::InternalDeveloperError::NoSourceDataConnector {
                    type_name: global_id.typename.clone(),
                    field_name: lang_graphql::mk_name!("node"),
                },
            )?;

            let field_mappings = model_source
                .type_mappings
                .get(&typename_mapping.type_name)
                .map(|type_mapping| match type_mapping {
                    resolved::types::TypeMapping::Object { field_mappings } => field_mappings,
                })
                .ok_or_else(|| error::InternalEngineError::InternalGeneric {
                    description: format!(
                        "type '{}' not found in model source type_mappings",
                        typename_mapping.type_name
                    ),
                })?;
            let filter_clauses = global_id
                .id
                .iter()
                .map(|(field_name, val)| {
                    let field_mapping = &field_mappings.get(field_name).ok_or_else(|| {
                        error::InternalEngineError::InternalGeneric {
                            description: format!("invalid field in annotation: {field_name:}"),
                        }
                    })?;
                    Ok(ndc::models::Expression::BinaryComparisonOperator {
                        column: ndc::models::ComparisonTarget::Column {
                            name: field_mapping.column.clone(),
                            path: vec![],
                        },
                        operator: ndc::models::BinaryComparisonOperator::Equal,
                        value: ndc::models::ComparisonValue::Scalar { value: val.clone() },
                    })
                })
                .collect::<Result<_, error::Error>>()?;

            let new_selection_set = field
                .selection_set
                .filter_field_calls_by_typename(global_id.typename);

            let mut usage_counts = UsagesCounts::new();
            let model_selection = model_selection::model_selection_ir(
                &new_selection_set,
                &typename_mapping.type_name,
                model_source,
                BTreeMap::new(),
                filter_clauses,
                &role_model_select_permission.filter,
                None, // limit
                None, // offset
                None, // order_by
                session_variables,
                // Get all the models/commands that were used as relationships
                &mut usage_counts,
            )?;
            Ok(Some(NodeSelect {
                field_name: &field_call.name,
                model_selection,
                selection_set: new_selection_set,
                usage_counts,
            }))
        }
    }
}
