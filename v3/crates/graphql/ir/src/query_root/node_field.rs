//! IR of the relay according to <https://relay.dev/graphql/objectidentification.htm>

use std::collections::{BTreeMap, HashMap};

use base64::{Engine, engine::general_purpose};
use hasura_authn_core::Session;
use indexmap::IndexMap;
use lang_graphql::{ast::common as ast, normalized_ast};
use open_dds::types::CustomTypeName;
use serde::Serialize;

use crate::error;
use crate::model_selection;
use graphql_schema::GDS;
use graphql_schema::{GlobalID, NamespaceAnnotation, NodeFieldTypeNameMapping};
use json_ext::HashMapWithJsonKey;
use metadata_resolve;
use metadata_resolve::Qualified;
use plan_types::UsagesCounts;

/// IR for the 'select_one' operation on a model
#[derive(Serialize, Debug)]
pub struct NodeSelect<'n, 's> {
    // The name of the field as published in the schema
    pub field_name: &'n ast::Name,

    /// Model Selection IR fragment
    pub model_selection: open_dds::query::ModelSelection,

    // We need this to post process the response for `__typename` fields and for
    // validating the response from the data connector. This is not a reference
    // as it is constructed from the original selection set by filtering fields
    // that are relevant.
    pub selection_set: normalized_ast::SelectionSet<'s, GDS>,

    // All the models/commands used in this operation. This includes the models/commands
    // used via relationships. And in future, the models/commands used in the filter clause
    pub(crate) usage_counts: UsagesCounts,
}

fn get_relay_node_namespace_typename_mappings<'s>(
    field_call: &normalized_ast::FieldCall<'s, GDS>,
) -> Result<
    &'s HashMapWithJsonKey<Qualified<CustomTypeName>, metadata_resolve::FilterPermission>,
    error::Error,
> {
    field_call
        .info
        .namespaced
        .as_ref()
        .and_then(|annotation| match annotation {
            NamespaceAnnotation::NodeFieldTypeMappings(type_mappings) => Some(type_mappings),
            _ => None,
        })
        .ok_or(error::Error::Internal(error::InternalError::Engine(
            error::InternalEngineError::ExpectedNamespaceAnnotationNotFound {
                namespace_annotation_type: "Node type mappings".to_string(),
            },
        )))
}

/// Generate the NDC IR for the node root field.
///
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
    models: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    object_types: &'s BTreeMap<
        metadata_resolve::Qualified<open_dds::types::CustomTypeName>,
        metadata_resolve::ObjectTypeWithRelationships,
    >,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
) -> Result<Option<NodeSelect<'n, 's>>, error::Error> {
    let id_arg_value = field_call
        .expected_argument(&lang_graphql::mk_name!("id"))?
        .value
        .as_id()?;

    let decoded_id_value = general_purpose::STANDARD
        .decode(id_arg_value.clone())
        .map_err(|e| error::Error::FailureDecodingGlobalId {
            encoded_value: id_arg_value.clone(),
            decoding_error: e.to_string(),
        })?;

    let global_id: GlobalID = serde_json::from_slice(decoded_id_value.as_slice())?;

    let typename_permissions: &'s HashMap<
        Qualified<CustomTypeName>,
        metadata_resolve::FilterPermission,
    > = &get_relay_node_namespace_typename_mappings(field_call)?.0;

    let typename_mapping = typename_mappings.get(&global_id.typename).ok_or(
        error::InternalDeveloperError::TypenameMappingNotFound {
            type_name: global_id.typename.clone(),
            mapping_kind: "Global ID",
        },
    )?;

    let role_model_select_permission = typename_permissions.get(&typename_mapping.type_name);

    match role_model_select_permission {
        // When a role doesn't have any model select permissions on the model
        // that is the Global ID source for the object type, we just return `null`.
        None => Ok(None),
        Some(_role_model_select_permission) => {
            let model_source = typename_mapping.model_source.as_ref().ok_or(
                error::InternalDeveloperError::NoSourceDataConnector {
                    type_name: global_id.typename.clone(),
                    field_name: lang_graphql::mk_name!("node"),
                },
            )?;

            let new_selection_set = field
                .selection_set
                .filter_field_calls_by_typename(global_id.typename.clone());

            let mut usage_counts = UsagesCounts::new();

            let filter_clause_expressions = global_id
                .id
                .iter()
                .map(
                    |(field_name, val)| open_dds::query::BooleanExpression::Comparison {
                        operand: open_dds::query::Operand::Field(
                            open_dds::query::ObjectFieldOperand {
                                target: Box::new(open_dds::query::ObjectFieldTarget {
                                    field_name: field_name.clone(),
                                    arguments: IndexMap::new(),
                                }),
                                nested: None,
                            },
                        ),
                        operator: open_dds::query::ComparisonOperator::Equals,
                        argument: Box::new(open_dds::query::Value::Literal(val.clone())),
                    },
                )
                .collect();
            let boolean_expression =
                open_dds::query::BooleanExpression::And(filter_clause_expressions);

            let model_selection = model_selection::model_selection_open_dd_ir(
                &new_selection_set,
                &typename_mapping.model_name,
                models,
                &model_source.type_mappings,
                object_types,
                None, // arguments
                Some(boolean_expression),
                vec![], // order_by
                None,   // limit
                None,   // offset
                &session.variables,
                request_headers,
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
