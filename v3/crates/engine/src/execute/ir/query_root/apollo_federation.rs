use std::collections::{BTreeMap, HashMap};

use hasura_authn_core::SessionVariables;
use lang_graphql::{ast::common as ast, normalized_ast};
use ndc_models;
use open_dds::types::CustomTypeName;
use serde::Serialize;

use crate::execute::error;
use crate::execute::ir::filter::ResolvedFilterExpression;
use crate::execute::ir::model_selection;
use crate::execute::model_tracking::UsagesCounts;
use crate::metadata::resolved;
use crate::metadata::resolved::subgraph::Qualified;
use crate::metadata::resolved::types::mk_name;
use crate::schema::types::{EntityFieldTypeNameMapping, NamespaceAnnotation};
use crate::schema::GDS;
use crate::utils::HashMapWithJsonKey;

/// IR for the '_entities' operation for a model
#[derive(Serialize, Debug)]
pub struct EntitySelect<'n, 's> {
    // The name of the field as published in the schema
    pub field_name: &'n ast::Name,

    /// Model Selection IR fragment
    pub model_selection: model_selection::ModelSelection<'s>,

    // We need this for validating the response from the data connector. This is not a reference as it is constructed
    // from the original selection set by filtering fields that are relevant.
    pub selection_set: normalized_ast::SelectionSet<'s, GDS>,

    // All the models/commands used in this operation. This includes the models/commands used via relationships. And in
    // future, the models/commands used in the filter clause
    pub(crate) usage_counts: UsagesCounts,
}

fn get_entity_namespace_typename_mappings<'s>(
    field_call: &normalized_ast::FieldCall<'s, GDS>,
) -> Result<
    &'s HashMapWithJsonKey<Qualified<CustomTypeName>, resolved::model::FilterPermission>,
    error::Error,
> {
    field_call
        .info
        .namespaced
        .as_ref()
        .and_then(|annotation| match annotation {
            NamespaceAnnotation::EntityTypeMappings(type_mappings) => Some(type_mappings),
            _ => None,
        })
        .ok_or(error::Error::InternalError(error::InternalError::Engine(
            error::InternalEngineError::ExpectedNamespaceAnnotationNotFound {
                namespace_annotation_type: "Entity type mappings".to_string(),
            },
        )))
}

/// Generate the NDC IR for the entities root field.
///
/// This function generates the NDC IR for the entities root field. The entities query looks something like:
///
/// ```graphql
/// query MyQuery($representations: [_Any!]!) {
///   _entities(representations: $representations) {
///     ... on album {
///       AlbumId
///     }
///     ... on Article {
///       id
///     }
///   }
/// }
/// ```
/// The `representations` argument is a list of objects with a `__typename` field and the fields that are used to filter
/// the entities. The `__typename` field is used to determine the type of the entity and the fields are used to filter
/// the entities.
pub(crate) fn entities_ir<'n, 's>(
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    typename_mappings: &'s HashMap<ast::TypeName, EntityFieldTypeNameMapping>,
    session_variables: &SessionVariables,
) -> Result<Vec<EntitySelect<'n, 's>>, error::Error> {
    let representations = field_call
        .expected_argument(&lang_graphql::mk_name!("representations"))?
        .value
        .as_list()?;
    let mut entity_selects = vec![];
    for representation in representations {
        let json_representation = representation.as_json();
        let representation = json_representation.as_object().ok_or(
            lang_graphql::normalized_ast::Error::UnexpectedValue {
                expected_kind: "OBJECT",
                found: json_representation.clone(),
            },
        )?;
        // The __typename field is used to determine the type of the entity
        let typename_value = representation.get("__typename").ok_or(
            error::Error::FieldNotFoundInEntityRepresentation {
                field_name: "__typename".to_string(),
            },
        )?;
        let typename_str = typename_value.as_str().ok_or(
            lang_graphql::normalized_ast::Error::UnexpectedValue {
                expected_kind: "STRING",
                found: typename_value.clone(),
            },
        )?;

        let typename = ast::TypeName(mk_name(typename_str).map_err(|_| {
            error::Error::TypeFieldInvalidGraphQlName {
                name: typename_str.to_string(),
            }
        })?);
        // Get the permissions for the typename
        let typename_permissions: &'s HashMap<
            Qualified<CustomTypeName>,
            resolved::model::FilterPermission,
        > = &get_entity_namespace_typename_mappings(field_call)?.0;
        let typename_mapping = typename_mappings.get(&typename).ok_or(
            error::InternalDeveloperError::TypenameMappingNotFound {
                type_name: typename.clone(),
                mapping_kind: "entity key field",
            },
        )?;
        let role_model_select_permission = typename_permissions.get(&typename_mapping.type_name);
        // If the typename has a permission, then we can proceed to generate the NDC IR for the entity
        if let Some(role_model_select_permission) = role_model_select_permission {
            // Get the model source for the entity
            let model_source = typename_mapping.model_source.as_ref().ok_or(
                error::InternalDeveloperError::NoSourceDataConnector {
                    type_name: typename.clone(),
                    field_name: lang_graphql::mk_name!("_entities"),
                },
            )?;

            // Generate the filter clause for the entity
            let filter_clause_expressions = typename_mapping
                .key_fields_ndc_mapping
                .iter()
                .map(|(field_name, field_mapping)| {
                    // Get the value of the field from the representation
                    let val = representation.get(&field_name.0 .0).ok_or(
                        error::Error::FieldNotFoundInEntityRepresentation {
                            field_name: field_name.0 .0.clone(),
                        },
                    )?;
                    Ok(ndc_models::Expression::BinaryComparisonOperator {
                        column: ndc_models::ComparisonTarget::Column {
                            name: field_mapping.column.clone(),
                            path: vec![], // We don't support nested fields in the key fields, so the path is empty
                        },
                        operator: field_mapping.equal_operator.clone(),
                        value: ndc_models::ComparisonValue::Scalar { value: val.clone() },
                    })
                })
                .collect::<Result<_, error::Error>>()?;

            // Filter the selection set to only include fields that are relevant to the entity
            let new_selection_set = field.selection_set.filter_field_calls_by_typename(typename);

            let filter_clauses = ResolvedFilterExpression {
                expression: Some(ndc_models::Expression::And {
                    expressions: filter_clause_expressions,
                }),
                relationships: BTreeMap::new(),
            };
            let mut usage_counts = UsagesCounts::new();
            let model_selection = model_selection::model_selection_ir(
                &new_selection_set,
                &typename_mapping.type_name,
                model_source,
                BTreeMap::new(),
                filter_clauses,
                role_model_select_permission,
                None, // limit
                None, // offset
                None, // order_by
                session_variables,
                // Get all the models/commands that were used as relationships
                &mut usage_counts,
            )?;
            entity_selects.push(EntitySelect {
                field_name: &field_call.name,
                model_selection,
                selection_set: new_selection_set,
                usage_counts,
            });
        }
    }
    Ok(entity_selects)
}
