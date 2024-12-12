use super::types::{ModelInfo, RelationshipNode, RelationshipTree, RequestError};
use axum::http::{Method, Uri};
use indexmap::IndexMap;
use open_dds::{
    identifier,
    identifier::{Identifier, SubgraphName},
    models::ModelName,
    query::{
        Alias, ObjectSubSelection, RelationshipSelection,
        RelationshipTarget as OpenDdRelationshipTarget,
    },
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, FieldName},
};
use serde::{Deserialize, Serialize};
mod filter;
mod include;
use crate::catalog::{Model, ObjectType, RelationshipTarget, Type};
use metadata_resolve::{unwrap_custom_type_name, Qualified};
use std::collections::BTreeMap;

#[derive(Debug, derive_more::Display, Serialize, Deserialize)]
pub enum ParseError {
    Filter(filter::FilterError),
    InvalidFieldName(String),
    InvalidModelName(String),
    InvalidSubgraph(String),
    PathLengthMustBeAtLeastTwo,
    CannotFindObjectType(Qualified<CustomTypeName>),
}

fn get_object_type<'a>(
    object_types: &'a BTreeMap<Qualified<CustomTypeName>, ObjectType>,
    object_type_name: &Qualified<CustomTypeName>,
) -> Result<&'a ObjectType, ParseError> {
    object_types
        .get(object_type_name)
        .ok_or_else(|| ParseError::CannotFindObjectType(object_type_name.clone()))
}

pub fn create_query_ir(
    model: &Model,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectType>,
    _http_method: &Method,
    uri: &Uri,
    relationship_tree: &mut RelationshipTree,
    query_string: &jsonapi_library::query::Query,
) -> Result<open_dds::query::QueryRequest, RequestError> {
    // get model info from parsing URI
    let ModelInfo {
        subgraph,
        name: model_name,
        unique_identifier: _,
        relationship: _,
    } = parse_url(uri).map_err(RequestError::ParseError)?;

    // validate the sparse fields in the query string
    validate_sparse_fields(object_types, query_string)?;

    // Parse the include relationships
    let include_relationships = query_string
        .include
        .as_ref()
        .map(|include| include::IncludeRelationships::parse(include));

    let field_selection = resolve_field_selection(
        object_types,
        &model.data_type,
        relationship_tree,
        query_string,
        include_relationships.as_ref(),
    )?;

    // create filters
    let filter_query = match &query_string.filter {
        Some(filter) => {
            let boolean_expression = filter::build_boolean_expression(model, filter)
                .map_err(|parse_error| RequestError::ParseError(ParseError::Filter(parse_error)))?;
            Ok(Some(boolean_expression))
        }
        None => Ok(None),
    }?;

    // create sorts
    let sort_query = match &query_string.sort {
        None => Ok(vec![]),
        Some(sort) => sort
            .iter()
            .map(|elem| build_order_by_element(elem).map_err(RequestError::ParseError))
            .collect::<Result<Vec<_>, RequestError>>(),
    }?;

    // pagination
    // spec: <https://jsonapi.org/format/#fetching-pagMetadata>
    let limit = query_string
        .page
        .as_ref()
        .and_then(|page| usize::try_from(page.limit).ok())
        .filter(|page| *page > 0);

    let offset = query_string
        .page
        .as_ref()
        .and_then(|page| usize::try_from(page.offset).ok())
        .filter(|page| *page > 0);

    // form the model selection
    let model_selection = open_dds::query::ModelSelection {
        selection: field_selection,
        target: open_dds::query::ModelTarget {
            arguments: IndexMap::new(),
            filter: filter_query,
            order_by: sort_query,
            limit,
            offset,
            model_name,
            subgraph,
        },
    };

    let queries = IndexMap::from_iter([(
        open_dds::query::Alias::new(identifier!("jsonapi_model_query")),
        open_dds::query::Query::Model(model_selection),
    )]);
    Ok(open_dds::query::QueryRequest::V1(
        open_dds::query::QueryRequestV1 { queries },
    ))
}

fn resolve_field_selection(
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectType>,
    object_type_name: &Qualified<CustomTypeName>,
    relationship_tree: &mut RelationshipTree,
    query_string: &jsonapi_library::query::Query,
    include_relationships: Option<&include::IncludeRelationships>,
) -> Result<IndexMap<Alias, ObjectSubSelection>, RequestError> {
    let object_type =
        get_object_type(object_types, object_type_name).map_err(RequestError::ParseError)?;

    // create the selection fields; include all fields of the model output type
    let mut selection = IndexMap::new();
    for (field_name, field_type) in &object_type.type_fields {
        if include_field(query_string, field_name, &object_type_name.name) {
            let field_name_ident = Identifier::new(field_name.as_str())
                .map_err(|e| RequestError::BadRequest(e.into()))?;

            let field_name = open_dds::types::FieldName::new(field_name_ident.clone());
            let field_alias = open_dds::query::Alias::new(field_name_ident);
            let sub_sel =
                open_dds::query::ObjectSubSelection::Field(open_dds::query::ObjectFieldSelection {
                    target: open_dds::query::ObjectFieldTarget {
                        arguments: IndexMap::new(),
                        field_name,
                    },
                    selection: resolve_nested_field_selection(
                        object_types,
                        relationship_tree,
                        query_string,
                        include_relationships,
                        field_type,
                    )?,
                });
            selection.insert(field_alias, sub_sel);
        }
    }
    // resolve include relationships
    let mut relationship_fields = resolve_include_relationships(
        object_type,
        object_types,
        relationship_tree,
        query_string,
        include_relationships,
    )?;
    selection.append(&mut relationship_fields);
    Ok(selection)
}

fn resolve_nested_field_selection(
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectType>,
    relationship_tree: &mut RelationshipTree,
    query_string: &jsonapi_library::query::Query,
    include_relationships: Option<&include::IncludeRelationships>,
    field_type: &Type,
) -> Result<Option<IndexMap<Alias, ObjectSubSelection>>, RequestError> {
    let selection = match field_type {
        Type::Scalar(_) | Type::ScalarForDataConnector(_) => None,
        Type::List(inner) => resolve_nested_field_selection(
            object_types,
            relationship_tree,
            query_string,
            include_relationships,
            inner.as_ref(),
        )?,
        Type::Object(type_name) => {
            let object_field_selection = resolve_field_selection(
                object_types,
                type_name,
                relationship_tree,
                query_string,
                include_relationships,
            )?;
            Some(object_field_selection)
        }
    };
    Ok(selection)
}

// check all types in sparse fields are accessible,
// for each type check all fields in sparse fields are accessible, explode if not
fn validate_sparse_fields(
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectType>,
    query_string: &jsonapi_library::query::Query,
) -> Result<(), RequestError> {
    if let Some(types) = &query_string.fields {
        for (type_name, type_fields) in types {
            let (_, object_type) = object_types
                .iter()
                .find(|(object_type_name, _)| object_type_name.name.0.as_str() == type_name)
                .ok_or_else(|| {
                    RequestError::BadRequest(format!("Unknown type in sparse fields: {type_name}"))
                })?;

            for type_field in type_fields {
                let string_fields: Vec<_> = object_type
                    .type_fields
                    .keys()
                    .map(ToString::to_string)
                    .collect();

                if !string_fields.contains(type_field) {
                    return Err(RequestError::BadRequest(format!(
                        "Unknown field in sparse fields: {type_field} in {type_name}"
                    )));
                }
            }
        }
    }
    Ok(())
}

fn resolve_include_relationships(
    object_type: &ObjectType,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectType>,
    relationship_tree: &mut RelationshipTree,
    query_string: &jsonapi_library::query::Query,
    include_relationships: Option<&include::IncludeRelationships>,
) -> Result<IndexMap<Alias, ObjectSubSelection>, RequestError> {
    let mut fields = IndexMap::new();
    if let Some(include_relationships) = include_relationships {
        for (relationship, nested_include) in &include_relationships.include {
            // Check the presence of the relationship
            let Some((relationship_name, target)) = object_type
                .type_relationships
                .iter()
                .find(|&(relationship_name, _)| relationship_name.as_str() == relationship)
            else {
                return Err(RequestError::BadRequest(format!(
                    "Relationship {relationship} not found"
                )));
            };
            let field_name_ident = Identifier::new(relationship)
                .map_err(|e| RequestError::BadRequest(format!("Invalid relationship name: {e}")))?;
            let (target_type, relationship_type) = match &target {
                RelationshipTarget::Model {
                    object_type,
                    relationship_type,
                } => (object_type, relationship_type.clone()),
                RelationshipTarget::ModelAggregate(model_type) => {
                    (model_type, RelationshipType::Object)
                }
                RelationshipTarget::Command { type_reference } => {
                    match unwrap_custom_type_name(type_reference) {
                        Some(object_type) => (
                            object_type,
                            crate::type_reference_to_relationship_type(type_reference),
                        ),
                        None => {
                            return Err(RequestError::BadRequest(
                                "Command relationship with built-in output type not supported yet"
                                    .to_string(),
                            ));
                        }
                    }
                }
            };
            let mut nested_relationships = RelationshipTree::default();
            let selection = resolve_field_selection(
                object_types,
                target_type,
                &mut nested_relationships,
                query_string,
                nested_include.as_ref(),
            )?;
            let relationship_node = RelationshipNode {
                object_type: target_type.clone(),
                relationship_type: relationship_type.clone(),
                nested: nested_relationships,
            };
            relationship_tree
                .relationships
                .insert(relationship.to_string(), relationship_node);
            let sub_selection = ObjectSubSelection::Relationship(RelationshipSelection {
                target: build_relationship_target(relationship_name.clone()),
                selection: Some(selection),
            });
            let field_alias = open_dds::query::Alias::new(field_name_ident);
            fields.insert(field_alias, sub_selection);
        }
    }
    Ok(fields)
}

fn build_relationship_target(relationship_name: RelationshipName) -> OpenDdRelationshipTarget {
    OpenDdRelationshipTarget {
        relationship_name,
        arguments: IndexMap::new(),
        filter: None,
        order_by: Vec::new(),
        limit: None,
        offset: None,
    }
}

// given the sparse fields for this request, should be include a given field in the query?
// this does not consider subgraphs at the moment - we match on `CustomTypeName` not
// `Qualified<CustomTypeName>`.
// This means that the below field is ambiguous where `Authors` type is defined in multiple
// subgraphs
// fields[Authors]=author_id,first_name
//
// This will need to be solved when we make users give JSONAPI types explicit names
// like we do in GraphQL
//
// fields[subgraphAuthors]=author_id,firstName&fields[otherAuthors]=author_id,last_name
fn include_field(
    query_string: &jsonapi_library::query::Query,
    field_name: &FieldName,
    object_type_name: &CustomTypeName,
) -> bool {
    if let Some(fields) = &query_string.fields {
        if let Some(object_fields) = fields.get(object_type_name.0.as_str()) {
            for object_field in object_fields {
                if object_field == field_name.as_str() {
                    return true;
                }
            }
            return false;
        }
    }
    // if no sparse fields provided for our model, return everything
    true
}

fn create_field_name(field_name: &str) -> Result<FieldName, ParseError> {
    let identifier = Identifier::new(field_name)
        .map_err(|_| ParseError::InvalidFieldName(field_name.to_string()))?;
    Ok(open_dds::types::FieldName::new(identifier))
}

// Sorting spec: <https://jsonapi.org/format/#fetching-sorting>
fn build_order_by_element(elem: &String) -> Result<open_dds::query::OrderByElement, ParseError> {
    let (field_name, direction) = if elem.starts_with('-') {
        (
            elem.split_at(1).1.to_string(),
            open_dds::models::OrderByDirection::Desc,
        )
    } else {
        (elem.to_string(), open_dds::models::OrderByDirection::Asc)
    };

    let operand = open_dds::query::Operand::Field(open_dds::query::ObjectFieldOperand {
        target: Box::new(open_dds::query::ObjectFieldTarget {
            field_name: create_field_name(&field_name)?,
            arguments: IndexMap::new(),
        }),
        nested: None,
    });
    Ok(open_dds::query::OrderByElement { operand, direction })
}

fn parse_url(uri: &Uri) -> Result<ModelInfo, ParseError> {
    let path = uri.path();
    let paths = path
        .split('/')
        .filter(|p| !p.is_empty())
        .collect::<Vec<_>>();
    if paths.len() < 2 {
        return Err(ParseError::PathLengthMustBeAtLeastTwo);
    }
    let subgraph = paths[0];
    let model_name = paths[1];
    let unique_identifier = paths.get(2).map(|x| (*x).to_string());
    let mut relationship = Vec::new();
    if paths.get(3).is_some() {
        relationship = paths[3..].iter().map(|i| (*i).to_string()).collect();
    }
    Ok(ModelInfo {
        name: ModelName::new(
            Identifier::new(model_name)
                .map_err(|_| ParseError::InvalidModelName(model_name.to_string()))?,
        ),
        subgraph: SubgraphName::try_new(subgraph)
            .map_err(|_| ParseError::InvalidSubgraph(subgraph.to_string()))?,
        unique_identifier,
        relationship,
    })
}
