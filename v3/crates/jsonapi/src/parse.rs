use super::types::{Model, ModelInfo, ParseError, RequestError};
use axum::http::{Method, Uri};
use indexmap::IndexMap;
use metadata_resolve::ModelExpressionType;
use open_dds::{
    identifier,
    identifier::{Identifier, SubgraphName},
    models::ModelName,
    types::FieldName,
};
use std::collections::BTreeMap;

pub fn create_query_ir(
    model: &Model,
    _http_method: &Method,
    uri: &Uri,
    query_string: &jsonapi_library::query::Query,
) -> Result<open_dds::query::QueryRequest, RequestError> {
    // get model info from parsing URI
    let ModelInfo {
        subgraph,
        name: model_name,
        unique_identifier: _,
        relationship: _,
    } = parse_url(uri)?;

    validate_sparse_fields(model, query_string)?;

    // create the selection fields; include all fields of the model output type
    let mut selection = IndexMap::new();
    for field_name in model.type_fields.keys() {
        if include_field(query_string, field_name, &model.name.name) {
            let field_name_ident = Identifier::new(field_name.as_str()).unwrap();
            let field_name = open_dds::types::FieldName::new(field_name_ident.clone());
            let field_alias = open_dds::query::Alias::new(field_name_ident);
            let sub_sel =
                open_dds::query::ObjectSubSelection::Field(open_dds::query::ObjectFieldSelection {
                    target: open_dds::query::ObjectFieldTarget {
                        arguments: IndexMap::new(),
                        field_name,
                    },
                    selection: None,
                });
            selection.insert(field_alias, sub_sel);
        }
    }

    // create filters
    let filter_query = query_string
        .filter
        .as_ref()
        .map(|filter| build_boolean_expression(model, filter));

    // create sorts
    let sort_query = match &query_string.sort {
        None => Ok(vec![]),
        Some(sort) => sort
            .iter()
            .map(|elem| Ok(build_order_by_element(elem)?))
            .collect::<Result<Vec<_>, RequestError>>(),
    }?;

    // pagination
    // spec: <https://jsonapi.org/format/#fetching-pagMetadata>
    // FIXME: unwrap
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
        selection,
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

// check all fields in sparse fields are accessible, explode if not
// this will disallow relationship or nested fields
fn validate_sparse_fields(
    model: &Model,
    query_string: &jsonapi_library::query::Query,
) -> Result<(), RequestError> {
    let model_name_string = model.name.name.to_string();
    if let Some(fields) = &query_string.fields {
        for (model_name, model_fields) in fields {
            if *model_name == model_name_string {
                for models_field in model_fields {
                    let string_fields: Vec<_> =
                        model.type_fields.keys().map(ToString::to_string).collect();

                    if !string_fields.contains(models_field) {
                        return Err(RequestError::BadRequest(format!(
                            "Unknown field in sparse fields: {models_field}"
                        )));
                    }
                }
            } else {
                return Err(RequestError::BadRequest(format!(
                    "Unknown model in sparse fields: {model_name}"
                )));
            }
        }
    }
    Ok(())
}

// given the sparse fields for this request, should be include a given field in the query?
// this does not consider subgraphs at the moment - we match on `ModelName` not
// `Qualified<ModelName>`.
// This means that the below field is ambiguous where `Authors` model is defined in multiple
// subgraphs
// fields[Authors]=author_id,first_name
//
// two possible solutions:
// 1. make users qualify the name inline
//
// fields[subgraph.Authors]=author_id,first_name&fields[other.Authors]=author_id,last_name
//
// 2. much like we make users explicitly give GraphQL names to things, we
// make them give JSONAPI models an unambiguous name in metadata, and the user provides that:
//
// fields[subgraphAuthors]=author_id,firstName&fields[otherAuthors]=author_id,last_name
fn include_field(
    query_string: &jsonapi_library::query::Query,
    field_name: &FieldName,
    model_name: &ModelName,
) -> bool {
    if let Some(fields) = &query_string.fields {
        if let Some(model_fields) = fields.get(model_name.as_str()) {
            for model_field in model_fields {
                if model_field == field_name.as_str() {
                    return true;
                }
            }
        }
    }
    // if no sparse fields provided for our model, return everything
    true
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
            field_name: open_dds::types::FieldName::new(Identifier::new(field_name)?),
            arguments: IndexMap::new(),
        }),
        nested: None,
    });
    Ok(open_dds::query::OrderByElement { operand, direction })
}

/* Example filter as query string - /movies?sort=...&filter=<json>
{
  "$or": [
    {
      "name": {
        "$eq": "Braveheart"
      }
    },
    {
      "name": {
        "$eq": "Gladiator"
      }
    },
    {
      "$and": [
        {
          "director": {
            "last_name": {
              "$eq": "Scorcese"
            }
          }
        },
        {
          "director": {
            "age": {
              "$gt": 50
            }
          }
        }
      ]
    }
  ],
  "name": {
    "$eq": "Foobar"
  }
}
*/
fn build_boolean_expression(
    model: &Model,
    _filter: &BTreeMap<String, Vec<String>>,
) -> open_dds::query::BooleanExpression {
    // TODO: actually parse and create a BooleanExpression
    if let Some(filter_exp_type) = &model.filter_expression_type {
        match filter_exp_type {
            ModelExpressionType::BooleanExpressionType(_x) => {}
            ModelExpressionType::ObjectBooleanExpressionType(_x) => {}
        }
    }
    // dummy return
    open_dds::query::BooleanExpression::IsNull(open_dds::query::Operand::Field(
        open_dds::query::ObjectFieldOperand {
            target: Box::new(open_dds::query::ObjectFieldTarget {
                field_name: open_dds::types::FieldName::new(identifier!("dummy")),
                arguments: IndexMap::new(),
            }),
            nested: None,
        },
    ))
}

fn parse_url(uri: &Uri) -> Result<ModelInfo, ParseError> {
    let path = uri.path();
    let paths = path
        .split('/')
        .filter(|p| !p.is_empty())
        .collect::<Vec<_>>();
    if paths.len() < 2 {
        return Err("Path length MUST BE >=2".into());
    }
    let subgraph = paths[0];
    let model_name = paths[1];
    let unique_identifier = paths.get(2).map(|x| (*x).to_string());
    let mut relationship = Vec::new();
    if paths.get(3).is_some() {
        relationship = paths[3..].iter().map(|i| (*i).to_string()).collect();
    }
    Ok(ModelInfo {
        name: ModelName::new(Identifier::new(model_name)?),
        subgraph: SubgraphName::try_new(subgraph)?,
        unique_identifier,
        relationship,
    })
}
