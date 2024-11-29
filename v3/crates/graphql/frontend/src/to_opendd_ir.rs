use graphql_schema::GDS;
use lang_graphql::ast::common::{self as ast};
use lang_graphql::normalized_ast::Operation;
use open_dds::query::{
    Alias, CommandSelection, CommandTarget, ModelSelection, ModelTarget, ObjectFieldSelection,
    ObjectFieldTarget, ObjectSubSelection, Query, QueryRequest, QueryRequestV1,
    RelationshipSelection, RelationshipTarget, Value,
};
use open_dds::{arguments::ArgumentName, identifier::Identifier};

use indexmap::IndexMap;

// given a resolved GraphQL request, turn it into OpenDD IR
pub fn to_opendd_ir(operation: &Operation<GDS>) -> QueryRequest {
    if operation.ty != ast::OperationType::Query {
        todo!("Convert non-queries");
    }

    let mut queries = IndexMap::new();
    for (query_alias, query) in &operation.selection_set.fields {
        for field_call in query.field_calls.values() {
            match field_call.info.generic {
                graphql_schema::Annotation::Output(
                    graphql_schema::OutputAnnotation::RootField(
                        graphql_schema::RootFieldAnnotation::Model {
                            data_type: _,
                            source: _,
                            kind: _,
                            name,
                        },
                    ),
                ) => {
                    let opendd_alias = Alias::new(Identifier::new(query_alias.0.as_str()).unwrap());

                    let selection = to_model_selection(&query.selection_set.fields);

                    let ArgumentOutputs {
                        arguments,
                        offset,
                        limit,
                    } = to_model_arguments(&field_call.arguments);

                    let query = Query::Model(ModelSelection {
                        selection,
                        target: ModelTarget {
                            arguments,
                            filter: None,
                            limit,
                            offset,
                            order_by: vec![],
                            model_name: name.name.clone(),
                            subgraph: name.subgraph.clone(),
                        },
                    });

                    queries.insert(opendd_alias, query);
                }
                graphql_schema::Annotation::Output(
                    graphql_schema::OutputAnnotation::RootField(
                        graphql_schema::RootFieldAnnotation::FunctionCommand { name, .. },
                    ),
                ) => {
                    let opendd_alias = Alias::new(Identifier::new(query_alias.0.as_str()).unwrap());

                    let selection = to_model_selection(&query.selection_set.fields);

                    let ArgumentOutputs { arguments, .. } =
                        to_model_arguments(&field_call.arguments);

                    let query = Query::Command(CommandSelection {
                        selection: Some(selection),
                        target: CommandTarget {
                            arguments,
                            command_name: name.name.clone(),
                            subgraph: name.subgraph.clone(),
                        },
                    });

                    queries.insert(opendd_alias, query);
                }

                _ => todo!("not implemented yet"),
            }
        }
    }

    QueryRequest::V1(QueryRequestV1 { queries })
}

struct ArgumentOutputs {
    arguments: IndexMap<ArgumentName, Value>,
    offset: Option<usize>,
    limit: Option<usize>,
}

fn to_model_arguments(
    arguments: &IndexMap<ast::Name, lang_graphql::normalized_ast::InputField<GDS>>,
) -> ArgumentOutputs {
    let mut model_arguments = IndexMap::new();
    let mut model_offset = None;
    let mut model_limit = None;

    for (name, argument) in arguments {
        // currently we are magically matching on strings, really we should be using the graphql field annotations
        // to determine the role of each field
        match name.as_str() {
            "offset" => {
                if let Some(offset_value) = match &argument.value {
                    lang_graphql::normalized_ast::Value::SimpleValue(
                        lang_graphql::normalized_ast::SimpleValue::Integer(i),
                    ) => usize::try_from(*i).ok(),
                    _ => None,
                } {
                    model_offset = Some(offset_value);
                }
            }
            "limit" => {
                if let Some(limit_value) = match &argument.value {
                    lang_graphql::normalized_ast::Value::SimpleValue(
                        lang_graphql::normalized_ast::SimpleValue::Integer(i),
                    ) => usize::try_from(*i).ok(),
                    _ => None,
                } {
                    model_limit = Some(limit_value);
                }
            }
            "args" => {
                // these are the `actual` model arguments, as opposed to stuff like `where` and `limit`
                // etc
                if let Ok(value_object) = argument.value.as_object() {
                    for (inner_name, inner_argument) in value_object {
                        let argument_name =
                            ArgumentName::new(Identifier::new(inner_name.as_str()).unwrap());
                        let argument_value =
                            open_dds::query::Value::Literal(inner_argument.value.as_json());

                        model_arguments.insert(argument_name, argument_value);
                    }
                }
            }
            _ => {
                // currently we add any other arguments for the time being, we will also need to
                // separate out `where` arguments and other bits and pieces, as well as decide what
                // happens with top-level arguments for select one (ie, the `albumId` primary key)
                //
                let argument_name = ArgumentName::new(Identifier::new(name.as_str()).unwrap());
                let argument_value = open_dds::query::Value::Literal(argument.value.as_json());

                model_arguments.insert(argument_name, argument_value);
            }
        }
    }

    ArgumentOutputs {
        arguments: model_arguments,
        offset: model_offset,
        limit: model_limit,
    }
}

fn to_model_selection(
    fields: &IndexMap<ast::Alias, lang_graphql::normalized_ast::Field<GDS>>,
) -> IndexMap<Alias, ObjectSubSelection> {
    let mut selection: IndexMap<Alias, ObjectSubSelection> = IndexMap::new();

    for field in fields.values() {
        let field_alias = Alias::new(Identifier::new(field.alias.0.as_str()).unwrap());

        let sub_selection = if field.selection_set.fields.is_empty() {
            None
        } else {
            Some(to_model_selection(&field.selection_set.fields))
        };

        let object_sub_selection = match field.field_calls.iter().next() {
            Some((_, field_call)) => match field_call.info.generic {
                graphql_schema::Annotation::Output(graphql_schema::OutputAnnotation::Field {
                    name,
                    ..
                }) => ObjectSubSelection::Field(ObjectFieldSelection {
                    selection: sub_selection,
                    target: ObjectFieldTarget {
                        field_name: name.clone(),
                        arguments: IndexMap::new(),
                    },
                }),
                graphql_schema::Annotation::Output(
                    graphql_schema::OutputAnnotation::RelationshipToModel(
                        model_relationship_annotation,
                    ),
                ) => {
                    let ArgumentOutputs {
                        arguments,
                        offset,
                        limit,
                    } = to_model_arguments(&field_call.arguments);
                    let relationship_target = RelationshipTarget {
                        relationship_name: model_relationship_annotation.relationship_name.clone(),
                        arguments,
                        filter: None,
                        order_by: vec![],
                        limit,
                        offset,
                    };
                    ObjectSubSelection::Relationship(RelationshipSelection {
                        target: relationship_target,
                        selection: sub_selection,
                    })
                }
                _ => todo!("only the simplest fields/relationship supported"),
            },
            _ => todo!("error: a field call must exist"),
        };

        selection.insert(field_alias, object_sub_selection);
    }
    selection
}
