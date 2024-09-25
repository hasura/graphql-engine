use graphql_schema::GDS;
use lang_graphql::ast::common::{self as ast};
use lang_graphql::normalized_ast::Operation;
use open_dds::query::{
    Alias, ModelSelection, ModelTarget, ObjectFieldSelection, ObjectFieldTarget,
    ObjectSubSelection, Query, QueryRequest, QueryRequestV1, Value,
};
use open_dds::{arguments::ArgumentName, identifier::Identifier};

use indexmap::IndexMap;

// given a resolved GraphQL request, turn it into OpenDD IR
pub fn to_opendd_ir(operation: &Operation<GDS>) -> QueryRequest {
    if operation.ty != ast::OperationType::Query {
        todo!("Convert non-queries");
    }

    let mut queries = IndexMap::new();
    for query in operation.selection_set.fields.values() {
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
                    let opendd_alias =
                        Alias::new(Identifier::new(field_call.name.as_str()).unwrap());

                    let selection = to_model_selection(&query.selection_set.fields);

                    let arguments = to_model_arguments(&field_call.arguments);

                    let query = Query::Model(ModelSelection {
                        selection,
                        target: ModelTarget {
                            arguments,
                            filter: None,
                            limit: None,
                            offset: None,
                            order_by: vec![],
                            model_name: name.name.clone(),
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

fn to_model_arguments(
    arguments: &IndexMap<ast::Name, lang_graphql::normalized_ast::InputField<GDS>>,
) -> IndexMap<ArgumentName, Value> {
    let mut model_arguments = IndexMap::new();
    for (name, argument) in arguments {
        let argument_name = ArgumentName::new(Identifier::new(name.as_str()).unwrap());
        let argument_value = open_dds::query::Value::Literal(argument.value.as_json());

        model_arguments.insert(argument_name, argument_value);
    }

    model_arguments
}

fn to_model_selection(
    fields: &IndexMap<ast::Alias, lang_graphql::normalized_ast::Field<GDS>>,
) -> IndexMap<Alias, ObjectSubSelection> {
    let mut selection: IndexMap<Alias, ObjectSubSelection> = IndexMap::new();

    for field in fields.values() {
        let field_alias = Alias::new(Identifier::new(field.alias.0.as_str()).unwrap());

        let field_name = match field.field_calls.iter().next() {
            Some((_, field_call)) => match field_call.info.generic {
                graphql_schema::Annotation::Output(graphql_schema::OutputAnnotation::Field {
                    name,
                    ..
                }) => name,
                _ => todo!("only the simplest fields supported"),
            },
            _ => todo!("error: a field call must exist"),
        };

        let object_sub_selection = ObjectSubSelection::Field(ObjectFieldSelection {
            selection: None,
            target: ObjectFieldTarget {
                field_name: field_name.clone(),
                arguments: IndexMap::new(),
            },
        });

        selection.insert(field_alias, object_sub_selection);
    }
    selection
}
