use crate::ast::common as ast;
use crate::normalized_ast as normalized;
use crate::schema;
use crate::validation::error::*;

use super::source::*;

fn normalize_scalar_value<
    'q,
    's,
    S: schema::SchemaContext,
    V: ValueSource<'q, 's, S>,
    NSGet: schema::NamespacedGetter<S>,
>(
    schema: &'s schema::Schema<S>,
    namespaced_getter: &NSGet,
    context: &V::Context,
    location_type: &LocationType<'q, 's>,
    value: &V,
    scalar: &'s schema::Scalar,
) -> Result<normalized::Value<'s, S>> {
    match scalar.name.as_str() {
        "Int" => value.get_integer(schema, namespaced_getter, context, location_type),
        "ID" => value.get_id(schema, namespaced_getter, context, location_type),
        "Float" => value.get_float(schema, namespaced_getter, context, location_type),
        "Boolean" => value.get_boolean(schema, namespaced_getter, context, location_type),
        "String" => value.get_string(schema, namespaced_getter, context, location_type),
        _ => value.as_json_normalized(schema, namespaced_getter, context, location_type),
    }
}

pub fn normalize<
    'q,
    's,
    S: schema::SchemaContext,
    V: ValueSource<'q, 's, S>,
    NSGet: schema::NamespacedGetter<S>,
>(
    schema: &'s schema::Schema<S>,
    namespaced_getter: &NSGet,
    context: &V::Context,
    value: &V,
    location_type: &LocationType<'q, 's>,
    type_info: &schema::InputType<'s, S>,
) -> Result<normalized::Value<'s, S>>
where
    's: 'q,
{
    let normalized_value = match &location_type.type_().base {
        ast::BaseType::Named(_) => {
            // If the value is `null` and the field is nullable return `null`
            if value.is_null() && location_type.type_().nullable {
                return Ok(normalized::Value::SimpleValue(
                    normalized::SimpleValue::Null,
                ));
            };
            match type_info {
                schema::InputType::Scalar(scalar) => normalize_scalar_value(
                    schema,
                    namespaced_getter,
                    context,
                    location_type,
                    value,
                    scalar,
                ),
                schema::InputType::Enum(enum_info) => normalize_enum_value(
                    schema,
                    namespaced_getter,
                    context,
                    location_type,
                    value,
                    enum_info,
                ),
                schema::InputType::InputObject(input_object) => {
                    let normalized_object = normalize_input_object(
                        schema,
                        namespaced_getter,
                        context,
                        location_type,
                        value,
                        input_object,
                    )?;
                    Ok(normalized_object)
                }
            }
        }
        ast::BaseType::List(wrapped_type) => {
            if value.is_null() {
                Ok(normalized::Value::SimpleValue(
                    normalized::SimpleValue::Null,
                ))
            } else if value.is_list() {
                value.fold_list(
                    schema,
                    namespaced_getter,
                    context,
                    location_type,
                    |mut l, v| {
                        // when normalizing list values, we don't want to coerce values as lists
                        // i.e, `[1,2]` isn't a valid value for `[[Int]]` but `1` is a valid value for `[Int]`
                        if wrapped_type.is_list() && !v.is_list() {
                            // raise an error
                            Err(Error::IncorrectFormat {
                                expected_type: "LIST",
                                actual_type: value.kind(),
                            })
                        } else {
                            l.push(normalize(
                                schema,
                                namespaced_getter,
                                context,
                                v,
                                &LocationType::List {
                                    type_: wrapped_type,
                                },
                                type_info,
                            )?);
                            Ok(l)
                        }
                    },
                )
            } else {
                // get the dimension of the list type
                // [Int] -> 1, [[Int]] -> 2, [[[Int]]] -> 3
                let expected_list_type_dimensions = wrapped_type.list_dimensions() + 1;
                // coerce the value to location_type.base.underlying_type()
                let expected_base_type = location_type.type_().underlying_type_container();
                let normalized_value = normalize(
                    schema,
                    namespaced_getter,
                    context,
                    value,
                    &LocationType::NoLocation {
                        type_: expected_base_type,
                    },
                    type_info,
                )?;
                // Wrap the coerced value into a list, dimension times.
                Ok(create_nested_vec(
                    expected_list_type_dimensions,
                    normalized_value,
                ))
            }
        }
    }?;
    if !location_type.type_().nullable && normalized_value.is_null() {
        Err(Error::UnexpectedNull {
            expected_type: location_type.type_().clone(),
        })
    } else {
        Ok(normalized_value)
    }
}

fn create_nested_vec<S: schema::SchemaContext>(
    dimension: usize,
    value: normalized::Value<'_, S>,
) -> normalized::Value<'_, S> {
    let mut current = value;
    for _ in 0..dimension {
        current = normalized::Value::List(vec![current]);
    }
    current
}

fn normalize_enum_value<
    'q,
    's,
    S: schema::SchemaContext,
    V: ValueSource<'q, 's, S>,
    NSGet: schema::NamespacedGetter<S>,
>(
    schema: &'s schema::Schema<S>,
    namespaced_getter: &NSGet,
    context: &V::Context,
    location_type: &LocationType<'q, 's>,
    value: &V,
    enum_info: &'s schema::Enum<S>,
) -> Result<normalized::Value<'s, S>> {
    value.fold_enum(
        schema,
        namespaced_getter,
        context,
        location_type,
        |raw_enum_value| {
            let (enum_info, namespaced) = namespaced_getter
                .get(enum_info.values.get(raw_enum_value).ok_or_else(|| {
                    Error::EnumValueNotFound {
                        type_name: enum_info.name.clone(),
                        enum_value: raw_enum_value.clone(),
                    }
                })?)
                .ok_or_else(|| Error::EnumValueNotFound {
                    type_name: enum_info.name.clone(),
                    enum_value: raw_enum_value.clone(),
                })?;

            Ok(normalized::Value::SimpleValue(
                normalized::SimpleValue::Enum(normalized::EnumValue {
                    name: raw_enum_value.clone(),
                    info: schema::NodeInfo {
                        generic: &enum_info.info,
                        namespaced,
                    },
                }),
            ))
        },
    )
}

fn normalize_input_object<
    'q,
    's,
    S: schema::SchemaContext,
    V: ValueSource<'q, 's, S>,
    NSGet: schema::NamespacedGetter<S>,
>(
    schema: &'s schema::Schema<S>,
    namespaced_getter: &NSGet,
    context: &V::Context,
    location_type: &LocationType<'q, 's>,
    value: &V,
    input_object: &'s schema::InputObject<S>,
) -> Result<normalized::Value<'s, S>>
where
    's: 'q,
{
    // let (normalized_object, required_field_count) = value.fold_key_values(
    let normalized_object =
        value.fold_key_values(
            schema,
            namespaced_getter,
            context,
            location_type,
            // (IndexMap::new(), 0),
            |mut normalized_object, field, field_value| {
                // |(mut normalized_object, mut required_field_count), field, field_value| {
                let (field_info, namespaced) = namespaced_getter
                    .get(input_object.fields.get(field).ok_or_else(|| {
                        Error::InputFieldNotFound {
                            type_name: input_object.name.clone(),
                            field_name: field.clone(),
                        }
                    })?)
                    .ok_or_else(|| Error::InputFieldNotFound {
                        type_name: input_object.name.clone(),
                        field_name: field.clone(),
                    })?;

                // Get the type of the field, we need this to normalize the field value
                let field_type = &field_info.field_type;
                let field_type_info = {
                    let field_type_info = schema
                        .types
                        .get(field_type.underlying_type())
                        .ok_or_else(|| Error::InternalTypeNotFound {
                            type_name: field_type.underlying_type().clone(),
                        })?;
                    field_type_info
                        .as_input_type()
                        .ok_or_else(|| Error::InternalNotInputType {
                            type_name: field_type.underlying_type().clone(),
                            actual_type: field_type_info.kind(),
                        })?
                };

                // normalize the RHS of the field
                let normalized_field_value = normalize(
                    schema,
                    namespaced_getter,
                    context,
                    field_value,
                    // TODO, this has to change to field info
                    &LocationType::Field {
                        type_: &field_info.field_type,
                        default_value: field_info.default_value.as_ref(),
                    },
                    &field_type_info,
                )?;

                let normalized_input_field = normalized::InputField {
                    name: field.clone(),
                    info: schema::NodeInfo {
                        generic: &field_info.info,
                        namespaced,
                    },
                    value: normalized_field_value,
                };

                // if the field already exists, we throw an error
                // (insert returns a reference to previous value)
                if normalized_object
                    .insert(field.clone(), normalized_input_field)
                    .is_some()
                {
                    return Err(Error::DuplicateInputFields {
                        type_name: field_type.underlying_type().clone(),
                        field: field.clone(),
                    });
                }

                // count all non-nullable fields
                // if !field_info.data.field_type.nullable {
                //     required_field_count += 1;
                // };
                // Ok((normalized_object, required_field_count))
                Ok(normalized_object)
            },
        )?;

    // all the non-nullable fields MUST be present
    // if required_field_count != input_object.required_field_count() {
    //     let required_fields = input_object
    //         .fields
    //         .values()
    //         .filter_map(|field| {
    //             if field.data.field_type.nullable {
    //                 None
    //             } else {
    //                 Some(&field.data.name)
    //             }
    //         })
    //         .collect::<HashSet<&ast::Name>>();
    //     let object_keys = normalized_object.keys().collect::<HashSet<&ast::Name>>();
    //     let missing_fields = required_fields
    //         .difference(&object_keys)
    //         .copied()
    //         .cloned()
    //         .collect::<Vec<ast::Name>>();
    //     return Err(Error::RequiredInputFieldsNotFound {
    //         type_name: input_object.name.clone(),
    //         field_names: missing_fields,
    //     });
    // } else {
    //     Ok(normalized_object)
    // }
    Ok(normalized_object)
}

#[cfg(test)]
mod test {
    use crate::parser::Parser;
    use crate::schema::Schema;
    use crate::schema::sdl::SDL;
    use crate::validation::input::source::LocationType::List;
    use crate::{
        ast::{common as ast, value},
        mk_name, normalized_ast,
        schema::{InputType, Scalar, sdl},
        validation::input::normalize,
    };

    fn fake_schema() -> Schema<SDL> {
        sdl::SDL::new("type Query {foo: Int}")
            .and_then(|v| v.build_schema())
            .unwrap()
    }

    fn list_int() -> ast::TypeContainer<ast::TypeName> {
        let int_typename = ast::TypeName(mk_name!("Int"));
        ast::TypeContainer {
            nullable: true,
            base: ast::BaseTypeContainer::List(Box::new(ast::TypeContainer {
                nullable: true,
                base: ast::BaseTypeContainer::Named(int_typename),
            })),
        }
    }

    fn list_list_int() -> ast::TypeContainer<ast::TypeName> {
        ast::TypeContainer {
            nullable: true,
            base: ast::BaseTypeContainer::List(Box::new(list_int())),
        }
    }

    // Test the examples from https://spec.graphql.org/October2021/#sec-List.Input-Coercion
    #[test]
    fn test_input_coercion_for_list_example_1() {
        // Test if foo: [Int] = [1,2,3] is coerced to [1,2,3]
        let int_typename = ast::TypeName(mk_name!("Int"));
        let int_scalar = Scalar {
            name: int_typename,
            description: None,
            directives: vec![],
        };
        let int_scalar_type_info = InputType::Scalar(&int_scalar);
        let schema = fake_schema();
        let value = Parser::new("[1,2,3]").parse_const_value().unwrap().item;
        let location_type = List { type_: &list_int() };
        let expected_value = normalized_ast::Value::List(vec![
            normalized_ast::Value::SimpleValue(normalized_ast::SimpleValue::Integer(1)),
            normalized_ast::Value::SimpleValue(normalized_ast::SimpleValue::Integer(2)),
            normalized_ast::Value::SimpleValue(normalized_ast::SimpleValue::Integer(3)),
        ]);
        let normalized_value = normalize::normalize(
            &schema,
            &sdl::SDLNamespacedGetter(),
            &(),
            &value,
            &location_type,
            &int_scalar_type_info,
        )
        .unwrap();
        assert_eq!(normalized_value, expected_value);
    }

    #[test]
    fn test_input_coercion_for_list_example_2() {
        // Test if foo: [Int] = 1 is coerced to [1]
        let int_typename = ast::TypeName(mk_name!("Int"));
        let int_scalar = Scalar {
            name: int_typename,
            description: None,
            directives: vec![],
        };
        let int_scalar_type_info = InputType::Scalar(&int_scalar);
        let schema = fake_schema();
        let value = value::ConstValue::SimpleValue(value::SimpleValue::Integer(1));
        let location_type = List { type_: &list_int() };
        let expected_value = normalized_ast::Value::List(vec![normalized_ast::Value::SimpleValue(
            normalized_ast::SimpleValue::Integer(1),
        )]);
        let normalized_value = normalize::normalize(
            &schema,
            &sdl::SDLNamespacedGetter(),
            &(),
            &value,
            &location_type,
            &int_scalar_type_info,
        )
        .unwrap();
        assert_eq!(normalized_value, expected_value);
    }

    #[test]
    fn test_input_coercion_for_list_example_3() {
        // Test if foo: [Int] = null is coerced to null
        let int_typename = ast::TypeName(mk_name!("Int"));
        let int_scalar = Scalar {
            name: int_typename,
            description: None,
            directives: vec![],
        };
        let int_scalar_type_info = InputType::Scalar(&int_scalar);
        let schema = fake_schema();
        let value = value::ConstValue::SimpleValue(value::SimpleValue::Null);
        let location_type = List { type_: &list_int() };
        let expected_value = normalized_ast::Value::SimpleValue(normalized_ast::SimpleValue::Null);
        let normalized_value = normalize::normalize(
            &schema,
            &sdl::SDLNamespacedGetter(),
            &(),
            &value,
            &location_type,
            &int_scalar_type_info,
        )
        .unwrap();
        assert_eq!(normalized_value, expected_value);
    }

    #[test]
    fn test_input_coercion_for_list_example_4() {
        // Test if foo: [Int] = [1, "b", true] is not coerced
        let int_typename = ast::TypeName(mk_name!("Int"));
        let int_scalar = Scalar {
            name: int_typename,
            description: None,
            directives: vec![],
        };
        let int_scalar_type_info = InputType::Scalar(&int_scalar);
        let schema = fake_schema();
        let value = Parser::new("[1, \"b\", true]")
            .parse_const_value()
            .unwrap()
            .item;
        let location_type = List { type_: &list_int() };
        let normalized_value = normalize::normalize(
            &schema,
            &sdl::SDLNamespacedGetter(),
            &(),
            &value,
            &location_type,
            &int_scalar_type_info,
        );
        assert!(normalized_value.is_err());
    }

    #[test]
    fn test_input_coercion_for_list_example_5() {
        // Test if foo: [[Int]] = 1 is coerced to [[1]]
        let int_typename = ast::TypeName(mk_name!("Int"));
        let int_scalar = Scalar {
            name: int_typename,
            description: None,
            directives: vec![],
        };
        let int_scalar_type_info = InputType::Scalar(&int_scalar);
        let schema = fake_schema();
        let value = value::ConstValue::SimpleValue(value::SimpleValue::Integer(1));
        let location_type = List {
            type_: &list_list_int(),
        };
        let expected_value = normalized_ast::Value::List(vec![normalized_ast::Value::List(vec![
            normalized_ast::Value::SimpleValue(normalized_ast::SimpleValue::Integer(1)),
        ])]);
        let normalized_value = normalize::normalize(
            &schema,
            &sdl::SDLNamespacedGetter(),
            &(),
            &value,
            &location_type,
            &int_scalar_type_info,
        )
        .unwrap();
        assert_eq!(normalized_value, expected_value);
    }

    #[test]
    fn test_input_coercion_for_list_example_6() {
        // Test if foo: [[Int]] = [1,2,3] is not coerced
        let int_typename = ast::TypeName(mk_name!("Int"));
        let int_scalar = Scalar {
            name: int_typename,
            description: None,
            directives: vec![],
        };
        let int_scalar_type_info = InputType::Scalar(&int_scalar);
        let schema = fake_schema();
        let value = Parser::new("[1,2,3]").parse_const_value().unwrap().item;
        let location_type = List {
            type_: &list_list_int(),
        };
        let normalized_value = normalize::normalize(
            &schema,
            &sdl::SDLNamespacedGetter(),
            &(),
            &value,
            &location_type,
            &int_scalar_type_info,
        );
        assert!(normalized_value.is_err());
    }

    #[test]
    fn test_input_coercion_for_list_example_7() {
        // Test if foo: [[Int]] = null is coerced to null
        let int_typename = ast::TypeName(mk_name!("Int"));
        let int_scalar = Scalar {
            name: int_typename,
            description: None,
            directives: vec![],
        };
        let int_scalar_type_info = InputType::Scalar(&int_scalar);
        let schema = fake_schema();
        let value = value::ConstValue::SimpleValue(value::SimpleValue::Null);
        let location_type = List {
            type_: &list_list_int(),
        };
        let expected_value = normalized_ast::Value::SimpleValue(normalized_ast::SimpleValue::Null);
        let normalized_value = normalize::normalize(
            &schema,
            &sdl::SDLNamespacedGetter(),
            &(),
            &value,
            &location_type,
            &int_scalar_type_info,
        )
        .unwrap();
        assert_eq!(normalized_value, expected_value);
    }

    #[test]
    fn test_input_coercion_for_list_example_8() {
        // Test if foo: [[Int]] = [[1], [2, 3]] is coerced to [[1], [2, 3]]
        let int_typename = ast::TypeName(mk_name!("Int"));
        let int_scalar = Scalar {
            name: int_typename,
            description: None,
            directives: vec![],
        };
        let int_scalar_type_info = InputType::Scalar(&int_scalar);
        let schema = fake_schema();
        let value = Parser::new("[[1], [2, 3]]")
            .parse_const_value()
            .unwrap()
            .item;
        let location_type = List {
            type_: &list_list_int(),
        };
        let expected_value = normalized_ast::Value::List(vec![
            normalized_ast::Value::List(vec![normalized_ast::Value::SimpleValue(
                normalized_ast::SimpleValue::Integer(1),
            )]),
            normalized_ast::Value::List(vec![
                normalized_ast::Value::SimpleValue(normalized_ast::SimpleValue::Integer(2)),
                normalized_ast::Value::SimpleValue(normalized_ast::SimpleValue::Integer(3)),
            ]),
        ]);
        let normalized_value = normalize::normalize(
            &schema,
            &sdl::SDLNamespacedGetter(),
            &(),
            &value,
            &location_type,
            &int_scalar_type_info,
        )
        .unwrap();
        assert_eq!(normalized_value, expected_value);
    }
}
