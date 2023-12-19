use crate::ast::common as ast;
use crate::normalized_ast as normalized;
use crate::schema;
use crate::validation::error::*;

use super::source::*;

fn normalize_scalar_value<'q, 's, S: schema::SchemaContext, V: ValueSource<'q, 's, S>>(
    schema: &'s schema::Schema<S>,
    namespace: &S::Namespace,
    context: &V::Context,
    location_type: &LocationType<'q, 's>,
    value: &V,
    scalar: &'s schema::Scalar,
) -> Result<normalized::Value<'s, S>> {
    match scalar.name.as_str() {
        "Int" => value.get_integer(schema, namespace, context, location_type),
        "ID" => value.get_id(schema, namespace, context, location_type),
        "Float" => value.get_float(schema, namespace, context, location_type),
        "Boolean" => value.get_boolean(schema, namespace, context, location_type),
        "String" => value.get_string(schema, namespace, context, location_type),
        _ => value.as_json_normalized(schema, namespace, context, location_type),
    }
}

pub fn normalize<'q, 's, S: schema::SchemaContext, V: ValueSource<'q, 's, S>>(
    schema: &'s schema::Schema<S>,
    namespace: &S::Namespace,
    context: &V::Context,
    value: &V,
    location_type: &LocationType<'q, 's>,
    type_info: &schema::InputType<'s, S>,
) -> Result<normalized::Value<'s, S>>
where
    's: 'q,
{
    let normalized_value = match &location_type.type_().base {
        ast::BaseType::Named(_) => match type_info {
            schema::InputType::Scalar(scalar) => {
                normalize_scalar_value(schema, namespace, context, location_type, value, scalar)
            }
            schema::InputType::Enum(enum_info) => {
                normalize_enum_value(schema, namespace, context, location_type, value, enum_info)
            }
            schema::InputType::InputObject(input_object) => {
                let normalized_object = normalize_input_object(
                    schema,
                    namespace,
                    context,
                    location_type,
                    value,
                    input_object,
                )?;
                // Ok(normalized::Value::Object(normalized_object))
                Ok(normalized_object)
            }
        },
        ast::BaseType::List(wrapped_type) => {
            let normalized_list =
                value.fold_list(schema, namespace, context, location_type, |mut l, v| {
                    l.push(normalize(
                        schema,
                        namespace,
                        context,
                        v,
                        &LocationType::List {
                            type_: wrapped_type,
                        },
                        type_info,
                    )?);
                    Ok(l)
                })?;
            Ok(normalized_list)
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

fn normalize_enum_value<'q, 's, S: schema::SchemaContext, V: ValueSource<'q, 's, S>>(
    schema: &'s schema::Schema<S>,
    namespace: &S::Namespace,
    context: &V::Context,
    location_type: &LocationType<'q, 's>,
    value: &V,
    enum_info: &'s schema::Enum<S>,
) -> Result<normalized::Value<'s, S>> {
    value.fold_enum(
        schema,
        namespace,
        context,
        location_type,
        |raw_enum_value| {
            let (enum_info, namespaced) = enum_info
                .values
                .get(raw_enum_value)
                .ok_or_else(|| Error::EnumValueNotFound {
                    type_name: enum_info.name.clone(),
                    enum_value: raw_enum_value.clone(),
                })?
                .get(namespace)
                .ok_or_else(|| Error::EnumValueNotAccessible {
                    namespace: namespace.to_string(),
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

fn normalize_input_object<'q, 's, S: schema::SchemaContext, V: ValueSource<'q, 's, S>>(
    schema: &'s schema::Schema<S>,
    namespace: &S::Namespace,
    context: &V::Context,
    location_type: &LocationType<'q, 's>,
    value: &V,
    input_object: &'s schema::InputObject<S>,
) -> Result<normalized::Value<'s, S>>
where
    's: 'q,
{
    // let (normalized_object, required_field_count) = value.fold_key_values(
    let normalized_object = value.fold_key_values(
        schema,
        namespace,
        context,
        location_type,
        // (IndexMap::new(), 0),
        |mut normalized_object, field, field_value| {
            // |(mut normalized_object, mut required_field_count), field, field_value| {
            let (field_info, namespaced) = input_object
                .fields
                .get(field)
                .ok_or_else(|| Error::InputFieldNotFound {
                    type_name: input_object.name.clone(),
                    field_name: field.clone(),
                })?
                .get(namespace)
                .ok_or_else(|| Error::InputFieldNotAccessible {
                    namespace: namespace.to_string(),
                    type_name: input_object.name.clone(),
                    field_name: field.clone(),
                })?;

            // Get the type of the field, we need this to normalize the field value
            let field_type = &field_info.field_type;
            let field_type_info = {
                let field_type_info =
                    schema
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
                namespace,
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
