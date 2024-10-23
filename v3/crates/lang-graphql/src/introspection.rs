// pub mod schema;

use std::collections::HashSet;

use crate::ast::common as ast;
use crate::ast::common::TypeName;
use crate::mk_name;
use crate::normalized_ast as normalized;
use crate::schema;
use crate::schema::RegisteredTypeName;

use indexmap::IndexMap;
use serde_json as json;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("internal introspection error: normalized introspection ast not as expected: {0}")]
    InternalNormalizationError(normalized::Error),
    #[error("invalid graphql name: {0}")]
    InvalidGraphQlName(String),
    #[error("internal introspection error: {0}")]
    Internal(String),
}

impl From<ast::InvalidGraphQlName> for Error {
    fn from(error: ast::InvalidGraphQlName) -> Self {
        Error::InvalidGraphQlName(error.0)
    }
}

impl From<normalized::Error> for Error {
    fn from(error: normalized::Error) -> Self {
        Error::InternalNormalizationError(error)
    }
}

impl From<json::Error> for Error {
    fn from(value: json::Error) -> Self {
        Self::Internal(value.to_string())
    }
}

pub type Result<T> = core::result::Result<T, Error>;

fn object_response<B>(response: Result<B>) -> Result<json::Value>
where
    B: serde::Serialize,
{
    Ok(json::to_value(response?)?)
}

/// for l map f, returning a Value::Array
fn array_response<A, F>(l: &[A], f: F) -> Result<json::Value>
where
    F: Fn(&A) -> Result<IndexMap<ast::Alias, json::Value>>,
{
    l.iter()
        .map(f)
        .collect::<Result<Vec<_>>>()
        // vec_alias_map_to_value() for effeciency vs. to_value()
        .map(json_ext::vec_alias_map_to_value)
}

pub fn named_type<'s, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    schema: &'s schema::Schema<S>,
    namespaced_getter: &NSGet,
    type_: &'s schema::TypeInfo<S>,
    selection_set: &normalized::SelectionSet<'s, S>,
) -> Result<IndexMap<ast::Alias, json::Value>> {
    match type_ {
        schema::TypeInfo::Scalar(scalar) => scalar_type(scalar, selection_set),
        schema::TypeInfo::Enum(enum_) => enum_type(namespaced_getter, enum_, selection_set),
        schema::TypeInfo::Object(object) => {
            object_type(namespaced_getter, schema, object, selection_set)
        }
        schema::TypeInfo::Interface(interface) => {
            interface_type(namespaced_getter, schema, interface, selection_set)
        }
        schema::TypeInfo::Union(union) => {
            union_type(namespaced_getter, schema, union, selection_set)
        }
        schema::TypeInfo::InputObject(input_object) => {
            input_object_type(namespaced_getter, schema, input_object, selection_set)
        }
    }
}

pub fn schema_type<'s, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    schema: &'s schema::Schema<S>,
    namespaced_getter: &NSGet,
    selection_set: &normalized::SelectionSet<'s, S>,
) -> Result<IndexMap<ast::Alias, json::Value>> {
    selection_set.as_object_selection_set(|_type_name, field, field_call| {
        match field_call.name.as_str() {
            // "description" => Ok(json::to_value(&scalar.description)?),
            "types" => {
                // we publish only those types that are reachable for the
                // namespace
                let mut accessible_types: HashSet<TypeName> = HashSet::new();
                collect_accessible_types(namespaced_getter, schema, &mut accessible_types);

                let mut publishable_types = schema
                    .types
                    .values()
                    // TODO: (anon) better way to filter schema types and in-built graphql types?
                    .filter(|x| {
                        accessible_types.contains(x.name())
                            || is_inbuilt_type(x)
                            || is_schema_type(x)
                    })
                    .collect::<Vec<_>>();

                // Careful: https://stackoverflow.com/questions/47121985/why-cant-i-use-a-key-function-that-returns-a-reference-when-sorting-a-vector-wi
                publishable_types.sort_by(|t1, t2| TypeName::cmp(t1.name(), t2.name()));
                array_response(&publishable_types, |type_| {
                    named_type(schema, namespaced_getter, type_, &field.selection_set)
                })
            }
            "queryType" => object_response(named_type_lookup_internal(
                schema,
                namespaced_getter,
                &schema.query_type,
                &field.selection_set,
            )),
            "mutationType" => schema.mutation_type.as_ref().map_or_else(
                || Ok(json::Value::Null),
                |type_name| {
                    object_response(named_type_lookup_internal(
                        schema,
                        namespaced_getter,
                        type_name,
                        &field.selection_set,
                    ))
                },
            ),
            "subscriptionType" => schema.subscription_type.as_ref().map_or_else(
                || Ok(json::Value::Null),
                |type_name| {
                    object_response(named_type_lookup_internal(
                        schema,
                        namespaced_getter,
                        type_name,
                        &field.selection_set,
                    ))
                },
            ),
            // TODO
            "directives" => Ok(json::Value::Array(vec![])),
            _ => Ok(json::Value::Null),
        }
    })
}

// the type_name should be present in schema
fn named_type_lookup_internal<'s, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    schema: &'s schema::Schema<S>,
    namespaced_getter: &NSGet,
    type_name: &ast::TypeName,
    selection_set: &normalized::SelectionSet<'s, S>,
) -> Result<IndexMap<ast::Alias, json::Value>> {
    let type_info = schema.get_type(type_name).ok_or_else(|| {
        Error::Internal(format!("expected type not found in schema: {type_name}"))
    })?;
    named_type(schema, namespaced_getter, type_info, selection_set)
}

fn scalar_type<'s, S: schema::SchemaContext>(
    scalar: &'s schema::Scalar,
    selection_set: &normalized::SelectionSet<'s, S>,
) -> Result<IndexMap<ast::Alias, json::Value>> {
    selection_set.as_object_selection_set(|_type_name, _field, field_call| {
        match field_call.name.as_str() {
            "kind" => Ok(json::to_value("SCALAR")?),
            "name" => Ok(json::to_value(&scalar.name)?),
            "description" => Ok(json::to_value(&scalar.description)?),
            // TODO: "specifiedByURL"
            _ => Ok(json::Value::Null),
        }
    })
}

fn enum_type<'s, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    namespaced_getter: &NSGet,
    enum_: &'s schema::Enum<S>,
    selection_set: &normalized::SelectionSet<'s, S>,
) -> Result<IndexMap<ast::Alias, json::Value>> {
    selection_set.as_object_selection_set(|_type_name, field, field_call| {
        match field_call.name.as_str() {
            "kind" => Ok(json::to_value("ENUM")?),
            "name" => Ok(json::to_value(&enum_.name)?),
            "description" => Ok(json::to_value(&enum_.description)?),
            "enumValues" => {
                let include_deprecated_name = mk_name!("includeDeprecated");
                let include_deprecated = field_call
                    .expected_argument(&include_deprecated_name)?
                    .value
                    .as_boolean()?;
                let mut allowed_values = enum_
                    .values
                    .values()
                    .filter_map(|namespaced_enum_value| {
                        namespaced_getter
                            .get(namespaced_enum_value)
                            .map(|v| v.0)
                            .filter(|enum_value| {
                                let is_deprecated = enum_value.deprecation_status.is_deprecated();
                                !is_deprecated || include_deprecated
                            })
                    })
                    .collect::<Vec<_>>();
                allowed_values.sort_by(|v1, v2| v1.value.cmp(&v2.value));
                array_response(&allowed_values, |value| {
                    enum_value(value, &field.selection_set)
                })
            }
            _ => Ok(json::Value::Null),
        }
    })
}

fn object_type<'s, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    namespaced_getter: &NSGet,
    schema: &'s schema::Schema<S>,
    object: &'s schema::Object<S>,
    selection_set: &normalized::SelectionSet<'s, S>,
) -> Result<IndexMap<ast::Alias, json::Value>> {
    selection_set.as_object_selection_set(|_type_name, field, field_call| {
        match field_call.name.as_str() {
            "kind" => Ok(json::to_value("OBJECT")?),
            "name" => Ok(json::to_value(&object.name)?),
            "description" => Ok(json::to_value(&object.description)?),
            "fields" => {
                let include_deprecated_name = mk_name!("includeDeprecated");
                let include_deprecated = field_call
                    .expected_argument(&include_deprecated_name)?
                    .value
                    .as_boolean()?;
                let mut allowed_fields = object
                    .fields
                    .values()
                    .filter_map(|namespaced_field| {
                        namespaced_getter
                            .get(namespaced_field)
                            .map(|v| v.0)
                            .filter(|field| {
                                let is_field_deprecated = field.deprecation_status.is_deprecated();
                                (!is_field_deprecated || include_deprecated)
                                    && !field.name.as_str().starts_with("__")
                            })
                    })
                    .collect::<Vec<_>>();
                // can't create dummy field inside the if-block, as it won't
                // live long enough
                let no_fields_accessible_name = mk_name!("_no_fields_accessible");
                let dummy_field = schema::Field::new(
                    no_fields_accessible_name,
                    None,
                    S::introspection_node(),
                    ast::TypeContainer::named_null(RegisteredTypeName::string()),
                    std::collections::BTreeMap::new(),
                    schema::DeprecationStatus::NotDeprecated,
                );
                if allowed_fields.is_empty() {
                    allowed_fields.push(&dummy_field);
                }
                allowed_fields.sort_by(|f1, f2| f1.name.cmp(&f2.name));
                array_response(&allowed_fields, |field_info| {
                    object_field(namespaced_getter, schema, field_info, &field.selection_set)
                })
            }
            "interfaces" => {
                let mut interfaces = object
                    .interfaces
                    .iter()
                    .filter_map(|(interface, namespaced)| {
                        namespaced_getter.get(namespaced).and(Some(interface))
                    })
                    .collect::<Vec<_>>();
                interfaces.sort_by(|i1, i2| TypeName::cmp(i1, i2));
                array_response(&interfaces, |interface| {
                    named_type_lookup_internal(
                        schema,
                        namespaced_getter,
                        interface,
                        &field.selection_set,
                    )
                })
            }
            _ => Ok(json::Value::Null),
        }
    })
}

fn interface_type<'s, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    namespaced_getter: &NSGet,
    schema: &'s schema::Schema<S>,
    interface: &'s schema::Interface<S>,
    selection_set: &normalized::SelectionSet<'s, S>,
) -> Result<IndexMap<ast::Alias, json::Value>> {
    selection_set.as_object_selection_set(|_type_name, field, field_call| {
        match field_call.name.as_str() {
            "kind" => Ok(json::to_value("INTERFACE")?),
            "name" => Ok(json::to_value(&interface.name)?),
            "description" => Ok(json::to_value(&interface.description)?),
            "fields" => {
                let include_deprecated_name = mk_name!("includeDeprecated");
                let include_deprecated = field_call
                    .expected_argument(&include_deprecated_name)?
                    .value
                    .as_boolean()?;
                let mut allowed_fields = interface
                    .fields
                    .values()
                    .filter_map(|namespaced_field| {
                        namespaced_getter
                            .get(namespaced_field)
                            .map(|v| v.0)
                            .filter(|field| {
                                let is_field_deprecated = field.deprecation_status.is_deprecated();
                                (!is_field_deprecated || include_deprecated)
                                    && !field.name.as_str().starts_with("__")
                            })
                    })
                    .collect::<Vec<_>>();
                allowed_fields.sort_by(|f1, f2| f1.name.cmp(&f2.name));
                array_response(&allowed_fields, |field_info| {
                    object_field(namespaced_getter, schema, field_info, &field.selection_set)
                })
            }
            "interfaces" => {
                let mut interfaces = interface
                    .interfaces
                    .iter()
                    .filter_map(|(interface, namespaced)| {
                        namespaced_getter.get(namespaced).and(Some(interface))
                    })
                    .collect::<Vec<_>>();
                interfaces.sort_by(|i1, i2| TypeName::cmp(i1, i2));
                array_response(&interfaces, |interface| {
                    named_type_lookup_internal(
                        schema,
                        namespaced_getter,
                        interface,
                        &field.selection_set,
                    )
                })
            }
            "possibleTypes" => {
                let mut possible_types = interface
                    .implemented_by
                    .iter()
                    .filter_map(|(member_type, namespaced)| {
                        namespaced_getter.get(namespaced).and(Some(member_type))
                    })
                    .collect::<Vec<_>>();
                possible_types.sort_by(|t1, t2| t1.0.cmp(&t2.0));
                array_response(&possible_types, |member_type| {
                    named_type_lookup_internal(
                        schema,
                        namespaced_getter,
                        member_type,
                        &field.selection_set,
                    )
                })
            }
            _ => Ok(json::Value::Null),
        }
    })
}

fn union_type<'s, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    namespaced_getter: &NSGet,
    schema: &'s schema::Schema<S>,
    union: &'s schema::Union<S>,
    selection_set: &normalized::SelectionSet<'s, S>,
) -> Result<IndexMap<ast::Alias, json::Value>> {
    selection_set.as_object_selection_set(|_type_name, field, field_call| {
        match field_call.name.as_str() {
            "kind" => Ok(json::to_value("UNION")?),
            "name" => Ok(json::to_value(&union.name)?),
            "description" => Ok(json::to_value(&union.description)?),
            "possibleTypes" => {
                let mut possible_types = union
                    .members
                    .iter()
                    .filter_map(|(member_type, namespaced)| {
                        namespaced_getter.get(namespaced).and(Some(member_type))
                    })
                    .collect::<Vec<_>>();
                possible_types.sort_by(|t1, t2| t1.0.cmp(&t2.0));
                array_response(&possible_types, |member_type| {
                    named_type_lookup_internal(
                        schema,
                        namespaced_getter,
                        member_type,
                        &field.selection_set,
                    )
                })
            }
            _ => Ok(json::Value::Null),
        }
    })
}

fn input_object_type<'s, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    namespaced_getter: &NSGet,
    schema: &'s schema::Schema<S>,
    input_object: &'s schema::InputObject<S>,
    selection_set: &normalized::SelectionSet<'s, S>,
) -> Result<IndexMap<ast::Alias, json::Value>> {
    selection_set.as_object_selection_set(|_type_name, field, field_call| {
        match field_call.name.as_str() {
            "kind" => Ok(json::to_value("INPUT_OBJECT")?),
            "name" => Ok(json::to_value(&input_object.name)?),
            "description" => Ok(json::to_value(&input_object.description)?),
            "inputFields" => {
                let include_deprecated_name = mk_name!("includeDeprecated");
                let include_deprecated = field_call
                    .expected_argument(&include_deprecated_name)?
                    .value
                    .as_boolean()?;
                let mut allowed_fields = input_object
                    .fields
                    .values()
                    .filter_map(|namespaced_input_field| {
                        namespaced_getter
                            .get(namespaced_input_field)
                            .map(|v| v.0)
                            .filter(|input_field| {
                                let is_field_deprecated =
                                    input_field.deprecation_status.is_deprecated();
                                !is_field_deprecated || include_deprecated
                            })
                    })
                    .collect::<Vec<_>>();
                // can't create dummy field inside the if-block, as it won't
                // live long enough
                let no_fields_accessible_name = mk_name!("_no_fields_accessible");
                let dummy_field = schema::InputField::new(
                    no_fields_accessible_name,
                    None,
                    S::introspection_node(),
                    ast::TypeContainer::named_null(RegisteredTypeName::string()),
                    None,
                    schema::DeprecationStatus::NotDeprecated,
                );
                if allowed_fields.is_empty() {
                    allowed_fields.push(&dummy_field);
                }
                allowed_fields.sort_by(|f1, f2| f1.name.cmp(&f2.name));
                array_response(&allowed_fields, |input_field| {
                    input_value(schema, namespaced_getter, input_field, &field.selection_set)
                })
            }
            _ => Ok(json::Value::Null),
        }
    })
}

fn object_field<'s, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    namespaced_getter: &NSGet,
    schema: &'s schema::Schema<S>,
    object_field: &'s schema::Field<S>,
    selection_set: &normalized::SelectionSet<'s, S>,
) -> Result<IndexMap<ast::Alias, json::Value>> {
    selection_set.as_object_selection_set(|_type_name, field, field_call| {
        match field_call.name.as_str() {
            "name" => Ok(json::to_value(&object_field.name)?),
            "description" => Ok(json::to_value(&object_field.description)?),
            "args" => {
                let include_deprecated_name = mk_name!("includeDeprecated");
                let include_deprecated = field_call
                    .expected_argument(&include_deprecated_name)?
                    .value
                    .as_boolean()?;
                let mut allowed_fields = object_field
                    .arguments
                    .values()
                    .filter_map(|namespaced_input_field| {
                        namespaced_getter
                            .get(namespaced_input_field)
                            .map(|v| v.0)
                            .filter(|input_field| {
                                let is_field_deprecated =
                                    input_field.deprecation_status.is_deprecated();
                                !is_field_deprecated || include_deprecated
                            })
                    })
                    .collect::<Vec<_>>();
                allowed_fields.sort_by(|f1, f2| f1.name.cmp(&f2.name));
                array_response(&allowed_fields, |input_field| {
                    input_value(schema, namespaced_getter, input_field, &field.selection_set)
                })
            }
            "type" => object_response(type_(
                schema,
                namespaced_getter,
                &object_field.field_type,
                &field.selection_set,
            )),
            "isDeprecated" => Ok(json::to_value(
                object_field.deprecation_status.is_deprecated(),
            )?),
            "deprecationReason" => Ok(json::to_value(object_field.deprecation_status.reason())?),
            _ => Ok(json::Value::Null),
        }
    })
}

fn input_value<'s, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    schema: &'s schema::Schema<S>,
    namespaced_getter: &NSGet,
    input_value: &'s schema::InputField<S>,
    selection_set: &normalized::SelectionSet<'s, S>,
) -> Result<IndexMap<ast::Alias, json::Value>> {
    selection_set.as_object_selection_set(|_type_name, field, field_call| {
        match field_call.name.as_str() {
            "name" => Ok(json::to_value(&input_value.name)?),
            "description" => Ok(json::to_value(&input_value.description)?),
            "type" => object_response(type_(
                schema,
                namespaced_getter,
                &input_value.field_type,
                &field.selection_set,
            )),
            // TODO: "defaultValue"
            "isDeprecated" => Ok(json::to_value(
                input_value.deprecation_status.is_deprecated(),
            )?),
            "deprecationReason" => Ok(json::to_value(input_value.deprecation_status.reason())?),
            _ => Ok(json::Value::Null),
        }
    })
}

fn enum_value<'s, S: schema::SchemaContext>(
    enum_value: &'s schema::EnumValue<S>,
    selection_set: &normalized::SelectionSet<'s, S>,
) -> Result<IndexMap<ast::Alias, json::Value>> {
    selection_set.as_object_selection_set(|_type_name, _field, field_call| {
        match field_call.name.as_str() {
            "name" => Ok(json::to_value(&enum_value.value)?),
            "description" => Ok(json::to_value(&enum_value.description)?),
            "isDeprecated" => Ok(json::to_value(
                enum_value.deprecation_status.is_deprecated(),
            )?),
            "deprecationReason" => Ok(json::to_value(enum_value.deprecation_status.reason())?),

            _ => Ok(json::Value::Null),
        }
    })
}

fn type_<'s, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    schema: &'s schema::Schema<S>,
    namespaced_getter: &NSGet,
    type_: &'s ast::Type,
    selection_set: &normalized::SelectionSet<'s, S>,
) -> Result<IndexMap<ast::Alias, json::Value>> {
    if type_.nullable {
        base_type(schema, namespaced_getter, &type_.base, selection_set)
    } else {
        selection_set.as_object_selection_set(|_type_name, field, field_call| {
            match field_call.name.as_str() {
                "kind" => Ok(json::to_value("NON_NULL")?),
                "ofType" => object_response(base_type(
                    schema,
                    namespaced_getter,
                    &type_.base,
                    &field.selection_set,
                )),
                _ => Ok(json::Value::Null),
            }
        })
    }
}

fn base_type<'s, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    schema: &'s schema::Schema<S>,
    namespaced_getter: &NSGet,
    base_type: &'s ast::BaseType,
    selection_set: &normalized::SelectionSet<'s, S>,
) -> Result<IndexMap<ast::Alias, json::Value>> {
    match &base_type {
        ast::BaseType::Named(n) => {
            named_type_lookup_internal(schema, namespaced_getter, n, selection_set)
        }
        ast::BaseType::List(l) => {
            selection_set.as_object_selection_set(|_type_name, field, field_call| match field_call
                .name
                .as_str()
            {
                "kind" => Ok(json::to_value("LIST")?),
                "ofType" => {
                    object_response(type_(schema, namespaced_getter, l, &field.selection_set))
                }
                _ => Ok(json::Value::Null),
            })
        }
    }
}

// Helper utilities
fn collect_accessible_types<S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    namespaced_getter: &NSGet,
    schema: &schema::Schema<S>,
    accessible_types: &mut HashSet<TypeName>,
) {
    // insert query root always to accessible types
    // then collect other accessible types
    accessible_types.insert(schema.query_type.clone());
    collect_accessible_types_(
        namespaced_getter,
        schema,
        &schema.query_type,
        accessible_types,
    );

    // insert mutation root always to accessible types if it exists in schema
    // and collect related accessible types
    if let Some(mutation_type) = &schema.mutation_type {
        accessible_types.insert(mutation_type.clone());
        collect_accessible_types_(namespaced_getter, schema, mutation_type, accessible_types);
    }

    // insert subscription root always to accessible types if it exists in schema
    // and collect related accessible types
    if let Some(subscription_type) = &schema.subscription_type {
        accessible_types.insert(subscription_type.clone());
        collect_accessible_types_(
            namespaced_getter,
            schema,
            subscription_type,
            accessible_types,
        );
    }
}

// Recursively collect types available/accessible to a given `Namespace`.
fn collect_accessible_types_<S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    namespaced_getter: &NSGet,
    schema: &schema::Schema<S>,
    current_type_name: &ast::TypeName,
    accessible_types: &mut HashSet<TypeName>,
) {
    let current_type = schema.types.get(current_type_name);
    match current_type {
        Some(schema::TypeInfo::Object(object)) => {
            for namespaced_fields in object.fields.values() {
                if let Some((field, _)) = namespaced_getter.get(namespaced_fields) {
                    for namespaced_input_fields in field.arguments.values() {
                        if let Some((input_field, _)) =
                            namespaced_getter.get(namespaced_input_fields)
                        {
                            let input_field_type_name = input_field.field_type.underlying_type();
                            // If a type isn't recorded yet, then traverse through its fields and collect accessible types
                            if accessible_types.insert(input_field_type_name.clone()) {
                                collect_accessible_types_(
                                    namespaced_getter,
                                    schema,
                                    input_field_type_name,
                                    accessible_types,
                                );
                            }
                        }
                    }
                    let field_type_name = field.field_type.underlying_type();
                    // If a type isn't recorded yet, then traverse through its fields and collect accessible types
                    if accessible_types.insert(field_type_name.clone()) {
                        collect_accessible_types_(
                            namespaced_getter,
                            schema,
                            field_type_name,
                            accessible_types,
                        );
                    }
                }
            }
        }
        Some(schema::TypeInfo::InputObject(input_object)) => {
            for namespaced_fields in input_object.fields.values() {
                if let Some((input_field, _)) = namespaced_getter.get(namespaced_fields) {
                    let input_field_type_name = input_field.field_type.underlying_type();
                    // If a type isn't recorded yet, then traverse through its fields and collect accessible types
                    if accessible_types.insert(input_field_type_name.clone()) {
                        collect_accessible_types_(
                            namespaced_getter,
                            schema,
                            input_field_type_name,
                            accessible_types,
                        );
                    }
                }
            }
        }
        // TODO: shouldn't we pattern match with other type infos?
        _ => {}
    }
}

fn is_inbuilt_type<S: schema::SchemaContext>(type_info: &schema::TypeInfo<S>) -> bool {
    matches!(
        type_info.name().as_str(),
        "Int" | "Float" | "Boolean" | "String" | "ID"
    )
}

fn is_schema_type<S: schema::SchemaContext>(type_info: &schema::TypeInfo<S>) -> bool {
    type_info.name().as_str().starts_with("__")
}
