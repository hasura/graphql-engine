use serde::Serialize;
use serde_json as json;
use serde_with::serde_as;
use std::collections::HashMap;

use crate::ast::common::{self as ast, TypeContainer, TypeName};
use crate::schema::{NodeInfo, SchemaContext};
use indexmap::IndexMap;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    // TODO: uses 'Debug' trait
    #[error("expected argument '{argument_name}' not found on field: {field_call_name}, arguments found: {arguments:?}")]
    ArgumentNotFound {
        field_call_name: ast::Name,
        argument_name: ast::Name,
        arguments: Vec<ast::Name>,
    },
    // TODO: uses 'Debug' trait
    #[error("empty field call not found, fields calls found: {keys:?}")]
    EmptyFieldCallNotFound { keys: Vec<Vec<ast::TypeName>> },

    // Empty typename found for an object selection set
    // TODO: add the selection set to the error?
    #[error("empty type_name found for an object selection set")]
    NoTypenameFound,

    #[error("value not as expected, expected: {expected_kind} but found: {found}")]
    UnexpectedValue {
        expected_kind: &'static str,
        found: json::Value,
    },
}

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Serialize, Clone, Debug, PartialEq)]
pub enum SimpleValue<'s, S: SchemaContext> {
    /// `null`.
    Null,
    /// GraphQL ID
    // TODO: According to https://spec.graphql.org/October2021/#sec-ID.Input-Coercion,
    //  the ID value should also accept an integer value and coerce it into the appropriate
    // format.
    Id(String),
    /// A number.
    Integer(i64),
    /// A float
    Float(f64),
    /// A string.
    String(String),
    /// A boolean.
    Boolean(bool),
    /// An enum. These are typically in `SCREAMING_SNAKE_CASE`.
    Enum(EnumValue<'s, S>),
}

#[derive(Serialize, Clone, Debug, PartialEq)]
pub struct EnumValue<'s, S: SchemaContext> {
    pub name: ast::Name,
    pub info: NodeInfo<'s, S>,
}

#[derive(Serialize, Clone, Debug, PartialEq)]
pub struct InputField<'s, S: SchemaContext> {
    pub name: ast::Name,
    pub info: NodeInfo<'s, S>,
    pub value: Value<'s, S>,
}

pub type Object<'s, S> = IndexMap<ast::Name, InputField<'s, S>>;

#[derive(Serialize, Clone, Debug, PartialEq)]
pub enum Value<'s, S: SchemaContext> {
    /// A leaf value
    SimpleValue(SimpleValue<'s, S>),
    /// A list of values.
    List(Vec<Value<'s, S>>),
    /// An object. This is a map of keys to values.
    Object(Object<'s, S>),
    /// JSON Value
    Json(serde_json::Value),
}

impl<'s, S: SchemaContext> Value<'s, S> {
    pub fn as_json(&self) -> serde_json::Value {
        match self {
            Value::SimpleValue(SimpleValue::Null) => serde_json::Value::Null,
            Value::SimpleValue(SimpleValue::Id(id)) => serde_json::Value::from(id.clone()),
            Value::SimpleValue(SimpleValue::Boolean(b)) => serde_json::Value::Bool(*b),
            Value::SimpleValue(SimpleValue::String(s)) => serde_json::Value::from(s.clone()),
            Value::SimpleValue(SimpleValue::Float(f)) => serde_json::Value::from(*f),
            Value::SimpleValue(SimpleValue::Integer(i)) => serde_json::Value::from(*i),
            Value::SimpleValue(SimpleValue::Enum(val)) => {
                serde_json::Value::from(val.name.to_string())
            }
            Value::List(xs) => serde_json::Value::from(
                xs.iter()
                    .map(Value::as_json)
                    .collect::<Vec<serde_json::Value>>(),
            ),
            Value::Object(xs) => serde_json::Value::Object(
                xs.iter()
                    .map(|(name, field)| (name.to_string(), field.value.as_json()))
                    .collect(),
            ),
            Value::Json(j) => j.clone(),
        }
    }

    pub fn as_boolean(&self) -> Result<bool> {
        match self {
            Value::SimpleValue(SimpleValue::Boolean(b)) => Ok(*b),
            _ => Err(Error::UnexpectedValue {
                expected_kind: "BOOLEAN",
                found: self.as_json(),
            }),
        }
    }

    pub fn as_string(&self) -> Result<&str> {
        match self {
            Value::SimpleValue(SimpleValue::String(s)) => Ok(s.as_str()),
            _ => Err(Error::UnexpectedValue {
                expected_kind: "STRING",
                found: self.as_json(),
            }),
        }
    }

    #[allow(clippy::cast_precision_loss)]
    pub fn as_float(&self) -> Result<f64> {
        match self {
            // Both integer and float input values are accepted for Float type.
            // Ref: https://spec.graphql.org/October2021/#sec-Float.Input-Coercion
            Value::SimpleValue(SimpleValue::Float(f)) => Ok(*f),
            Value::SimpleValue(SimpleValue::Integer(i)) => Ok(*i as f64),
            _ => Err(Error::UnexpectedValue {
                expected_kind: "FLOAT",
                found: self.as_json(),
            }),
        }
    }

    pub fn as_int_u32(&self) -> Result<u32> {
        match self {
            Value::SimpleValue(SimpleValue::Integer(i)) => {
                u32::try_from(*i).map_err(|_e| Error::UnexpectedValue {
                    expected_kind: "NON-NEGATIVE 32-bit INT",
                    found: self.as_json(),
                })
            }
            _ => Err(Error::UnexpectedValue {
                expected_kind: "INT",
                found: self.as_json(),
            }),
        }
    }

    pub fn as_int_i64(&self) -> Result<i64> {
        match self {
            Value::SimpleValue(SimpleValue::Integer(i)) => Ok(*i),
            _ => Err(Error::UnexpectedValue {
                expected_kind: "INT",
                found: self.as_json(),
            }),
        }
    }

    pub fn as_object(&self) -> Result<&IndexMap<ast::Name, InputField<'s, S>>> {
        match self {
            Value::Object(fields) => Ok(fields),
            _ => Err(Error::UnexpectedValue {
                expected_kind: "OBJECT",
                found: self.as_json(),
            }),
        }
    }

    pub fn as_list(&self) -> Result<&Vec<Value<'s, S>>> {
        match self {
            Value::List(values) => Ok(values),
            _ => Err(Error::UnexpectedValue {
                expected_kind: "LIST",
                found: self.as_json(),
            }),
        }
    }

    pub fn as_id(&self) -> Result<String> {
        match self {
            Value::SimpleValue(SimpleValue::Id(s)) => Ok(s.to_string()),
            _ => Err(Error::UnexpectedValue {
                expected_kind: "ID",
                found: self.as_json(),
            }),
        }
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Value::SimpleValue(SimpleValue::Null))
    }

    pub fn as_enum(&self) -> Result<&EnumValue<'s, S>> {
        match self {
            Value::SimpleValue(SimpleValue::Enum(val)) => Ok(val),
            _ => Err(Error::UnexpectedValue {
                expected_kind: "ENUM",
                found: self.as_json(),
            }),
        }
    }
}

#[derive(Serialize, Clone, Debug, PartialEq)]
pub struct Directive<'s, S: SchemaContext> {
    pub name: ast::Name,
    pub arguments: IndexMap<ast::Name, InputField<'s, S>>,
}

#[derive(Serialize, Clone, Debug, PartialEq)]
pub struct FieldCall<'s, S: SchemaContext> {
    pub name: ast::Name,
    /// Info
    pub info: NodeInfo<'s, S>,
    /// The arguments to the field, empty if no arguments are provided.
    pub arguments: IndexMap<ast::Name, InputField<'s, S>>,
}

impl<'s, S: SchemaContext> FieldCall<'s, S> {
    pub fn expected_argument(&self, argument: &ast::Name) -> Result<&InputField<'s, S>> {
        self.arguments
            .get(argument)
            .ok_or_else(|| Error::ArgumentNotFound {
                field_call_name: self.name.clone(),
                argument_name: argument.clone(),
                arguments: self.arguments.keys().cloned().collect(),
            })
    }
}

pub type FieldCalls<'s, S> = HashMap<Vec<ast::TypeName>, FieldCall<'s, S>>;

#[serde_as]
#[derive(Serialize, Clone, Debug, PartialEq)]
pub struct Field<'s, S: SchemaContext> {
    pub alias: ast::Alias,
    #[serde_as(as = "Vec<(_, _)>")]
    pub field_calls: FieldCalls<'s, S>,
    pub selection_set: SelectionSet<'s, S>,
    pub type_container: TypeContainer<TypeName>,
}

impl<'s, S: SchemaContext> Field<'s, S> {
    // unless we are dealing with interfaces or unions, there'll only be one 'FieldCall'
    // per field
    pub fn field_call(&self) -> Result<&FieldCall<'s, S>> {
        self.field_calls
            .get(&vec![])
            .ok_or_else(|| Error::EmptyFieldCallNotFound {
                keys: self.field_calls.keys().cloned().collect(),
            })
    }
}

#[derive(Serialize, Clone, Debug, PartialEq)]
pub struct SelectionSet<'s, S: SchemaContext> {
    pub fields: IndexMap<ast::Alias, Field<'s, S>>,
    pub type_name: Option<ast::TypeName>,
}

impl<'s, S: SchemaContext> SelectionSet<'s, S> {
    ///  Filters the field calls of the fields present in the selection set
    ///  that start with the given type name.
    ///
    ///  For example, consider the following selection set.
    ///
    ///   {
    ///      node(id: "eyJ2ZXJza=") {
    ///        ...on Articles { title }
    ///        ...on Authors { first_name }
    ///   }
    ///
    ///  The `title` field's `FieldCalls` will look like `HashMap [Articles] (<title `FieldCall`>)` and
    ///  the `first_name` field's `FieldCalls` will look like `HashMap [Authors] (<first_name `FieldCall`>)`.
    ///
    ///  Calling this method on the above selection set with `Articles` will only retain fields
    ///  whose `FieldCalls` key starts with `Articles`, in this case, only the `title` field will
    ///  be retained and also the `title` field's `FieldCalls` key will now be an empty vector.
    pub fn filter_field_calls_by_typename(&self, type_name: ast::TypeName) -> SelectionSet<'s, S> {
        let mut filtered_selection_set_fields = IndexMap::new();
        for (alias, field) in &self.fields {
            let mut field_calls = HashMap::new();
            let mut should_retain = false;
            for (type_name_path, field_call) in &field.field_calls {
                match type_name_path.as_slice() {
                    [] => {
                        field_calls.insert(vec![], field_call.clone());
                        should_retain = true;
                    }
                    [x, xs @ ..] => {
                        // Only if the given type name matches the
                        // head of the type name list, add it into the
                        // new `field_calls`, otherwise discard the field.
                        if x == &type_name.clone() {
                            field_calls.insert(xs.to_vec(), field_call.clone());
                            should_retain = true;
                        };
                    }
                };
            }

            if should_retain {
                let new_field = Field {
                    alias: alias.clone(),
                    field_calls,
                    selection_set: field
                        .selection_set
                        .filter_field_calls_by_typename(type_name.clone()),
                    type_container: field.type_container.clone(),
                };
                filtered_selection_set_fields.insert(alias.clone(), new_field);
            }
        }
        SelectionSet {
            fields: filtered_selection_set_fields,
            type_name: Some(type_name),
        }
    }

    pub fn as_object_selection_set<E, V, F>(
        &self,
        mut f: F,
    ) -> std::result::Result<IndexMap<ast::Alias, V>, E>
    where
        F: FnMut(&ast::TypeName, &Field<S>, &FieldCall<S>) -> std::result::Result<V, E>,
        E: From<Error>,
    {
        let type_name = self
            .type_name
            .as_ref()
            .ok_or_else(|| Error::NoTypenameFound)?;
        let mut response = IndexMap::new();
        for (alias, field) in &self.fields {
            let field_call = field.field_call()?;
            let field_response = f(type_name, field, field_call)?;
            response.insert(alias.clone(), field_response);
        }
        Ok(response)
    }
}

#[derive(Serialize, Clone, Debug, PartialEq)]
pub struct Operation<'s, S: SchemaContext> {
    pub ty: ast::OperationType,
    pub name: Option<ast::Name>,
    pub directives: IndexMap<ast::Name, Directive<'s, S>>,
    pub selection_set: SelectionSet<'s, S>,
}
