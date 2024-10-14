use indexmap::IndexMap;
use serde_json as json;
use std::collections::HashMap;

use crate::ast::common as ast;
use crate::ast::executable;
use crate::ast::value as gql;
use crate::normalized_ast as normalized;
use crate::schema;
use crate::validation::error::*;

use super::normalize::*;
use super::source::*;

#[derive(Clone, Copy)]
pub struct Variables<'q, 's, S: schema::SchemaContext> {
    pub definitions:
        &'q HashMap<&'q ast::Name, (&'q executable::VariableDefinition, schema::InputType<'s, S>)>,
    pub values: &'q HashMap<ast::Name, json::Value>,
}

fn are_base_types_compatible(variable_type: &ast::BaseType, location_type: &ast::BaseType) -> bool {
    match (variable_type, location_type) {
        (ast::BaseType::Named(variable_type_name), ast::BaseType::Named(location_type_name)) => {
            variable_type_name == location_type_name
        }
        (ast::BaseType::List(variable_list_type), ast::BaseType::List(location_list_type)) => {
            are_types_compatible(variable_list_type, location_list_type)
        }
        _ => false,
    }
}

fn are_types_compatible(variable_type: &ast::Type, location_type: &ast::Type) -> bool {
    fn check_nullability(variable_nullability: bool, location_nullability: bool) -> bool {
        location_nullability || !variable_nullability
    }
    are_base_types_compatible(&variable_type.base, &location_type.base)
        && check_nullability(variable_type.nullable, location_type.nullable)
}

enum VariableValue<'q, 's, S: schema::SchemaContext> {
    Json(&'q json::Value, &'q ast::Type, schema::InputType<'s, S>),
    Const(&'q gql::ConstValue, &'q ast::Type, schema::InputType<'s, S>),
    Normalized(&'s gql::ConstValue, &'q ast::Type, schema::InputType<'s, S>),
    None,
}

impl<'q, 's, S: schema::SchemaContext> VariableValue<'q, 's, S> {
    #[allow(clippy::match_same_arms)] // lifetimes are different
    pub fn into_json(self) -> serde_json::Value {
        match self {
            Self::Json(value, _, _) => value.clone(),
            Self::Const(value, _, _) => value.to_json(),
            Self::Normalized(value, _, _) => value.to_json(),
            Self::None => serde_json::Value::Null,
        }
    }

    pub fn normalize<NSGet: schema::NamespacedGetter<S>>(
        &self,
        schema: &'s schema::Schema<S>,
        namespaced_getter: &NSGet,
    ) -> Result<normalized::Value<'s, S>> {
        match self {
            VariableValue::Json(value, type_, type_info) => normalize(
                schema,
                namespaced_getter,
                &(),
                *value,
                &LocationType::NoLocation { type_ },
                type_info,
            ),
            VariableValue::Const(value, type_, type_info)
            | VariableValue::Normalized(value, type_, type_info) => normalize(
                schema,
                namespaced_getter,
                &(),
                *value,
                &LocationType::NoLocation { type_ },
                type_info,
            ),
            VariableValue::None => Ok(normalized::Value::SimpleValue(
                normalized::SimpleValue::Null,
            )),
        }
    }
}

impl<'q, 's, S: schema::SchemaContext> Variables<'q, 's, S> {
    fn get(
        &self,
        location_type: &LocationType<'q, 's>,
        variable: &ast::Name,
    ) -> Result<VariableValue<'q, 's, S>> {
        let variable_definition =
            self.definitions
                .get(variable)
                .ok_or_else(|| Error::VariableNotDefined {
                    variable_name: variable.clone(),
                })?;
        let variable_type = &variable_definition.0.var_type.item;

        // Ideally we would want to call are_types_compatible(location_type, variable_type) but a
        // default value can be provided despite variable_type being nullable through a variable's
        // default value or an argument's default value or a field's default value. So we check
        // for this condition first and then call 'are_types_compatible'
        //
        let variable_spread_allowed = if location_type.default_value().is_some()
            || variable_definition.0.default_value.is_some()
        {
            are_base_types_compatible(&variable_type.base, &location_type.type_().base)
        } else {
            are_types_compatible(variable_type, location_type.type_())
        };

        if variable_spread_allowed {
            // look for the variable value
            if let Some(variable_value) = self.values.get(variable) {
                Ok(VariableValue::Json(
                    variable_value,
                    variable_type,
                    variable_definition.1.clone(),
                ))
            // if not found, take the default value defined for the location
            } else if let Some(variable_default) = &variable_definition.0.default_value {
                Ok(VariableValue::Const(
                    &variable_default.item,
                    variable_type,
                    variable_definition.1.clone(),
                ))
            } else if let Some(location_default) = location_type.default_value() {
                Ok(VariableValue::Normalized(
                    location_default,
                    variable_type,
                    variable_definition.1.clone(),
                ))
            } else {
                Ok(VariableValue::None)
            }
        } else {
            Err(Error::VariableSpreadNotAllowed {
                variable_name: variable.clone(),
                variable_type: variable_type.clone(),
                location_type: location_type.type_().clone(),
            })
        }
    }
}

impl<'q, 's, S: schema::SchemaContext> ValueSource<'q, 's, S> for gql::Value
where
    's: 'q,
    S: 's,
{
    type Context = Variables<'q, 's, S>;

    fn as_json<NSGet: schema::NamespacedGetter<S>>(
        &self,
        schema: &'s schema::Schema<S>,
        namespaced_getter: &NSGet,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
    ) -> Result<serde_json::Value> {
        match self {
            Self::Variable(variable) => {
                let value = context.get(location_type, variable)?.into_json();
                Ok(value)
            }
            Self::SimpleValue(value) => Ok(value.to_json()),
            Self::List(l) => {
                let list = l
                    .iter()
                    .map(|i| {
                        i.item
                            .as_json(schema, namespaced_getter, context, location_type)
                    })
                    .collect::<Result<Vec<serde_json::Value>>>()?;
                Ok(serde_json::Value::Array(list))
            }
            Self::Object(o) => {
                let hash_map = o
                    .iter()
                    .map(|i| {
                        let gql::KeyValue { key, value } = &i.item;
                        Ok((
                            key.item.to_string(),
                            value.item.as_json(
                                schema,
                                namespaced_getter,
                                context,
                                location_type,
                            )?,
                        ))
                    })
                    .collect::<Result<serde_json::Map<String, serde_json::Value>>>()?;
                Ok(serde_json::Value::Object(hash_map))
            }
        }
    }

    fn fold_enum<F, NSGet>(
        &self,
        schema: &'s schema::Schema<S>,
        namespaced_getter: &NSGet,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
        f: F,
    ) -> Result<normalized::Value<'s, S>>
    where
        F: Fn(&ast::Name) -> Result<normalized::Value<'s, S>>,
        NSGet: schema::NamespacedGetter<S>,
    {
        match self {
            gql::Value::Variable(variable) => context
                .get(location_type, variable)?
                .normalize(schema, namespaced_getter),
            gql::Value::SimpleValue(gql::SimpleValue::Enum(e)) => f(e),
            _ => Err(Error::IncorrectFormat {
                expected_type: "ENUM",
                actual_type: self.kind(),
            }),
        }
    }

    fn get_integer<NSGet: schema::NamespacedGetter<S>>(
        &self,
        schema: &'s schema::Schema<S>,
        namespaced_getter: &NSGet,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
    ) -> Result<normalized::Value<'s, S>> {
        match self {
            gql::Value::Variable(variable) => context
                .get(location_type, variable)?
                .normalize(schema, namespaced_getter),
            gql::Value::SimpleValue(gql::SimpleValue::Integer(i)) => Ok(
                normalized::Value::SimpleValue(normalized::SimpleValue::Integer(*i)),
            ),
            _ => Err(Error::IncorrectFormat {
                expected_type: "INTEGER",
                actual_type: self.kind(),
            }),
        }
    }

    fn get_float<NSGet: schema::NamespacedGetter<S>>(
        &self,
        schema: &'s schema::Schema<S>,
        namespaced_getter: &NSGet,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
    ) -> Result<normalized::Value<'s, S>> {
        match self {
            gql::Value::Variable(variable) => context
                .get(location_type, variable)?
                .normalize(schema, namespaced_getter),
            // Both integer and float input values are accepted for Float type.
            // Ref: https://spec.graphql.org/October2021/#sec-Float.Input-Coercion
            gql::Value::SimpleValue(simple_value) => simple_value
                .as_f64()
                .ok_or_else(|| Error::IncorrectFormat {
                    expected_type: "FLOAT",
                    actual_type: self.kind(),
                })
                .map(|v| normalized::Value::SimpleValue(normalized::SimpleValue::Float(v))),
            _ => Err(Error::IncorrectFormat {
                expected_type: "FLOAT",
                actual_type: self.kind(),
            }),
        }
    }

    fn get_boolean<NSGet: schema::NamespacedGetter<S>>(
        &self,
        schema: &'s schema::Schema<S>,
        namespaced_getter: &NSGet,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
    ) -> Result<normalized::Value<'s, S>> {
        match self {
            gql::Value::Variable(variable) => context
                .get(location_type, variable)?
                .normalize(schema, namespaced_getter),
            gql::Value::SimpleValue(gql::SimpleValue::Boolean(b)) => Ok(
                normalized::Value::SimpleValue(normalized::SimpleValue::Boolean(*b)),
            ),
            _ => Err(Error::IncorrectFormat {
                expected_type: "BOOLEAN",
                actual_type: self.kind(),
            }),
        }
    }

    fn get_string<NSGet: schema::NamespacedGetter<S>>(
        &self,
        schema: &'s schema::Schema<S>,
        namespaced_getter: &NSGet,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
    ) -> Result<normalized::Value<'s, S>> {
        match self {
            gql::Value::Variable(variable) => context
                .get(location_type, variable)?
                .normalize(schema, namespaced_getter),
            gql::Value::SimpleValue(gql::SimpleValue::String(s)) => Ok(
                normalized::Value::SimpleValue(normalized::SimpleValue::String(s.clone())),
            ),
            _ => Err(Error::IncorrectFormat {
                expected_type: "STRING",
                actual_type: self.kind(),
            }),
        }
    }

    fn get_id<NSGet: schema::NamespacedGetter<S>>(
        &self,
        schema: &'s schema::Schema<S>,
        namespaced_getter: &NSGet,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
    ) -> Result<normalized::Value<'s, S>> {
        match self {
            gql::Value::Variable(variable) => context
                .get(location_type, variable)?
                .normalize(schema, namespaced_getter),
            gql::Value::SimpleValue(gql::SimpleValue::String(id)) => Ok(
                normalized::Value::SimpleValue(normalized::SimpleValue::Id(id.clone())),
            ),
            _ => Err(Error::IncorrectFormat {
                expected_type: "ID",
                actual_type: self.kind(),
            }),
        }
    }

    fn fold_list<F, NSGet: schema::NamespacedGetter<S>>(
        &self,
        schema: &'s schema::Schema<S>,
        namespaced_getter: &NSGet,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
        mut f: F,
    ) -> Result<normalized::Value<'s, S>>
    where
        F: FnMut(Vec<normalized::Value<'s, S>>, &Self) -> Result<Vec<normalized::Value<'s, S>>>,
    {
        // TODO single element array coercion
        match self {
            gql::Value::Variable(variable) => context
                .get(location_type, variable)?
                .normalize(schema, namespaced_getter),

            gql::Value::List(array) => {
                let mut accum = Vec::new();
                for value in array {
                    accum = f(accum, &value.item)?;
                }
                Ok(normalized::Value::List(accum))
            }
            _ => Err(Error::IncorrectFormat {
                expected_type: "LIST",
                actual_type: self.kind(),
            }),
        }
    }

    fn fold_key_values<F, NSGet: schema::NamespacedGetter<S>>(
        &self,
        schema: &'s schema::Schema<S>,
        namespaced_getter: &NSGet,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
        f: F,
    ) -> Result<normalized::Value<'s, S>>
    where
        F: Fn(normalized::Object<'s, S>, &ast::Name, &Self) -> Result<normalized::Object<'s, S>>,
    {
        match self {
            gql::Value::Variable(variable) => context
                .get(location_type, variable)?
                .normalize(schema, namespaced_getter),
            gql::Value::Object(object) => {
                let mut accum = IndexMap::new();
                for key_value in object {
                    let key_name = &key_value.item.key.item;
                    let value = &key_value.item.value.item;
                    accum = f(accum, key_name, value)?;
                }
                Ok(normalized::Value::Object(accum))
            }
            _ => Err(Error::IncorrectFormat {
                expected_type: "OBJECT",
                actual_type: self.kind(),
            }),
        }
    }

    fn is_list(&self) -> bool {
        match self {
            gql::Value::Variable(_variable) => true,
            gql::Value::List(_array) => true,
            gql::Value::Object(_obj) => false,
            gql::Value::SimpleValue(_simple_value) => false,
        }
    }

    fn is_null(&self) -> bool {
        matches!(self, gql::Value::SimpleValue(gql::SimpleValue::Null))
    }

    fn kind(&self) -> &'static str {
        self.kind()
    }
}
