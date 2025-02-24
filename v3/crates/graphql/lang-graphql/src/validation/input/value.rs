use indexmap::IndexMap;

use crate::ast::common as ast;
use crate::ast::value as gql;
use crate::normalized_ast as normalized;
use crate::schema;
use crate::validation::error::*;
use crate::validation::variables;

use super::source::*;

impl<'q, 's, S: schema::SchemaContext> ValueSource<'q, 's, S> for gql::Value
where
    's: 'q,
    S: 's,
{
    type Context = variables::Variables<'q, 's, S>;

    fn as_json<NSGet: schema::NamespacedGetter<S>>(
        &self,
        schema: &'s schema::Schema<S>,
        namespaced_getter: &NSGet,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
    ) -> Result<serde_json::Value> {
        match self {
            Self::Variable(variable) => {
                let value = context
                    .get(location_type, variable, schema, namespaced_getter)?
                    .as_json();
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
            gql::Value::Variable(variable) => {
                context.get(location_type, variable, schema, namespaced_getter)
            }
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
            gql::Value::Variable(variable) => {
                context.get(location_type, variable, schema, namespaced_getter)
            }
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
            gql::Value::Variable(variable) => {
                context.get(location_type, variable, schema, namespaced_getter)
            }
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
            gql::Value::Variable(variable) => {
                context.get(location_type, variable, schema, namespaced_getter)
            }
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
            gql::Value::Variable(variable) => {
                context.get(location_type, variable, schema, namespaced_getter)
            }
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
            gql::Value::Variable(variable) => {
                context.get(location_type, variable, schema, namespaced_getter)
            }
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
            gql::Value::Variable(variable) => {
                context.get(location_type, variable, schema, namespaced_getter)
            }
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
            gql::Value::Variable(variable) => {
                context.get(location_type, variable, schema, namespaced_getter)
            }
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
