use indexmap::IndexMap;
use serde_json as json;

use crate::ast::common as ast;
use crate::normalized_ast as normalized;
use crate::schema;
use crate::validation::NonNullGraphqlVariablesValidation;
use crate::validation::error::*;

use super::source::*;

fn json_kind(value: &json::Value) -> &'static str {
    match value {
        json::Value::Null => "NULL",
        json::Value::Bool(_) => "BOOLEAN",
        json::Value::Number(_) => "NUMBER",
        json::Value::String(_) => "STRING",
        json::Value::Array(_) => "ARRAY",
        json::Value::Object(_) => "OBJECT",
    }
}

impl<'q, 's, S: schema::SchemaContext> ValueSource<'q, 's, S> for json::Value {
    type Context = ();

    fn as_json<NSGet: schema::NamespacedGetter<S>>(
        &self,
        _schema: &'s schema::Schema<S>,
        _namespaced_getter: &NSGet,
        _context: &Self::Context,
        _location_type: &LocationType<'q, 's>,
        _validate_non_null_graphql_variables: &NonNullGraphqlVariablesValidation,
    ) -> Result<json::Value> {
        Ok(self.clone())
    }

    fn fold_enum<F, NSGet: schema::NamespacedGetter<S>>(
        &self,
        _schema: &'s schema::Schema<S>,
        _namespaced_getter: &NSGet,
        _context: &Self::Context,
        _location_type: &LocationType<'q, 's>,
        _validate_non_null_graphql_variables: &NonNullGraphqlVariablesValidation,

        f: F,
    ) -> Result<normalized::Value<'s, S>>
    where
        F: Fn(&ast::Name) -> Result<normalized::Value<'s, S>>,
    {
        let str = self.as_str().ok_or_else(|| Error::IncorrectFormat {
            expected_type: "STRING",
            actual_type: json_kind(self),
        })?;
        let name = str.parse().map_err(|_| Error::NotAValidName {
            str: str.to_owned(),
        })?;
        f(&name)
    }

    fn get_integer<NSGet: schema::NamespacedGetter<S>>(
        &self,
        _schema: &'s schema::Schema<S>,
        _namespaced_getter: &NSGet,
        _context: &Self::Context,
        _location_type: &LocationType<'q, 's>,
        _validate_non_null_graphql_variables: &NonNullGraphqlVariablesValidation,
    ) -> Result<normalized::Value<'s, S>> {
        self.as_i64()
            .ok_or_else(|| Error::IncorrectFormat {
                expected_type: "NUMBER",
                actual_type: json_kind(self),
            })
            .map(|v| normalized::Value::SimpleValue(normalized::SimpleValue::Integer(v)))
    }

    fn get_float<NSGet: schema::NamespacedGetter<S>>(
        &self,
        _schema: &'s schema::Schema<S>,
        _namespaced_getter: &NSGet,
        _context: &Self::Context,
        _location_type: &LocationType<'q, 's>,
        _validate_non_null_graphql_variables: &NonNullGraphqlVariablesValidation,
    ) -> Result<normalized::Value<'s, S>> {
        self.as_f64()
            .ok_or_else(|| Error::IncorrectFormat {
                expected_type: "NUMBER",
                actual_type: json_kind(self),
            })
            .map(|v| normalized::Value::SimpleValue(normalized::SimpleValue::Float(v)))
    }

    fn get_boolean<NSGet: schema::NamespacedGetter<S>>(
        &self,
        _schema: &'s schema::Schema<S>,
        _namespaced_getter: &NSGet,
        _context: &Self::Context,
        _location_type: &LocationType<'q, 's>,
        _validate_non_null_graphql_variables: &NonNullGraphqlVariablesValidation,
    ) -> Result<normalized::Value<'s, S>> {
        self.as_bool()
            .ok_or_else(|| Error::IncorrectFormat {
                expected_type: "BOOLEAN",
                actual_type: json_kind(self),
            })
            .map(|v| normalized::Value::SimpleValue(normalized::SimpleValue::Boolean(v)))
    }

    fn get_string<NSGet: schema::NamespacedGetter<S>>(
        &self,
        _schema: &'s schema::Schema<S>,
        _namespaced_getter: &NSGet,
        _context: &Self::Context,
        _location_type: &LocationType<'q, 's>,
        _validate_non_null_graphql_variables: &NonNullGraphqlVariablesValidation,
    ) -> Result<normalized::Value<'s, S>> {
        self.as_str()
            .ok_or_else(|| Error::IncorrectFormat {
                expected_type: "STRING",
                actual_type: json_kind(self),
            })
            .map(|v| normalized::Value::SimpleValue(normalized::SimpleValue::String(v.to_owned())))
    }

    fn get_id<NSGet: schema::NamespacedGetter<S>>(
        &self,
        _schema: &'s schema::Schema<S>,
        _namespaced_getter: &NSGet,
        _context: &Self::Context,
        _location_type: &LocationType<'q, 's>,
        _validate_non_null_graphql_variables: &NonNullGraphqlVariablesValidation,
    ) -> Result<normalized::Value<'s, S>> {
        self.as_str()
            .ok_or_else(|| Error::IncorrectFormat {
                expected_type: "ID",
                actual_type: json_kind(self),
            })
            .map(|v| normalized::Value::SimpleValue(normalized::SimpleValue::Id(v.to_owned())))
    }

    fn fold_list<F, NSGet: schema::NamespacedGetter<S>>(
        &self,
        _schema: &'s schema::Schema<S>,
        _namespaced_getter: &NSGet,
        _context: &Self::Context,
        _location_type: &LocationType<'q, 's>,
        _validate_non_null_graphql_variables: &NonNullGraphqlVariablesValidation,

        mut f: F,
    ) -> Result<normalized::Value<'s, S>>
    where
        F: FnMut(Vec<normalized::Value<'s, S>>, &Self) -> Result<Vec<normalized::Value<'s, S>>>,
    {
        let mut accum = Vec::new();
        match self.as_array() {
            None => {
                return Err(Error::IncorrectFormat {
                    expected_type: "ARRAY",
                    actual_type: json_kind(self),
                });
            }
            Some(array) => {
                for value in array {
                    accum = f(accum, value)?;
                }
            }
        }
        Ok(normalized::Value::List(accum))
    }

    fn fold_key_values<F, NSGet: schema::NamespacedGetter<S>>(
        &self,
        _schema: &'s schema::Schema<S>,
        _namespaced_getter: &NSGet,
        _context: &Self::Context,
        _location_type: &LocationType<'q, 's>,
        _validate_non_null_graphql_variables: &NonNullGraphqlVariablesValidation,
        f: F,
    ) -> Result<normalized::Value<'s, S>>
    where
        F: Fn(normalized::Object<'s, S>, &ast::Name, &Self) -> Result<normalized::Object<'s, S>>,
    {
        let mut accum = IndexMap::new();
        match self.as_object() {
            None => {
                return Err(Error::IncorrectFormat {
                    expected_type: "OBJECT",
                    actual_type: json_kind(self),
                });
            }
            Some(object) => {
                for (key, value) in object {
                    let key_name = key
                        .parse()
                        .map_err(|_| Error::NotAValidName { str: key.clone() })?;
                    accum = f(accum, &key_name, value)?;
                }
            }
        }
        Ok(normalized::Value::Object(accum))
    }

    fn is_list(&self) -> bool {
        self.is_array()
    }

    fn is_null(&self) -> bool {
        self.is_null()
    }

    fn kind(&self) -> &'static str {
        json_kind(self)
    }
}
