use indexmap::IndexMap;

use crate::ast::common as ast;
use crate::ast::value as gql;
use crate::normalized_ast as normalized;
use crate::schema;
use crate::validation::NonNullGraphqlVariablesValidation;
use crate::validation::error::*;

use super::source::*;

impl<'q, 's, S: schema::SchemaContext> ValueSource<'q, 's, S> for gql::ConstValue {
    type Context = ();

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
        let name = self.as_enum().ok_or_else(|| Error::IncorrectFormat {
            expected_type: "STRING",
            actual_type: self.kind(),
        })?;
        f(name)
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
                expected_type: "INTEGER",
                actual_type: self.kind(),
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
                expected_type: "FLOAT",
                actual_type: self.kind(),
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
                actual_type: self.kind(),
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
                actual_type: self.kind(),
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
        self.as_id()
            .ok_or_else(|| Error::IncorrectFormat {
                expected_type: "ID",
                actual_type: self.kind(),
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
        match self.as_list() {
            None => Err(Error::IncorrectFormat {
                expected_type: "ARRAY",
                actual_type: self.kind(),
            }),
            Some(array) => {
                let mut accum = Vec::new();
                for value in array {
                    accum = f(accum, &value.item)?;
                }
                Ok(normalized::Value::List(accum))
            }
        }
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
        match self.as_object() {
            None => Err(Error::IncorrectFormat {
                expected_type: "OBJECT",
                actual_type: self.kind(),
            }),
            Some(object) => {
                let mut accum = IndexMap::new();
                for key_value in object {
                    let key_name = &key_value.item.key.item;
                    let value = &key_value.item.value.item;
                    accum = f(accum, key_name, value)?;
                }
                Ok(normalized::Value::Object(accum))
            }
        }
    }

    fn as_json<NSGet: schema::NamespacedGetter<S>>(
        &self,
        _schema: &'s schema::Schema<S>,
        _namespaced_getter: &NSGet,
        _context: &Self::Context,
        _location_type: &LocationType<'q, 's>,
        _validate_non_null_graphql_variables: &NonNullGraphqlVariablesValidation,
    ) -> Result<serde_json::Value> {
        Ok(self.to_json())
    }

    fn is_list(&self) -> bool {
        self.as_list().is_some()
    }

    fn is_null(&self) -> bool {
        self.is_null()
    }

    fn kind(&self) -> &'static str {
        self.kind()
    }
}
