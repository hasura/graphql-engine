use crate::ast::common as ast;
use crate::ast::value as gql;
use crate::normalized_ast as normalized;
use crate::schema;
use crate::validation::error::*;

pub enum LocationType<'q, 's> {
    Argument {
        type_: &'q ast::Type,
        default_value: Option<&'s gql::ConstValue>,
    },
    Field {
        type_: &'q ast::Type,
        default_value: Option<&'s gql::ConstValue>,
    },
    List {
        type_: &'q ast::Type,
    },
    NoLocation {
        type_: &'q ast::Type,
    },
}

impl<'q, 's> LocationType<'q, 's> {
    pub fn type_(&self) -> &'q ast::Type {
        match self {
            LocationType::Argument {
                type_,
                default_value: _,
            } => type_,
            LocationType::Field {
                type_,
                default_value: _,
            } => type_,
            LocationType::List { type_ } => type_,
            LocationType::NoLocation { type_ } => type_,
        }
    }
    pub fn default_value(&self) -> Option<&'s gql::ConstValue> {
        match self {
            LocationType::Argument {
                type_: _,
                default_value,
            } => *default_value,
            LocationType::Field {
                type_: _,
                default_value,
            } => *default_value,
            LocationType::List { type_: _ } => None,
            LocationType::NoLocation { type_: _ } => None,
        }
    }
}

pub trait ValueSource<'q, 's, S: schema::SchemaContext>
where
    Self: Sized,
{
    type Context;

    fn get_integer(
        &self,
        schema: &'s schema::Schema<S>,
        namespace: &S::Namespace,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
    ) -> Result<normalized::Value<'s, S>>;
    fn get_float(
        &self,
        schema: &'s schema::Schema<S>,
        namespace: &S::Namespace,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
    ) -> Result<normalized::Value<'s, S>>;
    fn get_boolean(
        &self,
        schema: &'s schema::Schema<S>,
        namespace: &S::Namespace,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
    ) -> Result<normalized::Value<'s, S>>;
    fn get_string(
        &self,
        schema: &'s schema::Schema<S>,
        namespace: &S::Namespace,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
    ) -> Result<normalized::Value<'s, S>>;
    fn get_id(
        &self,
        schema: &'s schema::Schema<S>,
        namespace: &S::Namespace,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
    ) -> Result<normalized::Value<'s, S>>;

    fn fold_enum<F>(
        &self,
        schema: &'s schema::Schema<S>,
        namespace: &S::Namespace,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
        f: F,
    ) -> Result<normalized::Value<'s, S>>
    where
        F: Fn(&ast::Name) -> Result<normalized::Value<'s, S>>;

    fn fold_list<F>(
        &self,
        schema: &'s schema::Schema<S>,
        namespace: &S::Namespace,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
        f: F,
    ) -> Result<normalized::Value<'s, S>>
    where
        F: FnMut(Vec<normalized::Value<'s, S>>, &Self) -> Result<Vec<normalized::Value<'s, S>>>;

    fn fold_key_values<F>(
        &self,
        schema: &'s schema::Schema<S>,
        namespace: &S::Namespace,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
        f: F,
    ) -> Result<normalized::Value<'s, S>>
    where
        F: Fn(normalized::Object<'s, S>, &ast::Name, &Self) -> Result<normalized::Object<'s, S>>;

    fn as_json(
        &self,
        schema: &'s schema::Schema<S>,
        namespace: &S::Namespace,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
    ) -> Result<serde_json::Value>;

    fn as_json_normalized(
        &self,
        schema: &'s schema::Schema<S>,
        namespace: &S::Namespace,
        context: &Self::Context,
        location_type: &LocationType<'q, 's>,
    ) -> Result<normalized::Value<'s, S>> {
        self.as_json(schema, namespace, context, location_type)
            .map(|v| normalized::Value::Json(v))
    }

    fn is_list(&self) -> bool;

    fn is_null(&self) -> bool;

    fn kind(&self) -> &'static str;
}
