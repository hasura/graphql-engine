use schemars::JsonSchema;
use serde::{Deserialize, Deserializer, Serialize};
use smol_str::SmolStr;
use std::fmt::{self, Display, Formatter, Write};
use std::hash::Hash;
use std::str::FromStr;
// use std::ops::Deref;
// use std::borrow::Borrow;

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct InvalidGraphQlName(pub String);

#[derive(Serialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, JsonSchema)]
pub struct Name(SmolStr);

impl Name {
    pub fn get(&self) -> &SmolStr {
        &self.0
    }
    pub fn take(self) -> SmolStr {
        self.0
    }
    pub fn new(s: &str) -> Result<Name, InvalidGraphQlName> {
        Name::from_str(s)
    }
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl FromStr for Name {
    type Err = InvalidGraphQlName;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        if is_valid_graphql_name(s) {
            Ok(Name(SmolStr::new(s)))
        } else {
            Err(InvalidGraphQlName(s.into()))
        }
    }
}

impl<'de> Deserialize<'de> for Name {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        if !is_valid_graphql_name(&s) {
            return Err(serde::de::Error::custom(format!(
                "{s} is not a valid graphql name"
            )));
        }
        Ok(Name(SmolStr::new(&s)))
    }
}

fn match_first(c: char) -> bool {
    c == '_' || c.is_ascii_uppercase() || c.is_ascii_lowercase()
}

fn match_body(c: char) -> bool {
    c == '_' || c.is_ascii_uppercase() || c.is_ascii_lowercase() || c.is_ascii_digit()
}

fn is_valid_graphql_name(text: &str) -> bool {
    if let Some(first) = text.chars().next() {
        let body = &text[first.len_utf8()..];
        match_first(first) && body.chars().all(match_body)
    } else {
        false
    }
}

// Macro to build a valid graphql name
#[macro_export]
macro_rules! mk_name {
    ($name:literal) => {
        $crate::ast::common::Name::new($name).unwrap()
    };
}

// impl Borrow<str> for Name {
//     fn borrow(&self) -> &str {
//         &self.0
//     }
// }

// impl Deref for Name {
//     type Target = str;

//     fn deref(&self) -> &Self::Target {
//         &self.0
//     }
// }

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeName(pub Name);

impl TypeName {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Alias(pub Name);
impl Alias {
    pub fn new(name: Name) -> Alias {
        Alias(name)
    }
}
impl fmt::Display for Alias {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// The type of an operation; `query`, `mutation` or `subscription`.
///
/// [Reference](https://spec.graphql.org/June2018/#OperationType).
#[derive(Serialize, Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum OperationType {
    /// A query.
    Query,
    /// A mutation.
    Mutation,
    /// A subscription.
    Subscription,
}

impl Display for OperationType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Self::Query => "query",
            Self::Mutation => "mutation",
            Self::Subscription => "subscription",
        })
    }
}

/// A GraphQL type, for example `String` or `[String!]!`.
///
/// [Reference](https://spec.graphql.org/June2018/#Type).
#[derive(Serialize, Deserialize, Hash, Debug, PartialEq, Eq, Clone)]
pub struct TypeContainer<T> {
    /// The base type.
    pub base: BaseTypeContainer<T>,
    /// Whether the type is nullable.
    pub nullable: bool,
}

pub type Type = TypeContainer<TypeName>;

impl<T> TypeContainer<T> {
    pub fn named_non_null(named: T) -> TypeContainer<T> {
        TypeContainer {
            base: BaseTypeContainer::Named(named),
            nullable: false,
        }
    }
    pub fn named_null(named: T) -> TypeContainer<T> {
        TypeContainer {
            base: BaseTypeContainer::Named(named),
            nullable: true,
        }
    }
    pub fn list_null(element_type: TypeContainer<T>) -> TypeContainer<T> {
        TypeContainer {
            base: BaseTypeContainer::List(Box::new(element_type)),
            nullable: true,
        }
    }
    pub fn list_non_null(element_type: TypeContainer<T>) -> TypeContainer<T> {
        TypeContainer {
            base: BaseTypeContainer::List(Box::new(element_type)),
            nullable: false,
        }
    }
    pub fn underlying_type(&self) -> &T {
        match &self.base {
            BaseTypeContainer::Named(n) => n,
            BaseTypeContainer::List(ty) => ty.underlying_type(),
        }
    }

    pub fn underlying_type_container(&self) -> &TypeContainer<T> {
        match &self.base {
            BaseTypeContainer::Named(_) => self,
            BaseTypeContainer::List(ty) => ty.underlying_type_container(),
        }
    }

    pub fn is_list(&self) -> bool {
        match &self.base {
            BaseTypeContainer::Named(_) => false,
            BaseTypeContainer::List(_) => true,
        }
    }

    pub fn map<F, B>(self, f: F) -> TypeContainer<B>
    where
        F: FnOnce(T) -> B,
    {
        TypeContainer {
            base: self.base.map(f),
            nullable: self.nullable,
        }
    }

    pub fn list_dimensions(&self) -> usize {
        match &self.base {
            BaseTypeContainer::Named(_) => 0,
            BaseTypeContainer::List(ty) => 1 + ty.list_dimensions(),
        }
    }
}

// impl Type {
//     /// Create a type from the type string.
//     #[must_use]
//     pub fn new(ty: &str) -> Option<Self> {
//         let (nullable, ty) = if let Some(rest) = ty.strip_suffix('!') {
//             (false, rest)
//         } else {
//             (true, ty)
//         };

//         Some(Self {
//             base: if let Some(ty) = ty.strip_prefix('[') {
//                 BaseType::List(Box::new(Self::new(ty.strip_suffix(']')?)?))
//             } else {
//                 BaseType::Named(Name::new(ty))
//             },
//             nullable,
//         })
//     }
// }

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.base.fmt(f)?;
        if !self.nullable {
            f.write_char('!')?;
        }
        Ok(())
    }
}

/// A GraphQL base type, for example `String` or `[String!]`. This does not include whether the
/// type is nullable; for that see [Type](struct.Type.html).
#[derive(Serialize, Deserialize, Hash, Debug, PartialEq, Eq, Clone)]
pub enum BaseTypeContainer<T> {
    /// A named type, such as `String`.
    Named(T),
    /// A list type, such as `[String]`.
    List(Box<TypeContainer<T>>),
}
impl<T> BaseTypeContainer<T> {
    fn map<F, B>(self, f: F) -> BaseTypeContainer<B>
    where
        F: FnOnce(T) -> B,
    {
        match self {
            BaseTypeContainer::Named(t) => BaseTypeContainer::Named(f(t)),
            BaseTypeContainer::List(t) => BaseTypeContainer::List(Box::new(t.map(f))),
        }
    }
}

pub type BaseType = BaseTypeContainer<TypeName>;

impl Display for BaseType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Named(name) => name.fmt(f),
            Self::List(ty) => write!(f, "[{ty}]"),
        }
    }
}

#[test]
fn test_graphql_compliant_name() -> anyhow::Result<()> {
    // Positive tests
    let name: Name = serde_json::from_str("\"foo\"")?;
    assert_eq!(name.get(), "foo");

    let name: Name = serde_json::from_str("\"FooBar\"")?;
    assert_eq!(name.get(), "FooBar");

    let name: Name = serde_json::from_str("\"_foo\"")?;
    assert_eq!(name.get(), "_foo");

    let name: Name = serde_json::from_str("\"_Foo\"")?;
    assert_eq!(name.get(), "_Foo");

    let name: Name = serde_json::from_str("\"foo1\"")?;
    assert_eq!(name.get(), "foo1");

    let name: Name = serde_json::from_str("\"Foo1\"")?;
    assert_eq!(name.get(), "Foo1");

    let name: Name = serde_json::from_str("\"foo_1\"")?;
    assert_eq!(name.get(), "foo_1");

    // Negative tests
    let name: Result<Name, _> = serde_json::from_str("\"1foo\"");
    assert!(name.is_err());

    let name: Result<Name, _> = serde_json::from_str("\"-foo\"");
    assert!(name.is_err());

    let name: Result<Name, _> = serde_json::from_str("\"foo bar\"");
    assert!(name.is_err());

    let name: Result<Name, _> = serde_json::from_str("\"foo-bar\"");
    assert!(name.is_err());

    Ok(())
}
