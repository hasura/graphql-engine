use super::common::*;
use super::spanning::*;
use super::value::*;
// use std::collections::HashMap;

/// An executable GraphQL file or request string.
///
/// [Reference](https://spec.graphql.org/June2018/#ExecutableDocument).
#[derive(Debug, Clone)]
pub struct ExecutableDocument {
    /// The operations of the document.
    pub items: Vec<Spanning<ExecutableDefinition>>,
}

#[derive(Debug, Clone)]
pub enum ExecutableDefinition {
    Operation(OperationDefinition),
    Fragment(FragmentDefinition),
}

/// The operations of a GraphQL document.
///
/// There is either one anonymous operation or many named operations.
/// FIXME: I think we should use this? Then move the
///        lang-graphql/tests/query_testdata/ok/0001_empty test from 'ok' to 'err
// #[derive(Debug, Clone)]
// pub enum DocumentOperations {
//     /// The document contains a single anonymous operation.
//     Single(Spanning<OperationDefinition>),
//     /// The document contains many named operations.
//     Multiple(HashMap<Name, Spanning<OperationDefinition>>),
// }

pub type Argument = Spanning<KeyValue<Value>>;

/// A GraphQL directive, such as `@deprecated(reason: "Use the other field")`.
///
/// [Reference](https://spec.graphql.org/June2018/#Directive).
#[derive(Debug, Clone)]
pub struct Directive {
    /// The name of the directive.
    pub name: Spanning<Name>,
    /// The arguments to the directive.
    pub arguments: Option<Spanning<Vec<Argument>>>,
}

/// A GraphQL operation, such as `mutation($content:String!) { makePost(content: $content) { id } }`.
///
/// [Reference](https://spec.graphql.org/June2018/#OperationDefinition).
#[derive(Debug, Clone)]
pub struct OperationDefinition {
    /// The type of operation.
    pub ty: OperationType,
    /// The name of the operation.
    pub name: Option<Spanning<Name>>,
    /// The variable definitions.
    pub variable_definitions: Option<Spanning<Vec<Spanning<VariableDefinition>>>>,
    /// The operation's directives.
    pub directives: Vec<Spanning<Directive>>,
    /// The operation's selection set.
    pub selection_set: Spanning<SelectionSet>,
}

/// A variable definition inside a list of variable definitions, for example `$name:String!`.
///
/// [Reference](https://spec.graphql.org/June2018/#VariableDefinition).
#[derive(Debug, Clone)]
pub struct VariableDefinition {
    /// The name of the variable, without the preceding `$`.
    pub name: Spanning<Name>,
    /// The type of the variable.
    pub var_type: Spanning<Type>,
    /// The optional default value of the variable.
    pub default_value: Option<Spanning<ConstValue>>,
}

/// A set of fields to be selected, for example `{ name age }`.
///
/// [Reference](https://spec.graphql.org/June2018/#SelectionSet).
#[derive(Debug, Default, Clone)]
pub struct SelectionSet {
    /// The fields to be selected.
    pub items: Vec<Spanning<Selection>>,
}

/// A part of an object to be selected; a single field, a fragment spread or an inline fragment.
///
/// [Reference](https://spec.graphql.org/June2018/#Selection).
#[derive(Debug, Clone)]
pub enum Selection {
    /// Select a single field, such as `name` or `weightKilos: weight(unit: KILOGRAMS)`.
    Field(Field),
    /// Select using a fragment.
    FragmentSpread(FragmentSpread),
    /// Select using an inline fragment.
    InlineFragment(InlineFragment),
}

/// A field being selected on an object, such as `name` or `weightKilos: weight(unit: KILOGRAMS)`.
///
/// [Reference](https://spec.graphql.org/June2018/#Field).
#[derive(Debug, Clone)]
pub struct Field {
    /// The optional field alias.
    pub alias: Option<Spanning<Alias>>,
    /// The name of the field.
    pub name: Spanning<Name>,
    /// The arguments to the field, empty if no arguments are provided.
    pub arguments: Option<Spanning<Vec<Argument>>>,
    /// The directives in the field selector.
    pub directives: Vec<Spanning<Directive>>,
    /// The subfields being selected in this field, if it is an object. Empty if no fields are
    /// being selected.
    pub selection_set: Option<Spanning<SelectionSet>>,
}

/// A fragment selector, such as `... userFields`.
///
/// [Reference](https://spec.graphql.org/June2018/#FragmentSpread).
#[derive(Debug, Clone)]
pub struct FragmentSpread {
    /// The name of the fragment being selected.
    pub fragment_name: Spanning<Name>,
    /// The directives in the fragment selector.
    pub directives: Vec<Spanning<Directive>>,
}

/// An inline fragment selector, such as `... on User { name }`.
///
/// [Reference](https://spec.graphql.org/June2018/#InlineFragment).
#[derive(Debug, Clone)]
pub struct InlineFragment {
    /// The type condition.
    pub type_condition: Option<Spanning<TypeCondition>>,
    /// The directives in the inline fragment.
    pub directives: Vec<Spanning<Directive>>,
    /// The selected fields of the fragment.
    pub selection_set: Spanning<SelectionSet>,
}

/// The definition of a fragment, such as `fragment userFields on User { name age }`.
///
/// [Reference](https://spec.graphql.org/June2018/#FragmentDefinition).
#[derive(Debug, Clone)]
pub struct FragmentDefinition {
    /// Name of the fragment
    pub name: Spanning<Name>,
    /// The type this fragment operates on.
    pub type_condition: Spanning<TypeCondition>,
    /// Directives in the fragment.
    pub directives: Vec<Spanning<Directive>>,
    /// The fragment's selection set.
    pub selection_set: Spanning<SelectionSet>,
}

/// A type a fragment can apply to (`on` followed by the type).
///
/// [Reference](https://spec.graphql.org/June2018/#TypeCondition).
#[derive(Debug, Clone)]
pub struct TypeCondition {
    /// The type this fragment applies to.
    pub on: Spanning<TypeName>,
}
