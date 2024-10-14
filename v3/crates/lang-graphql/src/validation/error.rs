use thiserror::Error;

use crate::ast::{common as ast, spanning};

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Error, Debug, Clone)]
pub enum Error {
    #[error("fragment cycle detected through: {0:?}")]
    CycleDetected(Vec<ast::Name>),
    // TODO, this error isn't thrown yet
    #[error("unused fragment: {0}")]
    FragmentNotUsed(ast::Name),
    #[error("fragment not defined in the document: {0}")]
    UnknownFragment(ast::Name),
    #[error("{} is defined on a non-composite type: {type_name}", match fragment_name { None => "inline fragment".to_owned(), Some(fragment_name) => format!("fragment {fragment_name}")})]
    FragmentOnNonCompositeType {
        fragment_name: Option<ast::Name>,
        type_name: ast::TypeName,
    },
    #[error("fragment of type {fragment_type} cannot be spread on type {selection_type}")]
    FragmentCannotBeSpread {
        selection_type: ast::TypeName,
        fragment_type: ast::TypeName,
    },
    // TODO, this error isn't thrown yet
    #[error(
        "a selection set is specified on field '{field_name}' of non-composite type: {type_name}"
    )]
    SelectionOnNonCompositeType {
        field_name: ast::Name,
        type_name: ast::TypeName,
    },
    #[error("no such field on type {type_name}: {field_name}")]
    NoFieldOnType {
        type_name: ast::TypeName,
        field_name: ast::Name,
    },
    #[error("no such type defined in the document: {0}")]
    UnknownType(ast::TypeName),
    #[error("an internal error occured during validation: type lookup failed for {type_name}")]
    InternalTypeNotFound { type_name: ast::TypeName },
    #[error("an internal error occured during validation: field {field_name} lookup failed for sub type '{sub_type_name}' of type '{type_name}'")]
    InternalNoFieldOnSubtype {
        type_name: ast::TypeName,
        sub_type_name: ast::TypeName,
        field_name: ast::Name,
    },
    #[error(
        "different fields {field1} and {field2} cannot be merged under the same alias: {alias}"
    )]
    FieldsConflictDifferentFields {
        alias: ast::Alias,
        field1: ast::Name,
        field2: ast::Name,
    },
    #[error("fields of different type {type1} and {type2} cannot be merged under the same alias: {alias}")]
    FieldsConflictDifferingTypes {
        alias: ast::Alias,
        type1: ast::Type,
        type2: ast::Type,
    },
    #[error("cannot merge fields with different arguments on the same alias: {alias}")]
    FieldsConflictDifferingArguments {
        alias: ast::Alias,
        location1: Option<spanning::SourcePosition>,
        location2: Option<spanning::SourcePosition>,
    },
    #[error("the string {str} in the provided json value is not a valid GraphQL name")]
    NotAValidName { str: String },
    #[error("expected a value of type {expected_type} but found a value of type {actual_type}")]
    IncorrectFormat {
        expected_type: &'static str,
        actual_type: &'static str,
    },
    #[error("a null value found when expected a value of not nullable type: {expected_type}")]
    UnexpectedNull { expected_type: ast::Type },
    #[error("the field {field_name} on type {type_name} is not found")]
    InputFieldNotFound {
        type_name: ast::TypeName,
        field_name: ast::Name,
    },
    #[error("the required fields {} on type {type_name} are not found", field_names.iter().fold(String::new(), |acc, name| acc + &name.to_string()))]
    RequiredInputFieldsNotFound {
        type_name: ast::TypeName,
        field_names: Vec<ast::Name>,
    },
    #[error("the field {field} on type {type_name} is specified more than once")]
    DuplicateInputFields {
        type_name: ast::TypeName,
        field: ast::Name,
    },
    #[error("the enum value {enum_value} on type {type_name} is not found")]
    EnumValueNotFound {
        type_name: ast::TypeName,
        enum_value: ast::Name,
    },
    #[error("the variable {variable_name} is of type {type_name} which is not an input type")]
    NotInputType {
        variable_name: ast::Name,
        type_name: ast::TypeName,
    },
    #[error("an internal error occured: expected {type_name} to be an input type but is of {actual_type}")]
    InternalNotInputType {
        type_name: ast::TypeName,
        actual_type: &'static str,
    },
    #[error("the variable {variable_name} is not defined in the document")]
    VariableNotDefined { variable_name: ast::Name },
    #[error("required variable {variable_name} not provided")]
    RequiredVariableNotProvided { variable_name: ast::Name },
    #[error("the variable {variable_name} of type {variable_type} cannot be used at a location of type {location_type}")]
    VariableSpreadNotAllowed {
        variable_name: ast::Name,
        variable_type: ast::Type,
        location_type: ast::Type,
    },
    #[error("the following variable is defined more than once: {variable_name}")]
    DuplicateVariableDeclarations { variable_name: ast::Name },
    #[error("the following fragment is defined more than once: {fragment_name}")]
    DuplicateFragmentDefinitions { fragment_name: ast::Name },
    #[error("the following operation is defined more than once: {operation_name}")]
    DuplicateOperationDefinitions { operation_name: ast::Name },
    #[error("there should only be one anonymous operation in the document")]
    AnonymousOperationMustBeUnique,
    #[error("no mutations are defined in the schema")]
    NoMutationsAreDefined,
    #[error("no subscriptions are defined in the schema")]
    NoSubscriptionsAreDefined,
    #[error("internal error: selection root is not of object type")]
    InternalSelectionRootIsNotObject,
    #[error("operation not found: {operation_name}")]
    OperationNotFound { operation_name: ast::Name },
    #[error("no anonymous operation found in the document")]
    AnonymousOperationNotFound,
    #[error("expected arguments '{}' on field {field_name} of type {type_name} are not found", argument_names.iter().map(ToString::to_string).collect::<Vec<_>>().join(", "))]
    ArgumentsNotFound {
        type_name: ast::TypeName,
        field_name: ast::Name,
        argument_names: Vec<ast::Name>,
    },
    #[error("argument {argument_name} on field {field_name} of type {type_name} not found")]
    ArgumentNotFound {
        type_name: ast::TypeName,
        field_name: ast::Name,
        argument_name: ast::Name,
    },
    #[error("argument {argument_name} on field {field_name} of type {type_name} is defined more than once")]
    DuplicateArguments {
        type_name: ast::TypeName,
        field_name: ast::Name,
        argument_name: ast::Name,
    },
    #[error(
        "required argument {argument_name} not found on field {field_name} of type {type_name}"
    )]
    RequiredArgumentNotFound {
        type_name: ast::TypeName,
        field_name: ast::Name,
        argument_name: ast::Name,
    },
    #[error("no fields are selected")]
    FieldSelectionSetIsEmpty,
}
