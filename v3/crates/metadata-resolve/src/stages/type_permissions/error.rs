use crate::helpers::typecheck::{self, TypecheckIssue};
use crate::types::error::{Error, ShouldBeAnError};
use open_dds::types::{CustomTypeName, FieldName};

use crate::types::subgraph::Qualified;

#[derive(Debug, thiserror::Error)]
pub enum TypeOutputPermissionError {
    #[error("unsupported type in output type permissions definition: {type_name:}; only object types are supported")]
    UnsupportedTypeInOutputPermissions { type_name: CustomTypeName },
    #[error("multiple output type permissions have been defined for type: {type_name:}")]
    DuplicateOutputTypePermissions { type_name: CustomTypeName },
    #[error("unknown type used in output permissions: {type_name:}")]
    UnknownTypeInOutputPermissionsDefinition {
        type_name: Qualified<CustomTypeName>,
    },
    #[error("unknown field '{field_name:}' used in output permissions of type '{type_name:}'")]
    UnknownFieldInOutputPermissionsDefinition {
        field_name: FieldName,
        type_name: CustomTypeName,
    },
}

impl From<TypeOutputPermissionError> for TypePermissionError {
    fn from(val: TypeOutputPermissionError) -> Self {
        TypePermissionError::TypeOutputPermissionError(val)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TypeInputPermissionError {
    #[error("unsupported type in input type permissions definition: {type_name:}; only object types are supported")]
    UnsupportedTypeInInputPermissions { type_name: CustomTypeName },
    #[error("unknown field '{field_name:}' used in output permissions of type '{type_name:}'")]
    UnknownFieldInInputPermissionsDefinition {
        field_name: FieldName,
        type_name: CustomTypeName,
    },
    #[error("multiple input type permissions have been defined for type: {type_name:}")]
    DuplicateInputTypePermissions { type_name: CustomTypeName },
    #[error(
        "Type error in field preset of {field_name:}, for input type permissions definition of type {type_name:}: {type_error:}"
    )]
    FieldPresetTypeError {
        field_name: FieldName,
        type_name: CustomTypeName,
        type_error: typecheck::TypecheckError,
    },
}

impl From<TypeInputPermissionError> for TypePermissionError {
    fn from(val: TypeInputPermissionError) -> Self {
        TypePermissionError::TypeInputPermissionError(val)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TypePermissionError {
    #[error("{0}")]
    TypeOutputPermissionError(TypeOutputPermissionError),
    #[error("{0}")]
    TypeInputPermissionError(TypeInputPermissionError),
}

impl From<TypePermissionError> for Error {
    fn from(val: TypePermissionError) -> Self {
        Error::TypePermissionError(val)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TypePermissionIssue {
    #[error("Type error in field preset of {field_name:}, for input type permissions definition of type {type_name:}: {typecheck_issue:}")]
    FieldPresetTypecheckIssue {
        field_name: FieldName,
        type_name: CustomTypeName,
        typecheck_issue: TypecheckIssue,
    },
}

impl ShouldBeAnError for TypePermissionIssue {
    fn should_be_an_error(&self, flags: &open_dds::flags::OpenDdFlags) -> bool {
        match self {
            TypePermissionIssue::FieldPresetTypecheckIssue {
                typecheck_issue, ..
            } => typecheck_issue.should_be_an_error(flags),
        }
    }
}
