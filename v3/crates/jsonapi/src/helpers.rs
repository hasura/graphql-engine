use crate::catalog::ObjectType;
use crate::ParseError;
use metadata_resolve::{Qualified, QualifiedBaseType, QualifiedTypeReference};
use open_dds::{relationships::RelationshipType, types::CustomTypeName};
use std::collections::BTreeMap;

pub fn get_object_type<'a>(
    object_types: &'a BTreeMap<Qualified<CustomTypeName>, ObjectType>,
    object_type_name: &Qualified<CustomTypeName>,
) -> Result<&'a ObjectType, ParseError> {
    object_types
        .get(object_type_name)
        .ok_or_else(|| ParseError::CannotFindObjectType(object_type_name.clone()))
}

/// Helper function to convert a type reference to a relationship type.
pub(crate) fn type_reference_to_relationship_type(
    type_reference: &QualifiedTypeReference,
) -> RelationshipType {
    match type_reference.underlying_type {
        QualifiedBaseType::Named(_) => RelationshipType::Object,
        QualifiedBaseType::List(_) => RelationshipType::Array,
    }
}
