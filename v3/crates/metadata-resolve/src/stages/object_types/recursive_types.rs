pub use super::types::{ObjectTypeRepresentation, ObjectTypeWithTypeMappings, ObjectTypesIssue};
use crate::types::subgraph::Qualified;
use open_dds::types::CustomTypeName;
use std::collections::BTreeMap;

/// Helper struct to track the path during recursionsion detection
#[derive(Debug, Clone)]
struct FieldPathEntry {
    type_name: Qualified<CustomTypeName>,
    field_name: open_dds::types::FieldName,
}

/// Checks for infinite recursion in object types through non-nullable fields.
/// Returns a vector of issues for any recursive structures found.
pub(crate) fn check_recursive_object_types(
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithTypeMappings>,
) -> Vec<ObjectTypesIssue> {
    let mut issues = Vec::new();
    let mut visited = std::collections::HashSet::new();

    // Check all object types for recursive structures
    for (type_name, object_type) in object_types {
        check_recursive_object_type(
            object_types,
            type_name,
            &object_type.object_type,
            &mut Vec::new(),
            &mut visited,
            &mut issues,
        );
    }

    issues
}

/// Recursively checks for infinite recursion in object type field fields
fn check_recursive_object_type(
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithTypeMappings>,
    current_type: &Qualified<CustomTypeName>,
    object_type: &ObjectTypeRepresentation,
    path: &mut Vec<FieldPathEntry>,
    visited: &mut std::collections::HashSet<Qualified<CustomTypeName>>,
    issues: &mut Vec<ObjectTypesIssue>,
) {
    // Check if this type is already in our path (recursion detected)
    if let Some(cycle_start_idx) = path
        .iter()
        .position(|entry| entry.type_name == *current_type)
    {
        // Build the field path string for the errrecursionssage
        let field_path = path[cycle_start_idx..]
            .iter()
            .map(|entry| format!("{}.{}", entry.type_name, entry.field_name))
            .chain(std::iter::once(current_type.to_string()))
            .collect::<Vec<_>>()
            .join(" → ");

        issues.push(ObjectTypesIssue::RecursiveObjectType {
            type_name: current_type.clone(),
            field_path,
        });
        return;
    }

    // Only proceed if this type hasn't been fully visited yet
    if !visited.insert(current_type.clone()) {
        return;
    }

    // Check each field's type for potential recursion
    for (field_name, field) in &object_type.fields {
        // Get the underlying type name from the field type
        let field_type = field.field_type.get_underlying_type_name();

        // Only check fields whose type is a custom object type
        if let Some(field_type_name) = field_type.get_custom_type_name() {
            if let Some(field_object_type) = object_types.get(field_type_name) {
                // If the field is nullable or an array, we can skip it
                // as recursion can be broken by null value or empty array
                if field.field_type.nullable || field.field_type.is_array_type() {
                    continue;
                }

                // Update the path
                path.push(FieldPathEntry {
                    type_name: current_type.clone(),
                    field_name: field_name.clone(),
                });

                // Recursively check the field's type
                check_recursive_object_type(
                    object_types,
                    field_type_name,
                    &field_object_type.object_type,
                    path,
                    visited,
                    issues,
                );

                path.pop();
            }
        }
    }
}
