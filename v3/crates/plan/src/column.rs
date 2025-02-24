use super::types::PlanError;
use crate::metadata_accessor::OutputObjectTypeView;
use hasura_authn_core::Role;
use metadata_resolve::{
    FieldMapping, Qualified, QualifiedBaseType, QualifiedTypeName, TypeMapping,
};
use open_dds::{data_connector::DataConnectorColumnName, types::CustomTypeName};
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub struct ResolvedColumn {
    pub column_name: DataConnectorColumnName,
    pub field_path: Vec<DataConnectorColumnName>,
    pub field_mapping: FieldMapping,
}

/// Convert an ObjectFieldOperand into an NDC comparison target.
/// Also returns the FieldMapping for the enclosing object type, so
/// that additional mapping data (e.g. operators) can be extracted.
#[allow(clippy::assigning_clones)]
pub fn to_resolved_column(
    role: &Role,
    metadata: &metadata_resolve::Metadata,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    model_object_type: &OutputObjectTypeView,
    operand: &open_dds::query::ObjectFieldOperand,
) -> Result<ResolvedColumn, PlanError> {
    let TypeMapping::Object {
        ndc_object_type_name: _,
        field_mappings,
    } = type_mappings.get(type_name).ok_or_else(|| {
        PlanError::Internal(format!("can't find mapping object for type: {type_name}"))
    })?;

    // Walk down the tree of the ObjectFieldOperand, and maintain several pieces
    // of state as we go:

    // Keep track of the field mapping for the current object type:
    let mut field_mapping = field_mappings
        .get(&operand.target.field_name)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "can't find field {} in mapping for type: {}",
                operand.target.field_name, type_name
            ))
        })?;

    // The NDC field name of the top-level column
    let column_name = field_mapping.column.clone();

    // Keep track of the current field path (consisting of NDC names):
    let mut field_path = vec![];
    // Keep track of the rest of the tree to consider:
    let mut nested = operand.nested.clone();

    let field_type = model_object_type.get_field(&operand.target.field_name, role)?;

    // Keep track of the type of the current field under consideration
    // (this will be an object type until we reach the bottom of the tree):
    let mut current_type = field_type.field_type.underlying_type.clone();

    loop {
        match nested {
            None => {
                // At the bottom of the tree, return the comparison target with
                // the field path that we've accumulated:

                // if this is a scalar array, we create an object with a single
                // key
                let column_name = match field_type.field_type.underlying_type {
                    QualifiedBaseType::List(_) => {
                        plan_types::EXPRESSION_SCALAR_VALUE_VIRTUAL_COLUMN_NAME.into()
                    }
                    QualifiedBaseType::Named(_) => column_name,
                };

                return Ok(ResolvedColumn {
                    column_name,
                    field_path,
                    field_mapping: field_mapping.clone(),
                });
            }
            Some(operand) => {
                let open_dds::query::Operand::Field(field) = operand.as_ref() else {
                    return Err(PlanError::Internal(format!(
                        "unsupported operand: {operand:?}"
                    )));
                };

                assert!(field.target.arguments.is_empty());

                let field_name = &field.target.field_name;

                let QualifiedBaseType::Named(QualifiedTypeName::Custom(object_type_name)) =
                    current_type
                else {
                    return Err(PlanError::Internal(format!(
                        "field access on non-named type: {type_name:?}"
                    )));
                };

                let object_type = crate::metadata_accessor::get_output_object_type(
                    metadata,
                    &object_type_name,
                    role,
                )?;

                let field_defn = object_type.get_field(field_name, role)?;

                let field_type = &field_defn.field_type.underlying_type;

                let TypeMapping::Object {
                    ndc_object_type_name: _,
                    field_mappings,
                } = type_mappings.get(&object_type_name).ok_or_else(|| {
                    PlanError::Internal(format!("can't find mapping object for type: {type_name}"))
                })?;

                // Get the latest field mapping
                field_mapping = field_mappings.get(field_name).ok_or_else(|| {
                    PlanError::Internal(format!(
                        "can't find field {field_name} in mapping for type: {type_name}"
                    ))
                })?;

                // Add the NDC name of the object property to the field path
                field_path.push(field_mapping.column.clone());

                // Move to the next AST node
                nested = field.nested.clone();
                // Store the type of the current field:
                current_type = field_type.clone();
            }
        }
    }
}
