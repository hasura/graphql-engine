use std::collections::BTreeMap;

use datafusion::error::DataFusionError;
use metadata_resolve::{
    FieldMapping, Qualified, QualifiedBaseType, QualifiedTypeName, TypeMapping,
};
use open_dds::{data_connector::DataConnectorColumnName, types::CustomTypeName};

#[derive(Debug, Clone)]
pub(crate) struct PhysicalPlanOptions {
    pub(crate) disallow_mutations: bool,
}

impl PhysicalPlanOptions {
    pub(crate) fn new(disallow_mutations: bool) -> Self {
        Self { disallow_mutations }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ResolvedColumn {
    pub column_name: DataConnectorColumnName,
    pub field_path: Vec<DataConnectorColumnName>,
    pub field_mapping: FieldMapping,
}

/// Convert an ObjectFieldOperand into an NDC comparison target.
/// Also returns the FieldMapping for the enclosing object type, so
/// that additional mapping data (e.g. operators) can be extracted.
#[allow(clippy::assigning_clones)]
pub(crate) fn to_resolved_column(
    metadata: &metadata_resolve::Metadata,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    model_object_type: &metadata_resolve::ObjectTypeWithRelationships,
    operand: &open_dds::query::ObjectFieldOperand,
) -> datafusion::error::Result<ResolvedColumn> {
    let TypeMapping::Object {
        ndc_object_type_name: _,
        field_mappings,
    } = type_mappings.get(type_name).ok_or_else(|| {
        DataFusionError::Internal(format!("can't find mapping object for type: {type_name}"))
    })?;

    // Walk down the tree of the ObjectFieldOperand, and maintain several pieces
    // of state as we go:

    // Keep track of the field mapping for the current object type:
    let mut field_mapping = field_mappings
        .get(&operand.target.field_name)
        .ok_or_else(|| {
            DataFusionError::Internal(format!(
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

    let field_type = model_object_type
        .object_type
        .fields
        .get(&operand.target.field_name)
        .ok_or_else(|| {
            DataFusionError::Internal(format!(
                "can't find object field definition for field {} in type: {}",
                operand.target.field_name, type_name
            ))
        })?;

    // Keep track of the type of the current field under consideration
    // (this will be an object type until we reach the bottom of the tree):
    let mut current_type = field_type.field_type.underlying_type.clone();

    loop {
        match nested {
            None => {
                // At the bottom of the tree, return the comparison target with
                // the field path that we've accumulated:
                return Ok(ResolvedColumn {
                    column_name,
                    field_path,
                    field_mapping: field_mapping.clone(),
                });
            }
            Some(operand) => {
                let open_dds::query::Operand::Field(field) = operand.as_ref() else {
                    return Err(DataFusionError::Internal(format!(
                        "unsupported operand: {operand:?}"
                    )));
                };

                assert!(field.target.arguments.is_empty());

                let field_name = &field.target.field_name;

                let QualifiedBaseType::Named(QualifiedTypeName::Custom(object_type_name)) =
                    current_type
                else {
                    return Err(DataFusionError::Internal(format!(
                        "field access on non-named type: {type_name:?}"
                    )));
                };

                let Some(object_type) = metadata.object_types.get(&object_type_name) else {
                    return Err(DataFusionError::Internal(format!(
                        "field access on non-object type: {type_name:?}"
                    )));
                };

                let field_defn =
                    object_type
                        .object_type
                        .fields
                        .get(field_name)
                        .ok_or_else(|| {
                            DataFusionError::Internal(format!(
                    "can't find object field definition for field {field_name} in type: {type_name}"
                ))
                        })?;
                let field_type = &field_defn.field_type.underlying_type;

                let TypeMapping::Object {
                    ndc_object_type_name: _,
                    field_mappings,
                } = type_mappings.get(&object_type_name).ok_or_else(|| {
                    DataFusionError::Internal(format!(
                        "can't find mapping object for type: {type_name}"
                    ))
                })?;

                // Get the latest field mapping
                field_mapping = field_mappings.get(field_name).ok_or_else(|| {
                    DataFusionError::Internal(format!(
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
