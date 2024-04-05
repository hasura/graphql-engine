//! NDC query generation from 'ModelSelection' IR for relationships.

use ndc_models;
use std::collections::BTreeMap;

use super::selection_set;
use crate::execute::error;
use crate::execute::ir::model_selection::ModelSelection;
use crate::execute::ir::relationship;
use crate::execute::ir::selection_set::FieldSelection;

/// collect relationships recursively from IR components containing relationships,
/// and create NDC relationship definitions which will be added to the `relationships`
/// variable.
pub(crate) fn collect_relationships(
    ir: &ModelSelection<'_>,
    relationships: &mut BTreeMap<String, ndc_models::Relationship>,
) -> Result<(), error::Error> {
    // from selection fields
    for field in ir.selection.fields.values() {
        match field {
            FieldSelection::Column { .. } => (),
            FieldSelection::ModelRelationshipLocal {
                query,
                name,
                relationship_info,
            } => {
                relationships.insert(
                    name.to_string(),
                    relationship::process_model_relationship_definition(relationship_info)?,
                );
                collect_relationships(query, relationships)?;
            }
            FieldSelection::CommandRelationshipLocal {
                ir,
                name,
                relationship_info,
            } => {
                relationships.insert(
                    name.to_string(),
                    relationship::process_command_relationship_definition(relationship_info)?,
                );
                if let Some(nested_selection) = &ir.command_info.selection {
                    selection_set::collect_relationships_from_nested_selection(
                        nested_selection,
                        relationships,
                    )?;
                }
            }
            // we ignore remote relationships as we are generating relationship
            // definition for one data connector
            FieldSelection::ModelRelationshipRemote { .. } => (),
            FieldSelection::CommandRelationshipRemote { .. } => (),
        };
    }

    // from filter clause
    for (name, relationship) in ir.filter_clause.relationships.iter() {
        let result = relationship::process_model_relationship_definition(relationship)?;
        relationships.insert(name.to_string(), result);
    }

    // from order by clause
    if let Some(order_by) = &ir.order_by {
        for (name, relationship) in order_by.relationships.iter() {
            let result = relationship::process_model_relationship_definition(relationship)?;
            relationships.insert(name.to_string(), result);
        }
    };

    Ok(())
}
