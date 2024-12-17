//! NDC query generation from 'ModelSelection' IR for relationships.

use std::collections::BTreeMap;

use super::error;
use crate::ModelSelection;
use plan::process_model_relationship_definition;
use plan_types::{NdcRelationshipName, Relationship};

/// collect relationships from OrderBy IR component containing relationships.
pub(crate) fn collect_relationships_from_order_by(
    ir: &ModelSelection<'_>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
) -> Result<(), error::Error> {
    // from order by clause
    if let Some(order_by) = &ir.order_by {
        for (name, relationship) in &order_by.relationships {
            let result =
                process_model_relationship_definition(relationship).map_err(|plan_error| {
                    error::Error::Internal(error::InternalError::InternalGeneric {
                        description: plan_error.to_string(),
                    })
                })?;
            relationships.insert(name.clone(), result);
        }
    };
    Ok(())
}
