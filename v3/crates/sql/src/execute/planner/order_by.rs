use std::collections::BTreeMap;

use datafusion::error::{DataFusionError, Result};
use metadata_resolve::{Qualified, TypeMapping};
use open_dds::{query::OrderByElement, types::CustomTypeName};

use super::common::{to_resolved_column, ResolvedColumn};

pub(crate) fn to_resolved_order_by_element(
    metadata: &metadata_resolve::Metadata,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    model_object_type: &metadata_resolve::ObjectTypeWithRelationships,
    element: &OrderByElement,
) -> Result<ir::OrderByElement> {
    match &element.operand {
        open_dds::query::Operand::Field(operand) => {
            let ResolvedColumn {
                column_name,
                field_path,
                field_mapping: _,
            } = to_resolved_column(
                metadata,
                type_mappings,
                type_name,
                model_object_type,
                operand,
            )?;

            let target = ir::OrderByTarget::Column {
                name: column_name,
                field_path: if field_path.is_empty() {
                    None
                } else {
                    Some(field_path)
                },
                relationship_path: vec![],
            };

            Ok(ir::OrderByElement {
                order_direction: match element.direction {
                    open_dds::models::OrderByDirection::Asc => schema::ModelOrderByDirection::Asc,
                    open_dds::models::OrderByDirection::Desc => schema::ModelOrderByDirection::Desc,
                },
                target,
            })
        }
        _ => Err(DataFusionError::Internal(format!(
            "unsupported operand in sort: {:?}",
            element.operand
        ))),
    }
}
