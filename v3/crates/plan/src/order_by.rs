use std::collections::BTreeMap;

use super::column::{to_resolved_column, ResolvedColumn};
use super::types::PlanError;
use metadata_resolve::{Qualified, TypeMapping};
use open_dds::{query::OrderByElement, types::CustomTypeName};
use plan_types::ResolvedFilterExpression;

pub fn to_resolved_order_by_element(
    metadata: &metadata_resolve::Metadata,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    type_name: &Qualified<CustomTypeName>,
    model_object_type: &metadata_resolve::ObjectTypeWithRelationships,
    element: &OrderByElement,
) -> Result<plan_types::OrderByElement<ResolvedFilterExpression>, PlanError> {
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

            let target = plan_types::OrderByTarget::Column {
                name: column_name,
                field_path,
                relationship_path: vec![],
            };

            Ok(plan_types::OrderByElement {
                order_direction: match element.direction {
                    open_dds::models::OrderByDirection::Asc => plan_types::OrderByDirection::Asc,
                    open_dds::models::OrderByDirection::Desc => plan_types::OrderByDirection::Desc,
                },
                target,
            })
        }
        _ => Err(PlanError::Internal(format!(
            "unsupported operand in sort: {:?}",
            element.operand
        ))),
    }
}
