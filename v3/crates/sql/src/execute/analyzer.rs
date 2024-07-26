// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements.  See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.  The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License.  You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.

//! Analyzed rule to replace TableScan references
//! such as DataFrames and Views and inlines the LogicalPlan.

use std::sync::Arc;

use datafusion::{
    common::{
        config::ConfigOptions,
        tree_node::{Transformed, TransformedResult, TreeNode},
        Result,
    },
    error::DataFusionError,
    logical_expr::{logical_plan::LogicalPlan, Extension, TableScan},
    optimizer::AnalyzerRule,
};
use execute::ir::selection_set::NdcFieldAlias;
use indexmap::IndexMap;
use metadata_resolve::{self as resolved};
use open_dds::identifier::Identifier;
use open_dds::types::FieldName;

use crate::{catalog::introspection::HASURA_METADATA_SCHEMA, plan::NDCQuery};

/// Analyzed rule that inlines TableScan that provide a [`LogicalPlan`]
/// (DataFrame / ViewTable)
pub struct ReplaceTableScan {
    default_schema: Option<Arc<String>>,
    catalog: Arc<crate::catalog::OpenDDCatalogProvider>,
}

impl ReplaceTableScan {
    pub fn new(
        default_schema: Option<Arc<String>>,
        catalog: Arc<crate::catalog::OpenDDCatalogProvider>,
    ) -> Self {
        Self {
            default_schema,
            catalog,
        }
    }
}

impl AnalyzerRule for ReplaceTableScan {
    fn analyze(&self, plan: LogicalPlan, _: &ConfigOptions) -> Result<LogicalPlan> {
        plan.transform_up(|n| {
            analyze_internal(
                self.default_schema.as_ref().map(|x| x.as_str()),
                &self.catalog,
                n,
            )
        })
        .data()
    }

    fn name(&self) -> &str {
        "replace_table_scan_with_ndc_query"
    }
}

fn analyze_internal(
    default_schema: Option<&str>,
    catalog: &crate::catalog::OpenDDCatalogProvider,
    plan: LogicalPlan,
) -> Result<Transformed<LogicalPlan>> {
    // rewrite any subqueries in the plan first
    let transformed_plan = plan.map_subqueries(|plan| {
        plan.transform_up(|n| analyze_internal(default_schema, catalog, n))
    })?;

    let transformed_plan = transformed_plan.transform_data(|plan| match plan {
        LogicalPlan::TableScan(TableScan {
            table_name,
            source: _,
            projection: _,
            projected_schema,
            filters: _,
            fetch: _,
        }) if table_name.schema() != Some(HASURA_METADATA_SCHEMA)
            && table_name.schema() != Some("information_schema") =>
        {
            let table = catalog.get(default_schema, &table_name).ok_or_else(|| {
                DataFusionError::Internal(format!(
                    "table provider not found for replace_table_scan: {table_name}"
                ))
            })?;
            let model_source = table.source.as_ref().ok_or_else(|| {
                DataFusionError::Plan(format!(
                    "model source should be configured for {}",
                    table.name
                ))
            })?;
            let mut ndc_fields = IndexMap::new();

            let base_type_fields = {
                let base_type_mapping = model_source
                    .type_mappings
                    .get(&table.data_type)
                    .ok_or_else(|| {
                        DataFusionError::Internal(format!(
                            "couldn't fetch type_mapping of type {} for model {}",
                            table.data_type, table.name
                        ))
                    })?;
                match base_type_mapping {
                    resolved::TypeMapping::Object {
                        ndc_object_type_name: _,
                        field_mappings,
                    } => field_mappings,
                }
            };
            for field in projected_schema.fields() {
                let field_name = {
                    let field_name = Identifier::new(field.name().clone()).map_err(|e| {
                        DataFusionError::Internal(format!(
                            "field name conversion failed {}: {}",
                            field.name(),
                            e
                        ))
                    })?;
                    FieldName::new(field_name)
                };
                let ndc_field = {
                    base_type_fields
                        .get(&field_name)
                        .ok_or_else(|| {
                            DataFusionError::Internal(format!(
                                "couldn't fetch field mapping of field {} in type {} for model {}",
                                field_name, table.data_type, table.name
                            ))
                        })
                        .map(|field_mapping| field_mapping.column.clone())
                }?;
                ndc_fields.insert(
                    NdcFieldAlias::from(field.name().as_str()),
                    ndc_field.clone(),
                );
            }

            let ndc_query_node = NDCQuery {
                table: table_name.clone(),
                fields: ndc_fields,
                data_source_name: Arc::new(model_source.collection.clone()),
                schema: projected_schema,
            };
            Ok(Transformed::yes(LogicalPlan::Extension(Extension {
                node: Arc::new(ndc_query_node),
            })))
        }
        _ => Ok(Transformed::no(plan)),
    })?;

    Ok(transformed_plan)
}
