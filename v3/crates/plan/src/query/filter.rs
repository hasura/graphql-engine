use super::relationships::{
    get_relationship_field_mapping_of_field_name, process_model_relationship_definition,
};
use crate::types::{PlanError, RelationshipError};
use indexmap::IndexMap;
use metadata_resolve::{DataConnectorLink, FieldMapping, Qualified, RelationshipCapabilities};
use open_dds::{
    data_connector::DataConnectorColumnName,
    models::ModelName,
    relationships::{RelationshipName, RelationshipType},
    types::CustomTypeName,
};
use plan_types::{
    Expression, Field, FieldsSelection, JoinLocations, LocalModelRelationshipInfo, NdcFieldAlias,
    NdcRelationshipName, PredicateQueryTree, PredicateQueryTrees, QueryExecutionPlan,
    QueryExecutionTree, QueryNodeNew, Relationship, RelationshipColumnMapping,
    ResolvedFilterExpression, SourceNdcColumn, UniqueNumber,
};
use std::collections::BTreeMap;

/// Plan the expression IR type.
pub fn plan_expression<'a>(
    expression: &'a Expression<'_>,
    relationships: &'a mut BTreeMap<NdcRelationshipName, Relationship>,
    remote_predicates: &'a mut PredicateQueryTrees,
    unique_number: &mut UniqueNumber,
) -> Result<ResolvedFilterExpression, PlanError> {
    match expression {
        Expression::And {
            expressions: and_expressions,
        } => {
            let mut results = Vec::new();
            for and_expression in and_expressions {
                let result = plan_expression(
                    and_expression,
                    relationships,
                    remote_predicates,
                    unique_number,
                )?;
                results.push(result);
            }
            Ok(ResolvedFilterExpression::mk_and(results))
        }
        Expression::Or {
            expressions: or_expressions,
        } => {
            let mut results = Vec::new();
            for or_expression in or_expressions {
                let result = plan_expression(
                    or_expression,
                    relationships,
                    remote_predicates,
                    unique_number,
                )?;
                results.push(result);
            }
            Ok(ResolvedFilterExpression::mk_or(results))
        }
        Expression::Not {
            expression: not_expression,
        } => {
            let result = plan_expression(
                not_expression,
                relationships,
                remote_predicates,
                unique_number,
            )?;
            Ok(ResolvedFilterExpression::mk_not(result))
        }
        Expression::LocalField(local_field_comparison) => Ok(
            ResolvedFilterExpression::LocalFieldComparison(local_field_comparison.clone()),
        ),
        Expression::LocalNestedArray {
            predicate,
            field_path,
            column,
        } => {
            let resolved_predicate =
                plan_expression(predicate, relationships, remote_predicates, unique_number)?;
            Ok(ResolvedFilterExpression::LocalNestedArray {
                column: column.clone(),
                field_path: field_path.clone(),
                predicate: Box::new(resolved_predicate),
            })
        }
        Expression::LocalNestedScalarArray {
            predicate,
            field_path,
            column,
        } => {
            let resolved_predicate =
                plan_expression(predicate, relationships, remote_predicates, unique_number)?;
            Ok(ResolvedFilterExpression::LocalNestedScalarArray {
                column: column.clone(),
                field_path: field_path.clone(),
                predicate: Box::new(resolved_predicate),
            })
        }
        Expression::RelationshipLocalComparison {
            relationship,
            field_path,
            predicate,
            info,
        } => {
            let relationship_filter =
                plan_expression(predicate, relationships, remote_predicates, unique_number)?;

            relationships.insert(
                relationship.clone(),
                process_model_relationship_definition(info)?,
            );

            Ok(ResolvedFilterExpression::LocalRelationshipComparison {
                field_path: field_path.clone(),
                relationship: relationship.clone(),
                predicate: Box::new(relationship_filter),
            })
        }
        Expression::RelationshipRemoteComparison {
            relationship: _,
            target_model_name,
            target_model_source,
            ndc_column_mapping,
            predicate,
        } => {
            let (remote_query_node, rest_predicate_trees, collection_relationships) =
                plan_remote_predicate(ndc_column_mapping, predicate, unique_number)?;

            let query_execution_plan: QueryExecutionPlan = QueryExecutionPlan {
                query_node: remote_query_node,
                collection: target_model_source.collection.clone(),
                arguments: BTreeMap::new(),
                collection_relationships,
                variables: None,
                data_connector: target_model_source.data_connector.clone(),
            };

            let predicate_query_tree = PredicateQueryTree {
                ndc_column_mapping: ndc_column_mapping.clone(),
                target_model_name: (*target_model_name).clone(),
                query: QueryExecutionTree {
                    query_execution_plan,
                    remote_predicates: PredicateQueryTrees::new(),
                    remote_join_executions: JoinLocations::new(),
                },
                children: rest_predicate_trees,
            };

            let remote_predicate_id = remote_predicates.insert(unique_number, predicate_query_tree);

            Ok(ResolvedFilterExpression::RemoteRelationshipComparison {
                remote_predicate_id,
            })
        }
    }
}

/// Generate comparison expression plan for remote relationshp predicate.
pub fn plan_remote_predicate<'a>(
    ndc_column_mapping: &'a [RelationshipColumnMapping],
    predicate: &'a Expression<'_>,
    unique_number: &mut UniqueNumber,
) -> Result<
    (
        QueryNodeNew,
        PredicateQueryTrees,
        BTreeMap<NdcRelationshipName, Relationship>,
    ),
    PlanError,
> {
    let mut relationships = BTreeMap::new();
    let mut remote_predicates = PredicateQueryTrees::new();
    let planned_predicate = plan_expression(
        predicate,
        &mut relationships,
        &mut remote_predicates,
        unique_number,
    )?;

    let query_node = QueryNodeNew {
        limit: None,
        offset: None,
        order_by: None,
        predicate: Some(planned_predicate),
        aggregates: None,
        fields: Some(FieldsSelection {
            fields: build_ndc_query_fields(ndc_column_mapping),
        }),
        group_by: None,
    };

    Ok((query_node, remote_predicates, relationships))
}

/// Generate the NDC query fields with the mapped NDC columns in a remote relationship.
/// These field values are fetched from the remote data connector.
fn build_ndc_query_fields(
    ndc_column_mapping: &[plan_types::RelationshipColumnMapping],
) -> IndexMap<NdcFieldAlias, Field> {
    let mut fields = IndexMap::new();
    for mapping in ndc_column_mapping {
        let field = Field::Column {
            column: mapping.target_ndc_column.clone(),
            fields: None,
            arguments: BTreeMap::new(),
        };
        fields.insert(
            NdcFieldAlias::from(mapping.target_ndc_column.as_str()),
            field,
        );
    }
    fields
}

/// Build a relationship comparison expression from a relationship predicate.
/// This is used for for both query and permission filter predicates.
/// Corresponding inner predicates need to be resolved prior to this function call
/// and passed as `relationship_predicate`.
pub fn build_relationship_comparison_expression<'s>(
    type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    column_path: Vec<DataConnectorColumnName>,
    data_connector_link: &'s DataConnectorLink,
    relationship_name: &'s RelationshipName,
    relationship_type: &'s RelationshipType,
    source_type: &'s Qualified<CustomTypeName>,
    target_model_name: &'s Qualified<ModelName>,
    target_model_source: &'s metadata_resolve::ModelSource,
    target_capabilities: &'s RelationshipCapabilities,
    target_type: &'s Qualified<CustomTypeName>,
    mappings: &'s Vec<metadata_resolve::RelationshipModelMapping>,
    relationship_predicate: Expression<'s>,
) -> Result<Expression<'s>, RelationshipError> {
    // Determine whether the relationship is local or remote
    match metadata_resolve::get_comparable_relationship_execution_strategy(
        &data_connector_link.name,
        &target_model_source.data_connector.name,
        target_capabilities.supports_relationships.as_ref(),
    ) {
        metadata_resolve::ComparableRelationshipExecutionStrategy::NDCPushdown => {
            let ndc_relationship_name = NdcRelationshipName::new(source_type, relationship_name);

            let local_model_relationship_info = LocalModelRelationshipInfo {
                relationship_name,
                relationship_type,
                source_type,
                source_data_connector: data_connector_link,
                source_type_mappings: type_mappings,
                target_model_name,
                target_source: target_model_source,
                target_type,
                mappings,
            };

            Ok(Expression::RelationshipLocalComparison {
                field_path: column_path,
                relationship: ndc_relationship_name,
                predicate: Box::new(relationship_predicate),
                info: local_model_relationship_info,
            })
        }

        metadata_resolve::ComparableRelationshipExecutionStrategy::InEngine => {
            // Build a NDC column mapping out of the relationship mappings.
            // This mapping is later used to build the local field comparison expressions
            // using values fetched from remote source
            let mut ndc_column_mapping = Vec::new();
            for relationship_mapping in mappings {
                let source_field = &relationship_mapping.source_field.field_name;
                let FieldMapping {
                    column: source_column,
                    comparison_operators,
                    ..
                } = get_relationship_field_mapping_of_field_name(
                    type_mappings,
                    source_type,
                    relationship_name,
                    source_field,
                )?;

                let equal_operator = comparison_operators
                    .as_ref()
                    .and_then(|ops| ops.eq_operator.as_ref())
                    .ok_or_else(|| {
                        RelationshipError::SourceColumnMissingEqualComparisonOperator {
                            relationship_name: relationship_name.clone(),
                            source_field: source_field.clone(),
                            source_column: source_column.clone(),
                        }
                    })?;

                let source_ndc_column = SourceNdcColumn {
                    column: source_column.clone(),
                    field_path: column_path.clone(),
                    eq_operator: equal_operator.clone(),
                };

                match &relationship_mapping.target {
                    metadata_resolve::RelationshipModelMappingTarget::ModelField(
                        relationship_mapping,
                    ) => {
                        let target_ndc_column = &relationship_mapping
                            .target_ndc_column
                            .as_ref()
                            .ok_or_else(|| RelationshipError::MissingTargetColumn {
                                relationship_name: relationship_name.clone(),
                                source_field: source_field.clone(),
                                target_field: relationship_mapping.target_field.field_name.clone(),
                            })?
                            .column;

                        ndc_column_mapping.push(RelationshipColumnMapping {
                            source_ndc_column,
                            target_ndc_column: target_ndc_column.clone(),
                        });
                    }
                    metadata_resolve::RelationshipModelMappingTarget::Argument(argument_name) => {
                        return Err(RelationshipError::RemotePredicatesNotSupportedWithArgumentMappingTarget {
                            relationship_name: relationship_name.clone(),
                            source_type: source_type.clone(),
                            source_field: source_field.clone(),
                            target_argument: argument_name.clone(),
                        });
                    }
                }
            }

            Ok(Expression::RelationshipRemoteComparison {
                relationship: relationship_name.clone(),
                target_model_name,
                target_model_source: target_model_source.clone().into(),
                ndc_column_mapping,
                predicate: Box::new(relationship_predicate),
            })
        }
    }
}
