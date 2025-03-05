use super::{
    relationships::{
        calculate_remote_relationship_fields_for_command_target,
        calculate_remote_relationship_fields_for_model_target,
        process_command_relationship_definition, process_model_relationship_definition,
        CommandRemoteRelationshipParts, ModelRemoteRelationshipParts,
    },
    CommandPlan,
};
use crate::metadata_accessor::OutputObjectTypeView;
use crate::types::{PlanError, RelationshipError};
use hasura_authn_core::Session;
use indexmap::IndexMap;
use metadata_resolve::{
    FieldNestedness, Metadata, Qualified, QualifiedBaseType, QualifiedTypeReference,
    RelationshipExecutionCategory, TypeMapping,
};
use open_dds::{
    arguments::ArgumentName,
    commands::DataConnectorCommand,
    models::ModelName,
    query::{
        Alias, CommandSelection, CommandTarget, ModelSelection, ModelTarget, ObjectFieldSelection,
        ObjectFieldTarget, ObjectSubSelection, RelationshipAggregateSelection,
        RelationshipSelection, RelationshipTarget, Value,
    },
    relationships::RelationshipName,
    types::{CustomTypeName, DataConnectorArgumentName, FieldName},
};
use plan_types::{
    CommandReturnKind, Field, JoinLocations, JoinNode, Location, LocationKind, NdcFieldAlias,
    NestedArray, NestedField, NestedObject, PredicateQueryTrees, ProcessResponseAs,
    QueryExecutionPlan, QueryExecutionTree, RemoteJoin, RemoteJoinType, ResolvedFilterExpression,
    UniqueNumber,
};
use std::collections::BTreeMap;

pub fn resolve_field_selection(
    metadata: &Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    object_type_name: &Qualified<CustomTypeName>,
    object_type: &OutputObjectTypeView,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    data_connector: &metadata_resolve::DataConnectorLink,
    selection: &IndexMap<Alias, ObjectSubSelection>,
    relationship_field_nestedness: metadata_resolve::FieldNestedness,
    relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    remote_join_executions: &mut JoinLocations,
    remote_predicates: &mut PredicateQueryTrees,
    unique_number: &mut UniqueNumber,
) -> Result<IndexMap<NdcFieldAlias, Field>, PlanError> {
    let metadata_resolve::TypeMapping::Object { field_mappings, .. } =
        type_mappings.get(object_type_name).ok_or_else(|| {
            PlanError::Internal(format!(
                "couldn't fetch type_mapping of type {object_type_name}",
            ))
        })?;

    let mut ndc_fields = IndexMap::new();
    for (field_alias, object_sub_selection) in selection {
        match object_sub_selection {
            ObjectSubSelection::Field(field_selection) => {
                from_field_selection(
                    metadata,
                    session,
                    request_headers,
                    type_mappings,
                    data_connector,
                    field_selection,
                    field_mappings,
                    object_type_name,
                    object_type,
                    relationship_field_nestedness,
                    NdcFieldAlias::from(field_alias.as_str()),
                    &mut ndc_fields,
                    relationships,
                    remote_join_executions,
                    remote_predicates,
                    unique_number,
                )?;
            }
            ObjectSubSelection::Relationship(relationship_selection) => {
                from_relationship_selection(
                    selection,
                    relationship_selection,
                    metadata,
                    session,
                    request_headers,
                    object_type_name,
                    object_type,
                    type_mappings,
                    data_connector,
                    relationship_field_nestedness,
                    NdcFieldAlias::from(field_alias.as_str()),
                    &mut ndc_fields,
                    relationships,
                    remote_join_executions,
                    remote_predicates,
                    unique_number,
                )?;
            }
            ObjectSubSelection::RelationshipAggregate(relationship_aggregate_selection) => {
                from_relationship_aggregate_selection(
                    relationship_aggregate_selection,
                    metadata,
                    session,
                    request_headers,
                    object_type_name,
                    object_type,
                    selection,
                    type_mappings,
                    data_connector,
                    relationship_field_nestedness,
                    NdcFieldAlias::from(field_alias.as_str()),
                    &mut ndc_fields,
                    relationships,
                    remote_join_executions,
                    remote_predicates,
                    unique_number,
                )?;
            }
        }
    }
    Ok(ndc_fields)
}

fn from_field_selection(
    metadata: &Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    data_connector: &metadata_resolve::DataConnectorLink,
    field_selection: &ObjectFieldSelection,
    field_mappings: &BTreeMap<FieldName, metadata_resolve::FieldMapping>,
    object_type_name: &Qualified<CustomTypeName>,
    object_type: &OutputObjectTypeView,
    relationship_field_nestedness: metadata_resolve::FieldNestedness,
    ndc_field_alias: NdcFieldAlias,
    ndc_fields: &mut IndexMap<NdcFieldAlias, Field>,
    relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    remote_join_executions: &mut JoinLocations,
    remote_predicates: &mut PredicateQueryTrees,
    unique_number: &mut UniqueNumber,
) -> Result<(), PlanError> {
    let ObjectFieldTarget {
        field_name,
        arguments,
    } = &field_selection.target;

    let field_type = object_type.get_field(field_name, &session.role)?.field_type;

    let field_mapping = field_mappings.get(field_name).ok_or_else(|| {
        PlanError::Internal(format!(
            "couldn't fetch field mapping of field {field_name} in type {object_type_name}"
        ))
    })?;

    let mut nested_remote_join_executions = JoinLocations::new();

    let fields = resolve_nested_field_selection(
        metadata,
        session,
        request_headers,
        type_mappings,
        data_connector,
        field_selection,
        field_type,
        relationship_field_nestedness,
        relationships,
        &mut nested_remote_join_executions,
        remote_predicates,
        unique_number,
    )?;

    if !nested_remote_join_executions.locations.is_empty() {
        // take any remote joins from the inner selection
        // and wrap them in nesting
        remote_join_executions.locations.insert(
            field_name.to_string(),
            Location {
                join_node: JoinNode::Local(LocationKind::NestedData),
                rest: nested_remote_join_executions,
            },
        );
    }

    let field_arguments = resolve_field_arguments(field_name, arguments, field_mapping)?;

    ndc_fields.insert(
        ndc_field_alias,
        Field::Column {
            column: field_mapping.column.clone(),
            fields,
            arguments: field_arguments,
        },
    );

    Ok(())
}

fn resolve_field_arguments(
    field_name: &FieldName,
    input_arguments: &IndexMap<ArgumentName, Value>,
    field_mapping: &metadata_resolve::FieldMapping,
) -> Result<BTreeMap<DataConnectorArgumentName, plan_types::Argument>, PlanError> {
    // NOTE: Presets for field arguments are not currently supported.
    let mut arguments = BTreeMap::new();
    for (argument_name, argument_value) in input_arguments {
        let ndc_argument_name = field_mapping.argument_mappings.get(argument_name).ok_or_else(|| {
            PlanError::Internal(format!(
                "couldn't find the argument mapping for argument {argument_name} in field {field_name}"
            ))
        })?;
        let argument = match argument_value {
            Value::Literal(literal) => plan_types::Argument::Literal {
                value: literal.clone(),
            },
            Value::BooleanExpression(_boolean_expression) => Err(PlanError::Internal(format!(
                "boolean expression arguments are not supported in field {field_name}",
            )))?,
        };
        arguments.insert(ndc_argument_name.clone(), argument);
    }
    Ok(arguments)
}

fn resolve_nested_field_selection(
    metadata: &Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    data_connector: &metadata_resolve::DataConnectorLink,
    field_selection: &ObjectFieldSelection,
    field_type: &QualifiedTypeReference,
    relationship_field_nestedness: metadata_resolve::FieldNestedness,
    relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    remote_join_executions: &mut JoinLocations,
    remote_predicates: &mut PredicateQueryTrees,
    unique_number: &mut UniqueNumber,
) -> Result<Option<NestedField>, PlanError> {
    match &field_selection.selection {
        None => {
            // Nested selection not found. Fallback to selecting all accessible nested fields.
            ndc_nested_field_selection_for(metadata, session, field_type, type_mappings)
        }
        Some(nested_selection) => {
            // Get the underlying object type
            let field_type_name = match field_type.get_underlying_type_name() {
                metadata_resolve::QualifiedTypeName::Custom(custom_type) => custom_type,
                metadata_resolve::QualifiedTypeName::Inbuilt(_) => {
                    // Inbuilt type can't have nested selection. Raise internal error.
                    return Err(PlanError::Internal(format!(
                        "field {} is a built-in scalar and cannot have a nested selection",
                        field_selection.target.field_name,
                    )));
                }
            };
            let nested_object_type = crate::metadata_accessor::get_output_object_type(
                metadata,
                field_type_name,
                &session.role,
            )?;

            let new_relationship_field_nestedness = match field_type.underlying_type {
                metadata_resolve::QualifiedBaseType::Named(_) => FieldNestedness::ObjectNested,
                metadata_resolve::QualifiedBaseType::List(_) => FieldNestedness::ArrayNested,
            };

            // Resolve the nested selection
            let resolved_nested_selection = resolve_field_selection(
                metadata,
                session,
                request_headers,
                field_type_name,
                &nested_object_type,
                type_mappings,
                data_connector,
                nested_selection,
                relationship_field_nestedness.max(new_relationship_field_nestedness),
                relationships,
                remote_join_executions,
                remote_predicates,
                unique_number,
            )?;

            // Build the nested field based on the underlying type
            let nested_field = match field_type.underlying_type {
                metadata_resolve::QualifiedBaseType::Named(_) => {
                    // This is an object type
                    plan_types::NestedField::Object(plan_types::NestedObject {
                        fields: resolved_nested_selection,
                    })
                }
                metadata_resolve::QualifiedBaseType::List(_) => {
                    // This is a list of object type
                    plan_types::NestedField::Array(plan_types::NestedArray {
                        fields: Box::new(plan_types::NestedField::Object(
                            plan_types::NestedObject {
                                fields: resolved_nested_selection,
                            },
                        )),
                    })
                }
            };
            Ok(Some(nested_field))
        }
    }
}

/// Resolve a relationship field
fn from_relationship_selection(
    source_selection: &IndexMap<Alias, ObjectSubSelection>,
    relationship_selection: &RelationshipSelection,
    metadata: &Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    object_type_name: &Qualified<CustomTypeName>,
    object_type: &OutputObjectTypeView,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    data_connector: &metadata_resolve::DataConnectorLink,
    relationship_field_nestedness: metadata_resolve::FieldNestedness,
    ndc_field_alias: NdcFieldAlias,
    ndc_fields: &mut IndexMap<NdcFieldAlias, Field>,
    relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    remote_join_executions: &mut JoinLocations,
    remote_predicates: &mut PredicateQueryTrees,
    unique_number: &mut UniqueNumber,
) -> Result<(), PlanError> {
    let relationship_name = &relationship_selection.target.relationship_name;
    let relationship_field =
        get_relationship_field(object_type_name, object_type, relationship_name)?;
    match &relationship_field.target {
        metadata_resolve::RelationshipTarget::Model(model_relationship_target) => {
            from_model_relationship(
                metadata,
                session,
                request_headers,
                object_type_name,
                source_selection,
                relationship_selection,
                relationship_field,
                relationship_field_nestedness,
                type_mappings,
                data_connector,
                model_relationship_target,
                ndc_field_alias,
                ndc_fields,
                relationships,
                remote_join_executions,
                remote_predicates,
                unique_number,
            )
        }
        metadata_resolve::RelationshipTarget::Command(command_relationship_target) => {
            Ok(from_command_relationship(
                metadata,
                session,
                request_headers,
                object_type_name,
                source_selection,
                relationship_selection,
                relationship_field,
                relationship_field_nestedness,
                data_connector,
                type_mappings,
                command_relationship_target,
                ndc_field_alias,
                ndc_fields,
                relationships,
                remote_join_executions,
                remote_predicates,
                unique_number,
            )?)
        }
    }
}

fn from_model_relationship(
    metadata: &Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    object_type_name: &Qualified<CustomTypeName>,
    source_selection: &IndexMap<Alias, ObjectSubSelection>,
    relationship_selection: &RelationshipSelection,
    relationship_field: &metadata_resolve::RelationshipField,
    relationship_field_nestedness: metadata_resolve::FieldNestedness,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    source_data_connector: &metadata_resolve::DataConnectorLink,
    model_relationship_target: &metadata_resolve::ModelRelationshipTarget,
    ndc_field_alias: NdcFieldAlias,
    ndc_fields: &mut IndexMap<NdcFieldAlias, Field>,
    collect_relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    remote_join_executions: &mut JoinLocations,
    remote_predicates: &mut PredicateQueryTrees,
    unique_number: &mut UniqueNumber,
) -> Result<(), PlanError> {
    let RelationshipSelection { target, selection } = relationship_selection;
    let RelationshipTarget {
        relationship_name,
        arguments,
        filter,
        order_by,
        limit,
        offset,
    } = target;
    let target_model_name = &model_relationship_target.model_name;

    let target_model = metadata.models.get(target_model_name).ok_or_else(|| {
        PlanError::Internal(format!("model {target_model_name} not found in metadata"))
    })?;

    let target_model_source =
        target_model.model.source.as_ref().ok_or_else(|| {
            PlanError::Internal(format!("model {target_model_name} has no source"))
        })?;

    let target_source = metadata_resolve::ModelTargetSource {
        model: target_model_source.clone(),
        capabilities: relationship_field
            .target_capabilities
            .as_ref()
            .ok_or_else(|| {
                PlanError::Relationship(RelationshipError::Other(format!(
                    "Relationship capabilities not found for relationship {} in data connector {}",
                    relationship_name, &target_model_source.data_connector.name,
                )))
            })?
            .clone(),
    };

    let relationship_model_target = ModelTarget {
        subgraph: target_model_name.subgraph.clone(),
        model_name: target_model_name.name.clone(),
        arguments: arguments.clone(),
        filter: filter.clone(),
        order_by: order_by.clone(),
        limit: *limit,
        offset: *offset,
    };

    let relationship_target_model_selection = ModelSelection {
        target: relationship_model_target,
        selection: selection.as_ref().map_or_else(IndexMap::new, Clone::clone),
    };

    // is it local or remote?
    match metadata_resolve::field_selection_relationship_execution_category(
        relationship_field_nestedness,
        source_data_connector,
        &target_model_source.data_connector,
        &target_source.capabilities,
    ) {
        metadata_resolve::RelationshipExecutionCategory::RemoteForEach => {
            let QueryExecutionTree {
                query_execution_plan: mut query_execution,
                remote_join_executions: sub_join_locations,
                remote_predicates: new_remote_predicates,
            } = super::model::from_model_selection(
                &relationship_target_model_selection,
                metadata,
                session,
                request_headers,
                unique_number,
            )?;

            let ModelRemoteRelationshipParts {
                join_mapping,
                mut relationship_join_filter_expressions,
                arguments: new_arguments,
            } = calculate_remote_relationship_fields_for_model_target(
                object_type_name,
                relationship_name,
                target_model_name,
                &model_relationship_target.mappings,
                source_type_mappings,
                &target_source.model.argument_mappings,
                source_selection,
                ndc_fields,
            )
            .map_err(PlanError::Relationship)?;

            // store remote predicates
            remote_predicates.0.extend(new_remote_predicates.0);

            // add field comparison to QueryExecutionPlan
            // we specifically push_front to match old GraphQL pipeline plan
            // once that's removed ordering is not important
            if let Some(ref query_filter) = query_execution.query_node.predicate {
                relationship_join_filter_expressions.push_front(query_filter.clone());
            }

            // combine existing filter with new ones
            if !relationship_join_filter_expressions.is_empty() {
                query_execution.query_node.predicate = Some(ResolvedFilterExpression::mk_and(
                    relationship_join_filter_expressions.into(),
                ));
            }

            // add the new arguments
            query_execution.arguments.extend(new_arguments);

            let remote_join = RemoteJoin {
                target_ndc_execution: query_execution,
                target_data_connector: target_source.model.data_connector.clone(),
                join_mapping,
                process_response_as: ProcessResponseAs::Array { is_nullable: true },
                remote_join_type: RemoteJoinType::ToModel,
            };

            remote_join_executions.locations.insert(
                relationship_name.as_str().to_owned(),
                Location {
                    join_node: JoinNode::Remote(remote_join),
                    rest: sub_join_locations,
                },
            );

            Ok(())
        }
        metadata_resolve::RelationshipExecutionCategory::Local => {
            // Collect this local relationship
            let ndc_relationship_name = record_local_model_relationship(
                object_type_name,
                relationship_name,
                target_model_name,
                &target_source,
                source_type_mappings,
                source_data_connector,
                model_relationship_target,
                collect_relationships,
            )?;

            let QueryExecutionTree {
                query_execution_plan:
                    QueryExecutionPlan {
                        query_node,
                        collection: _,
                        arguments: ndc_arguments,
                        collection_relationships: mut ndc_relationships,
                        variables: _,
                        data_connector: _,
                    },
                remote_predicates: new_remote_predicates,
                remote_join_executions: new_remote_join_executions,
            } = super::model::from_model_selection(
                &relationship_target_model_selection,
                metadata,
                session,
                request_headers,
                unique_number,
            )?;

            // Collect relationships from the generated query above
            collect_relationships.append(&mut ndc_relationships);

            if !new_remote_join_executions.is_empty() {
                // Collect remote joins, adding local relationship context to them
                remote_join_executions.locations.insert(
                    relationship_name.to_string(),
                    Location {
                        join_node: JoinNode::Local(LocationKind::LocalRelationship),
                        rest: new_remote_join_executions,
                    },
                );
            }

            // Collect remote predicates
            remote_predicates.0.extend(new_remote_predicates.0);

            ndc_fields.insert(
                ndc_field_alias,
                Field::Relationship {
                    relationship: ndc_relationship_name,
                    arguments: ndc_arguments,
                    query_node: Box::new(query_node),
                },
            );

            Ok(())
        }
    }
}

fn from_command_relationship(
    metadata: &Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    object_type_name: &Qualified<CustomTypeName>,
    source_selection: &IndexMap<Alias, ObjectSubSelection>,
    relationship_selection: &RelationshipSelection,
    relationship_field: &metadata_resolve::RelationshipField,
    relationship_field_nestedness: metadata_resolve::FieldNestedness,
    source_data_connector: &metadata_resolve::DataConnectorLink,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    command_relationship_target: &metadata_resolve::CommandRelationshipTarget,
    ndc_field_alias: NdcFieldAlias,
    ndc_fields: &mut IndexMap<NdcFieldAlias, Field>,
    collect_relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    remote_join_executions: &mut JoinLocations,
    remote_predicates: &mut PredicateQueryTrees,
    unique_number: &mut UniqueNumber,
) -> Result<(), PlanError> {
    let RelationshipSelection { target, selection } = relationship_selection;
    // Query parameters (limit, offset etc.) are not applicable for command selections
    let RelationshipTarget {
        relationship_name,
        arguments,
        filter: _,
        order_by: _,
        limit: _,
        offset: _,
    } = target;

    let command_name = &command_relationship_target.command_name;

    let command = metadata.commands.get(command_name).ok_or_else(|| {
        PlanError::Internal(format!("command {command_name} not found in metadata"))
    })?;

    let command_source = command
        .command
        .source
        .as_ref()
        .ok_or_else(|| PlanError::Internal(format!("command {command_name} has no source")))?;

    let function_name = match &command_source.source {
        DataConnectorCommand::Function(function_name) => function_name,
        DataConnectorCommand::Procedure(_) => {
            return Err(PlanError::Relationship(
                RelationshipError::ProcedureRelationshipsNotSupported {
                    relationship_name: relationship_name.clone(),
                },
            ));
        }
    };

    let target_capabilities = relationship_field
        .target_capabilities
        .as_ref()
        .ok_or_else(|| {
            PlanError::Relationship(RelationshipError::Other(format!(
                "Relationship capabilities not found for relationship {} in data connector {}",
                relationship_name, &command_source.data_connector.name,
            )))
        })?
        .clone();

    let command_target = CommandTarget {
        subgraph: command_name.subgraph.clone(),
        command_name: command_name.name.clone(),
        arguments: arguments.clone(),
    };

    let command_selection = CommandSelection {
        target: command_target,
        selection: selection.clone(),
    };

    let from_command = super::command::from_command_selection(
        &command_selection,
        metadata,
        session,
        request_headers,
        command_name,
        command,
        command_source,
        unique_number,
    )?;

    // is it local or remote?
    match metadata_resolve::field_selection_relationship_execution_category(
        relationship_field_nestedness,
        source_data_connector,
        &command_source.data_connector,
        &target_capabilities,
    ) {
        metadata_resolve::RelationshipExecutionCategory::RemoteForEach => {
            let CommandRemoteRelationshipParts {
                join_mapping,
                arguments: new_arguments,
            } = calculate_remote_relationship_fields_for_command_target(
                object_type_name,
                relationship_name,
                command_name,
                &command_relationship_target.mappings,
                source_type_mappings,
                &command_source.argument_mappings,
                source_selection,
                ndc_fields,
            )
            .map_err(PlanError::Relationship)?;

            let QueryExecutionTree {
                mut query_execution_plan,
                remote_predicates: new_remote_predicates,
                remote_join_executions: new_remote_join_executions,
            } = match from_command.command_plan {
                CommandPlan::Function(execution_tree) => execution_tree,
                CommandPlan::Procedure(_ndc_procedure) => {
                    // This shouldn't happen as we are already checking for procedure above
                    return Err(PlanError::Relationship(
                        RelationshipError::ProcedureRelationshipsNotSupported {
                            relationship_name: relationship_name.clone(),
                        },
                    ));
                }
            };

            // add the new arguments
            query_execution_plan.arguments.extend(new_arguments);

            // we push remote predicates to the outer list
            remote_predicates.0.extend(new_remote_predicates.0);

            let return_kind = match command.command.output_type.underlying_type {
                QualifiedBaseType::List(_) => CommandReturnKind::Array,
                QualifiedBaseType::Named(_) => CommandReturnKind::Object,
            };

            let rj_info = RemoteJoin {
                target_ndc_execution: query_execution_plan,
                target_data_connector: command_source.data_connector.clone(),
                join_mapping,
                process_response_as: ProcessResponseAs::CommandResponse {
                    command_name: command_name.clone().into(),
                    is_nullable: command.command.output_type.nullable,
                    return_kind,
                    response_config: command_source.data_connector.response_config.clone(),
                },
                remote_join_type: RemoteJoinType::ToCommand,
            };

            remote_join_executions.locations.insert(
                relationship_name.as_str().to_owned(),
                Location {
                    join_node: JoinNode::Remote(rj_info),
                    rest: new_remote_join_executions,
                },
            );

            Ok(())
        }
        metadata_resolve::RelationshipExecutionCategory::Local => {
            let local_command_relationship_info = plan_types::LocalCommandRelationshipInfo {
                relationship_name,
                source_type: object_type_name,
                source_type_mappings,
                command_name,
                argument_mappings: &command_source.argument_mappings,
                function_name,
                mappings: &command_relationship_target.mappings,
            };

            let ndc_relationship_name =
                plan_types::NdcRelationshipName::new(object_type_name, relationship_name);

            collect_relationships.insert(
                ndc_relationship_name.clone(),
                process_command_relationship_definition(&local_command_relationship_info)?,
            );

            let QueryExecutionTree {
                query_execution_plan:
                    QueryExecutionPlan {
                        query_node,
                        collection: _,
                        arguments: ndc_arguments,
                        collection_relationships: mut ndc_relationships,
                        variables: _,
                        data_connector: _,
                    },
                remote_predicates: new_remote_predicates,
                remote_join_executions: new_remote_join_executions,
            } = match from_command.command_plan {
                CommandPlan::Function(execution_tree) => execution_tree,
                CommandPlan::Procedure(_ndc_procedure) => {
                    // This shouldn't happen as we are already checking for procedure above
                    return Err(PlanError::Relationship(
                        RelationshipError::ProcedureRelationshipsNotSupported {
                            relationship_name: relationship_name.clone(),
                        },
                    ));
                }
            };

            // collect all relationships / joins etc
            collect_relationships.append(&mut ndc_relationships);
            remote_predicates.0.extend(new_remote_predicates.0);

            if !new_remote_join_executions.locations.is_empty() {
                // Collect any remote joins, adding local relationship context
                remote_join_executions.locations.insert(
                    relationship_name.to_string(),
                    Location {
                        join_node: JoinNode::Local(LocationKind::LocalRelationship),
                        rest: new_remote_join_executions,
                    },
                );
            }

            ndc_fields.insert(
                ndc_field_alias,
                Field::Relationship {
                    relationship: ndc_relationship_name,
                    arguments: ndc_arguments,
                    query_node: Box::new(query_node),
                },
            );

            Ok(())
        }
    }
}

fn record_local_model_relationship(
    object_type_name: &Qualified<CustomTypeName>,
    relationship_name: &RelationshipName,
    target_model_name: &Qualified<ModelName>,
    target_source: &metadata_resolve::ModelTargetSource,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    source_data_connector: &metadata_resolve::DataConnectorLink,
    model_relationship_target: &metadata_resolve::ModelRelationshipTarget,
    collect_relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
) -> Result<plan_types::NdcRelationshipName, PlanError> {
    let local_model_relationship_info = plan_types::LocalModelRelationshipInfo {
        relationship_name,
        relationship_type: &model_relationship_target.relationship_type,
        source_type: object_type_name,
        source_data_connector,
        source_type_mappings,
        target_model_name,
        target_source: &target_source.model,
        target_type: &model_relationship_target.target_typename,
        mappings: &model_relationship_target.mappings,
    };

    let ndc_relationship_name =
        plan_types::NdcRelationshipName::new(object_type_name, relationship_name);

    collect_relationships.insert(
        ndc_relationship_name.clone(),
        process_model_relationship_definition(&local_model_relationship_info)?,
    );
    Ok(ndc_relationship_name)
}

pub fn reject_remote_relationship(
    relationship_name: &RelationshipName,
    source_data_connector: &metadata_resolve::DataConnectorLink,
    target_data_connector: &metadata_resolve::DataConnectorLink,
) -> Result<(), PlanError> {
    if target_data_connector.name != source_data_connector.name {
        return Err(PlanError::Relationship(RelationshipError::Other(format!(
            "Remote relationships are not supported: {relationship_name}",
        ))));
    }
    Ok(())
}

/// Resolve a relationship field
fn from_relationship_aggregate_selection(
    relationship_aggregate_selection: &RelationshipAggregateSelection,
    metadata: &Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    object_type_name: &Qualified<CustomTypeName>,
    object_type: &OutputObjectTypeView,
    source_selection: &IndexMap<Alias, ObjectSubSelection>,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    source_data_connector: &metadata_resolve::DataConnectorLink,
    relationship_field_nestedness: metadata_resolve::FieldNestedness,
    ndc_field_alias: NdcFieldAlias,
    ndc_fields: &mut IndexMap<NdcFieldAlias, Field>,
    collect_relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    remote_join_executions: &mut JoinLocations,
    remote_predicates: &mut PredicateQueryTrees,
    unique_number: &mut UniqueNumber,
) -> Result<(), PlanError> {
    let RelationshipAggregateSelection { target, selection } = relationship_aggregate_selection;
    let RelationshipTarget {
        relationship_name,
        arguments,
        filter,
        order_by,
        limit,
        offset,
    } = target;
    let relationship_field =
        get_relationship_field(object_type_name, object_type, relationship_name)?;
    match &relationship_field.target {
        metadata_resolve::RelationshipTarget::Command(_) => Err(PlanError::Internal(format!(
            "Command relationships are not supported in aggregate fields: {relationship_name}"
        ))),
        metadata_resolve::RelationshipTarget::Model(model_relationship_target) => {
            let target_model_name = &model_relationship_target.model_name;
            let target_model = metadata.models.get(target_model_name).ok_or_else(|| {
                PlanError::Internal(format!("model {target_model_name} not found in metadata"))
            })?;

            let target_model_source = target_model.model.source.as_deref().ok_or_else(|| {
                PlanError::Internal(format!("model {target_model_name} has no source"))
            })?;

            let target_capabilities =
                relationship_field
                    .target_capabilities
                    .as_ref()
                    .ok_or_else(|| {
                        PlanError::Relationship(RelationshipError::Other(format!(
                    "Relationship capabilities not found for relationship {} in data connector {}",
                    relationship_name, &target_model_source.data_connector.name,
                )))
                    })?;

            let relationship_model_target = ModelTarget {
                subgraph: target_model_name.subgraph.clone(),
                model_name: target_model_name.name.clone(),
                arguments: arguments.clone(),
                filter: filter.clone(),
                order_by: order_by.clone(),
                limit: *limit,
                offset: *offset,
            };

            let relationship_aggregate_expression = model_relationship_target
                .relationship_aggregate
                .as_ref()
                .map(|aggregate| &aggregate.aggregate_expression);

            // is it local or remote?
            match metadata_resolve::field_selection_relationship_execution_category(
                relationship_field_nestedness,
                source_data_connector,
                &target_model_source.data_connector,
                target_capabilities,
            ) {
                RelationshipExecutionCategory::RemoteForEach => {
                    let QueryExecutionTree {
                        query_execution_plan: mut query_execution,
                        remote_join_executions: sub_join_locations,
                        remote_predicates: new_remote_predicates,
                    } = super::model::from_model_aggregate_selection(
                        &relationship_model_target,
                        selection,
                        metadata,
                        session,
                        relationship_aggregate_expression,
                        request_headers,
                        unique_number,
                    )?;

                    let ModelRemoteRelationshipParts {
                        join_mapping,
                        mut relationship_join_filter_expressions,
                        arguments: new_arguments,
                    } = calculate_remote_relationship_fields_for_model_target(
                        object_type_name,
                        relationship_name,
                        target_model_name,
                        &model_relationship_target.mappings,
                        source_type_mappings,
                        &target_model_source.argument_mappings,
                        source_selection,
                        ndc_fields,
                    )
                    .map_err(PlanError::Relationship)?;

                    // store remote predicates
                    remote_predicates.0.extend(new_remote_predicates.0);

                    // add field comparison to QueryExecutionPlan
                    // we specifically push_front to match old GraphQL pipeline plan
                    // once that's removed ordering is not important
                    if let Some(ref query_filter) = query_execution.query_node.predicate {
                        relationship_join_filter_expressions.push_front(query_filter.clone());
                    }

                    // combine existing filter with new ones
                    if !relationship_join_filter_expressions.is_empty() {
                        query_execution.query_node.predicate =
                            Some(ResolvedFilterExpression::mk_and(
                                relationship_join_filter_expressions.into(),
                            ));
                    }

                    // add the new arguments
                    query_execution.arguments.extend(new_arguments);

                    let remote_join = RemoteJoin {
                        target_ndc_execution: query_execution,
                        target_data_connector: target_model_source.data_connector.clone(),
                        join_mapping,
                        process_response_as: ProcessResponseAs::Array { is_nullable: true },
                        remote_join_type: RemoteJoinType::ToModel,
                    };

                    remote_join_executions.locations.insert(
                        ndc_field_alias.to_string(),
                        Location {
                            join_node: JoinNode::Remote(remote_join),
                            rest: sub_join_locations,
                        },
                    );

                    Ok(())
                }
                RelationshipExecutionCategory::Local => {
                    let local_model_relationship_info = plan_types::LocalModelRelationshipInfo {
                        relationship_name,
                        relationship_type: &model_relationship_target.relationship_type,
                        source_type: object_type_name,
                        source_data_connector,
                        source_type_mappings,
                        target_model_name,
                        target_source: target_model_source,
                        target_type: &model_relationship_target.target_typename,
                        mappings: &model_relationship_target.mappings,
                    };

                    let ndc_relationship_name =
                        plan_types::NdcRelationshipName::new(object_type_name, relationship_name);

                    // Record this relationship
                    collect_relationships.insert(
                        ndc_relationship_name.clone(),
                        process_model_relationship_definition(&local_model_relationship_info)?,
                    );

                    let QueryExecutionTree {
                        query_execution_plan:
                            QueryExecutionPlan {
                                query_node,
                                collection: _,
                                arguments: ndc_arguments,
                                collection_relationships: mut ndc_relationships,
                                variables: _,
                                data_connector: _,
                            },
                        remote_join_executions: new_remote_join_executions,
                        remote_predicates: new_remote_predicates,
                    } = super::model::from_model_aggregate_selection(
                        &relationship_model_target,
                        selection,
                        metadata,
                        session,
                        relationship_aggregate_expression,
                        request_headers,
                        unique_number,
                    )?;

                    // Collect relationships from the generated query above
                    collect_relationships.append(&mut ndc_relationships);
                    remote_join_executions
                        .locations
                        .extend(new_remote_join_executions.locations);
                    remote_predicates.0.extend(new_remote_predicates.0);

                    ndc_fields.insert(
                        ndc_field_alias,
                        Field::Relationship {
                            relationship: ndc_relationship_name,
                            arguments: ndc_arguments,
                            query_node: Box::new(query_node),
                        },
                    );

                    Ok(())
                }
            }
        }
    }
}

fn get_relationship_field<'a>(
    object_type_name: &'a Qualified<CustomTypeName>,
    object_type: &'a OutputObjectTypeView,
    relationship_name: &'a RelationshipName,
) -> Result<&'a metadata_resolve::RelationshipField, PlanError> {
    let relationship_field = object_type
        .relationship_fields
        .get(relationship_name)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "couldn't find the relationship {} in the type {}",
                relationship_name, &object_type_name,
            ))
        })?;
    Ok(relationship_field)
}

fn ndc_nested_field_selection_for(
    metadata: &Metadata,
    session: &Session,
    column_type: &QualifiedTypeReference,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
) -> Result<Option<NestedField>, PlanError> {
    match &column_type.underlying_type {
        metadata_resolve::QualifiedBaseType::Named(name) => match name {
            metadata_resolve::QualifiedTypeName::Custom(name) => {
                if let Some(_scalar_type) = metadata.scalar_types.get(name) {
                    return Ok(None);
                }

                let object_type = crate::metadata_accessor::get_output_object_type(
                    metadata,
                    name,
                    &session.role,
                )?;

                let TypeMapping::Object {
                    ndc_object_type_name: _,
                    field_mappings,
                } = type_mappings.get(name).ok_or_else(|| {
                    PlanError::Internal(format!("can't find mapping object for type: {name}"))
                })?;

                let mut fields = IndexMap::new();

                for (field_name, field_mapping) in field_mappings {
                    // `ObjectType` will only include a field if the role has access to it.
                    if let Some(field_def) = object_type.fields.get(field_name) {
                        let nested_fields: Option<NestedField> = ndc_nested_field_selection_for(
                            metadata,
                            session,
                            field_def.field_type,
                            type_mappings,
                        )?;
                        fields.insert(
                            NdcFieldAlias::from(field_name.as_str()),
                            Field::Column {
                                column: field_mapping.column.clone(),
                                fields: nested_fields,
                                arguments: BTreeMap::new(),
                            },
                        );
                    }
                }

                Ok(Some(NestedField::Object(NestedObject { fields })))
            }
            metadata_resolve::QualifiedTypeName::Inbuilt(_) => Ok(None),
        },
        metadata_resolve::QualifiedBaseType::List(list_type) => {
            let fields = ndc_nested_field_selection_for(
                metadata,
                session,
                list_type.as_ref(),
                type_mappings,
            )?;

            Ok(fields.map(|fields| {
                NestedField::Array(NestedArray {
                    fields: Box::new(fields),
                })
            }))
        }
    }
}
