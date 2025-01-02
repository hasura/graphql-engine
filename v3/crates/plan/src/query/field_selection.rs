use crate::types::{PlanError, RelationshipError};
use hasura_authn_core::Session;
use indexmap::IndexMap;
use std::collections::BTreeMap;
use std::sync::Arc;

use super::{
    execute_plan_from_function,
    relationships::{
        process_command_relationship_definition, process_model_relationship_definition,
    },
    CommandPlan,
};
use metadata_resolve::{Metadata, Qualified, QualifiedTypeReference, TypeMapping};
use open_dds::{
    commands::DataConnectorCommand,
    models::ModelName,
    query::{
        Alias, CommandSelection, CommandTarget, ModelSelection, ModelTarget, ObjectFieldSelection,
        ObjectSubSelection, RelationshipAggregateSelection, RelationshipSelection,
        RelationshipTarget,
    },
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName},
};
use plan_types::{Field, NdcFieldAlias, NestedArray, NestedField, NestedObject, UniqueNumber};

pub fn resolve_field_selection(
    metadata: &Metadata,
    session: &Arc<Session>,
    request_headers: &reqwest::header::HeaderMap,
    object_type_name: &Qualified<CustomTypeName>,
    object_type: &metadata_resolve::ObjectTypeWithRelationships,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    data_connector: &metadata_resolve::DataConnectorLink,
    selection: &IndexMap<Alias, ObjectSubSelection>,
    relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
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
        let ndc_field = match object_sub_selection {
            ObjectSubSelection::Field(field_selection) => from_field_selection(
                metadata,
                session,
                request_headers,
                type_mappings,
                data_connector,
                field_selection,
                field_mappings,
                object_type_name,
                object_type,
                relationships,
                unique_number,
            )?,
            ObjectSubSelection::Relationship(relationship_selection) => {
                from_relationship_selection(
                    relationship_selection,
                    metadata,
                    session,
                    request_headers,
                    object_type_name,
                    object_type,
                    type_mappings,
                    data_connector,
                    relationships,
                    unique_number,
                )?
            }
            ObjectSubSelection::RelationshipAggregate(relationship_aggregate_selection) => {
                from_relationship_aggregate_selection(
                    relationship_aggregate_selection,
                    metadata,
                    session,
                    request_headers,
                    object_type_name,
                    object_type,
                    type_mappings,
                    data_connector,
                    relationships,
                    unique_number,
                )?
            }
        };
        ndc_fields.insert(NdcFieldAlias::from(field_alias.as_str()), ndc_field);
    }
    Ok(ndc_fields)
}

fn from_field_selection(
    metadata: &Metadata,
    session: &Arc<Session>,
    request_headers: &reqwest::header::HeaderMap,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    data_connector: &metadata_resolve::DataConnectorLink,
    field_selection: &ObjectFieldSelection,
    field_mappings: &BTreeMap<FieldName, metadata_resolve::FieldMapping>,
    object_type_name: &Qualified<CustomTypeName>,
    object_type: &metadata_resolve::ObjectTypeWithRelationships,
    relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<Field, PlanError> {
    let type_permissions = object_type
        .type_output_permissions
        .get(&session.role)
        .ok_or_else(|| {
            PlanError::Permission(format!(
                "role {} does not have permission to select any fields of type {}",
                session.role, object_type_name,
            ))
        })?;
    if !type_permissions
        .allowed_fields
        .contains(&field_selection.target.field_name)
    {
        return Err(PlanError::Permission(format!(
            "role {} does not have permission to select the field {} from type {}",
            session.role, field_selection.target.field_name, object_type_name,
        )));
    }

    let field_mapping = field_mappings
        .get(&field_selection.target.field_name)
        // .map(|field_mapping| field_mapping.column.clone())
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "couldn't fetch field mapping of field {} in type {}",
                field_selection.target.field_name, object_type_name,
            ))
        })?;

    let field_type = &object_type
        .object_type
        .fields
        .get(&field_selection.target.field_name)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "could not look up type of field {}",
                field_selection.target.field_name
            ))
        })?
        .field_type;

    let fields = resolve_nested_field_selection(
        metadata,
        session,
        request_headers,
        type_mappings,
        data_connector,
        field_selection,
        field_type,
        relationships,
        unique_number,
    )?;

    let ndc_field = Field::Column {
        column: field_mapping.column.clone(),
        fields,
        arguments: BTreeMap::new(),
    };
    Ok(ndc_field)
}

fn resolve_nested_field_selection(
    metadata: &Metadata,
    session: &Arc<Session>,
    request_headers: &reqwest::header::HeaderMap,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    data_connector: &metadata_resolve::DataConnectorLink,
    field_selection: &ObjectFieldSelection,
    field_type: &QualifiedTypeReference,
    relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<Option<NestedField>, PlanError> {
    match &field_selection.selection {
        None => {
            // Nested selection not found. Fallback to selecting all accessible nested fields.
            ndc_nested_field_selection_for(
                metadata,
                session,
                &field_selection.target.field_name,
                field_type,
                type_mappings,
            )
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
            let nested_object_type =
                metadata.object_types.get(field_type_name).ok_or_else(|| {
                    PlanError::Internal(format!(
                        "could not find object type {} in metadata for field {}",
                        field_type_name, field_selection.target.field_name
                    ))
                })?;
            // Resolve the nested selection
            let resolved_nested_selection = resolve_field_selection(
                metadata,
                session,
                request_headers,
                field_type_name,
                nested_object_type,
                type_mappings,
                data_connector,
                nested_selection,
                relationships,
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
    relationship_selection: &RelationshipSelection,
    metadata: &Metadata,
    session: &Arc<Session>,
    request_headers: &reqwest::header::HeaderMap,
    object_type_name: &Qualified<CustomTypeName>,
    object_type: &metadata_resolve::ObjectTypeWithRelationships,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    data_connector: &metadata_resolve::DataConnectorLink,
    relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<Field, PlanError> {
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
                relationship_selection,
                relationship_field,
                type_mappings,
                data_connector,
                model_relationship_target,
                relationships,
                unique_number,
            )
        }
        metadata_resolve::RelationshipTarget::Command(command_relationship_target) => {
            from_command_relationship(
                metadata,
                session,
                request_headers,
                object_type_name,
                relationship_selection,
                data_connector,
                type_mappings,
                command_relationship_target,
                relationships,
                unique_number,
            )
        }
    }
}

fn from_model_relationship(
    metadata: &Metadata,
    session: &Arc<Session>,
    request_headers: &reqwest::header::HeaderMap,
    object_type_name: &Qualified<CustomTypeName>,
    relationship_selection: &RelationshipSelection,
    relationship_field: &metadata_resolve::RelationshipField,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    source_data_connector: &metadata_resolve::DataConnectorLink,
    model_relationship_target: &metadata_resolve::ModelRelationshipTarget,
    collect_relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<Field, PlanError> {
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
    // Collect this local relationship
    let ndc_relationship_name = record_local_model_relationship(
        metadata,
        object_type_name,
        relationship_name,
        target_model_name,
        relationship_field,
        source_type_mappings,
        source_data_connector,
        model_relationship_target,
        collect_relationships,
    )?;
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

    let (_, ndc_query, ndc_fields) = super::model::from_model_selection(
        &relationship_target_model_selection,
        metadata,
        session,
        request_headers,
        unique_number,
    )?;
    let plan_types::QueryExecutionPlan {
        query_node,
        collection: _,
        arguments: ndc_arguments,
        collection_relationships: mut ndc_relationships,
        variables: _,
        data_connector: _,
    } = super::model::ndc_query_to_query_execution_plan(&ndc_query, &ndc_fields, &IndexMap::new());

    // Collect relationships from the generated query above
    collect_relationships.append(&mut ndc_relationships);

    let ndc_field = Field::Relationship {
        relationship: ndc_relationship_name,
        arguments: ndc_arguments,
        query_node: Box::new(query_node),
    };
    Ok(ndc_field)
}

fn from_command_relationship(
    metadata: &Metadata,
    session: &Arc<Session>,
    request_headers: &reqwest::header::HeaderMap,
    object_type_name: &Qualified<CustomTypeName>,
    relationship_selection: &RelationshipSelection,
    source_data_connector: &metadata_resolve::DataConnectorLink,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    command_relationship_target: &metadata_resolve::CommandRelationshipTarget,
    collect_relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<Field, PlanError> {
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
            return Err(PlanError::Relationship(RelationshipError::Other(format!(
                "Procedure relationships are not supported: {relationship_name}",
            ))));
        }
    };
    let local_command_relationship_info = plan_types::LocalCommandRelationshipInfo {
        relationship_name,
        source_type: object_type_name,
        source_type_mappings,
        command_name,
        argument_mappings: &command_source.argument_mappings,
        function_name,
        mappings: &command_relationship_target.mappings,
    };

    // Reject remote relationships
    reject_remote_relationship(
        relationship_name,
        source_data_connector,
        &command_source.data_connector,
    )?;

    let ndc_relationship_name =
        plan_types::NdcRelationshipName::new(object_type_name, relationship_name);
    collect_relationships.insert(
        ndc_relationship_name.clone(),
        process_command_relationship_definition(&local_command_relationship_info)?,
    );

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

    let plan_types::QueryExecutionPlan {
        query_node,
        collection: _,
        arguments: ndc_arguments,
        collection_relationships: mut ndc_relationships,
        variables: _,
        data_connector: _,
    } = match from_command.command_plan {
        CommandPlan::Function(ndc_function) => execute_plan_from_function(&ndc_function),
        CommandPlan::Procedure(_ndc_procedure) => {
            // This shouldn't happen as we are already checking for procedure above
            return Err(PlanError::Relationship(RelationshipError::Other(format!(
                "Procedure relationships are not supported: {relationship_name}",
            ))));
        }
    };

    collect_relationships.append(&mut ndc_relationships);

    let ndc_field = Field::Relationship {
        relationship: ndc_relationship_name,
        arguments: ndc_arguments,
        query_node: Box::new(query_node),
    };
    // returning the target data connector to check if it's a remote relationship at the caller
    Ok(ndc_field)
}

fn record_local_model_relationship(
    metadata: &Metadata,
    object_type_name: &Qualified<CustomTypeName>,
    relationship_name: &RelationshipName,
    target_model_name: &Qualified<ModelName>,
    relationship_field: &metadata_resolve::RelationshipField,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    source_data_connector: &metadata_resolve::DataConnectorLink,
    model_relationship_target: &metadata_resolve::ModelRelationshipTarget,
    collect_relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
) -> Result<plan_types::NdcRelationshipName, PlanError> {
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
    let local_model_relationship_info = plan_types::LocalModelRelationshipInfo {
        relationship_name,
        relationship_type: &model_relationship_target.relationship_type,
        source_type: object_type_name,
        source_data_connector,
        source_type_mappings,
        target_source: &target_source,
        target_type: &model_relationship_target.target_typename,
        mappings: &model_relationship_target.mappings,
    };

    let ndc_relationship_name =
        plan_types::NdcRelationshipName::new(object_type_name, relationship_name);

    // Reject remote relationship
    reject_remote_relationship(
        relationship_name,
        source_data_connector,
        &target_model_source.data_connector,
    )?;

    collect_relationships.insert(
        ndc_relationship_name.clone(),
        process_model_relationship_definition(&local_model_relationship_info)?,
    );
    Ok(ndc_relationship_name)
}

fn reject_remote_relationship(
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
    session: &Arc<Session>,
    request_headers: &reqwest::header::HeaderMap,
    object_type_name: &Qualified<CustomTypeName>,
    object_type: &metadata_resolve::ObjectTypeWithRelationships,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
    source_data_connector: &metadata_resolve::DataConnectorLink,
    collect_relationships: &mut BTreeMap<plan_types::NdcRelationshipName, plan_types::Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<Field, PlanError> {
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

            let target_model_source = target_model.model.source.as_ref().ok_or_else(|| {
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
            let local_model_relationship_info = plan_types::LocalModelRelationshipInfo {
                relationship_name,
                relationship_type: &model_relationship_target.relationship_type,
                source_type: object_type_name,
                source_data_connector,
                source_type_mappings,
                target_source: &target_source,
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

            let relationship_model_target = ModelTarget {
                subgraph: target_model_name.subgraph.clone(),
                model_name: target_model_name.name.clone(),
                arguments: arguments.clone(),
                filter: filter.clone(),
                order_by: order_by.clone(),
                limit: *limit,
                offset: *offset,
            };

            let super::model::ModelAggregateSelection {
                object_type_name: _,
                query: ndc_query,
                fields: aggregate_fields,
            } = super::model::from_model_aggregate_selection(
                &relationship_model_target,
                selection,
                metadata,
                session,
                request_headers,
                unique_number,
            )?;
            let plan_types::QueryExecutionPlan {
                query_node,
                collection: _,
                arguments: ndc_arguments,
                collection_relationships: mut ndc_relationships,
                variables: _,
                data_connector: _,
            } = super::model::ndc_query_to_query_execution_plan(
                &ndc_query,
                &plan_types::FieldsSelection {
                    fields: IndexMap::new(),
                },
                &aggregate_fields,
            );
            // Collect relationships from the generated query above
            collect_relationships.append(&mut ndc_relationships);
            Ok(Field::Relationship {
                relationship: ndc_relationship_name,
                arguments: ndc_arguments,
                query_node: Box::new(query_node),
            })
        }
    }
}

fn get_relationship_field<'a>(
    object_type_name: &'a Qualified<CustomTypeName>,
    object_type: &'a metadata_resolve::ObjectTypeWithRelationships,
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
    session: &Arc<Session>,
    column_name: &FieldName,
    column_type: &QualifiedTypeReference,
    type_mappings: &BTreeMap<Qualified<CustomTypeName>, TypeMapping>,
) -> Result<Option<NestedField>, PlanError> {
    match &column_type.underlying_type {
        metadata_resolve::QualifiedBaseType::Named(name) => match name {
            metadata_resolve::QualifiedTypeName::Custom(name) => {
                if let Some(_scalar_type) = metadata.scalar_types.get(name) {
                    return Ok(None);
                }
                if let Some(object_type) = metadata.object_types.get(name) {
                    let TypeMapping::Object {
                        ndc_object_type_name: _,
                        field_mappings,
                    } = type_mappings.get(name).ok_or_else(|| {
                        PlanError::Internal(format!("can't find mapping object for type: {name}"))
                    })?;

                    let type_output_permissions = object_type
                        .type_output_permissions
                        .get(&session.role)
                        .ok_or_else(|| {
                            PlanError::Permission(format!(
                                "cannot select nested field {column_name}; role {} does not have permission to select any fields of type {}",
                                session.role, name,
                            ))
                        })?;

                    let mut fields = IndexMap::new();

                    for (field_name, field_mapping) in field_mappings {
                        // Only include field if the role has access to it.
                        if type_output_permissions.allowed_fields.contains(field_name) {
                            let field_def = object_type.object_type.fields.get(field_name).ok_or_else(|| PlanError::Internal(format!(
                                "can't find object field definition for field {field_name} in type: {name}"
                            )))?;
                            let nested_fields: Option<NestedField> =
                                ndc_nested_field_selection_for(
                                    metadata,
                                    session,
                                    field_name,
                                    &field_def.field_type,
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

                    return Ok(Some(NestedField::Object(NestedObject { fields })));
                }

                Err(PlanError::Internal(format!(
                    "named type was neither a scalar nor an object: {name}",
                )))
            }
            metadata_resolve::QualifiedTypeName::Inbuilt(_) => Ok(None),
        },
        metadata_resolve::QualifiedBaseType::List(list_type) => {
            let fields = ndc_nested_field_selection_for(
                metadata,
                session,
                column_name,
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
