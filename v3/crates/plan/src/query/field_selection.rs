use crate::types::PlanError;
use hasura_authn_core::Session;
use indexmap::IndexMap;
use std::collections::BTreeMap;
use std::sync::Arc;

use graphql_ir::process_model_relationship_definition;
use metadata_resolve::{Metadata, Qualified, QualifiedTypeReference, TypeMapping};
use open_dds::{
    query::{
        Alias, ModelSelection, ModelTarget, ObjectFieldSelection, ObjectSubSelection,
        RelationshipSelection,
    },
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
            ObjectSubSelection::RelationshipAggregate(_) => {
                return Err(PlanError::Internal(
                    "only normal field/relationship selections are supported in NDCPushDownPlanner.".into(),
                ));
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
    let RelationshipSelection { target, selection } = relationship_selection;
    let (_, relationship_field) = object_type
        .relationship_fields
        .iter()
        .find(|(_, relationship_field)| {
            relationship_field.relationship_name == target.relationship_name
        })
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "couldn't find the relationship {} in the type {}",
                target.relationship_name, &object_type_name,
            ))
        })?;

    let metadata_resolve::RelationshipTarget::Model(model_relationship_target) =
        &relationship_field.target
    else {
        return Err(PlanError::Relationship(format!(
            "Expecting Model as a relationship target for {}",
            target.relationship_name,
        )));
    };
    let target_model_name = &model_relationship_target.model_name;
    let target_model = metadata.models.get(target_model_name).ok_or_else(|| {
        PlanError::Internal(format!("model {target_model_name} not found in metadata"))
    })?;

    let target_model_source =
        target_model.model.source.as_ref().ok_or_else(|| {
            PlanError::Internal(format!("model {target_model_name} has no source"))
        })?;

    // Reject remote relationships
    if target_model_source.data_connector.name != data_connector.name {
        return Err(PlanError::Relationship(format!(
            "Remote relationships are not supported: {}",
            &target.relationship_name
        )));
    }

    let target_source = metadata_resolve::ModelTargetSource {
        model: target_model_source.clone(),
        capabilities: relationship_field
            .target_capabilities
            .as_ref()
            .ok_or_else(|| {
                PlanError::Relationship(format!(
                    "Relationship capabilities not found for relationship {} in data connector {}",
                    &target.relationship_name, &target_model_source.data_connector.name,
                ))
            })?
            .clone(),
    };
    let local_model_relationship_info = plan_types::LocalModelRelationshipInfo {
        relationship_name: &target.relationship_name,
        relationship_type: &model_relationship_target.relationship_type,
        source_type: object_type_name,
        source_data_connector: data_connector,
        source_type_mappings: type_mappings,
        target_source: &target_source,
        target_type: &model_relationship_target.target_typename,
        mappings: &model_relationship_target.mappings,
    };

    let ndc_relationship_name =
        plan_types::NdcRelationshipName::new(object_type_name, &target.relationship_name);
    relationships.insert(
        ndc_relationship_name.clone(),
        process_model_relationship_definition(&local_model_relationship_info).map_err(|err| {
            PlanError::Internal(format!(
                "Unable to process relationship {} definition: {}",
                &target.relationship_name, err
            ))
        })?,
    );

    let relationship_model_target = ModelTarget {
        subgraph: target_model_name.subgraph.clone(),
        model_name: target_model_name.name.clone(),
        arguments: target.arguments.clone(),
        filter: target.filter.clone(),
        order_by: target.order_by.clone(),
        limit: target.limit,
        offset: target.offset,
    };

    let relationship_target_model_selection = ModelSelection {
        target: relationship_model_target,
        selection: selection.as_ref().map_or_else(IndexMap::new, Clone::clone),
    };

    let (_, ndc_query, relationship_fields) = super::model::from_model_selection(
        &relationship_target_model_selection,
        metadata,
        session,
        request_headers,
        unique_number,
    )?;
    let plan_types::QueryExecutionPlan {
        query_node,
        collection: _,
        arguments,
        mut collection_relationships,
        variables: _,
        data_connector: _,
    } = super::model::ndc_query_to_query_execution_plan(
        &ndc_query,
        &relationship_fields,
        &IndexMap::new(),
    );

    // Collect relationships from the generated query above
    relationships.append(&mut collection_relationships);

    let ndc_field = Field::Relationship {
        relationship: ndc_relationship_name,
        arguments,
        query_node: Box::new(query_node),
    };
    Ok(ndc_field)
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
