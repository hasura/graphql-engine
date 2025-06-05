use std::collections::BTreeMap;

use graphql_schema::GDS;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast::{self, Operation};
use metadata_resolve::{FilterPermission, ModelPredicate};
use open_dds::arguments::ArgumentName;
use open_dds::relationships::RelationshipType;
use open_dds::types::Deprecated;
use query_usage_analytics::{
    self, ArgumentPresetsUsage, FieldPresetsUsage, FieldUsage, FilterPredicateUsage, GqlField,
    GqlInputField, GqlOperation, OpenddObject, PermissionUsage, PredicateRelationshipUsage,
    RelationshipTarget, RelationshipUsage,
};
use tracing_util::{ErrorVisibility, TraceableError};

#[derive(Debug, thiserror::Error)]
#[error("Query usage analytics encoding failed: {0}")]
/// Error occurs while generating query usage analytics JSON.
/// Wraps JSON encoding error, the only error currently encountered.
pub struct QueryUsageAnalyzeError(#[from] serde_json::Error);

impl TraceableError for QueryUsageAnalyzeError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::Internal
    }
}

pub fn analyze_query_usage<'s>(normalized_request: &'s Operation<'s, GDS>) -> GqlOperation {
    let operation_name = normalized_request
        .name
        .as_ref()
        .map_or_else(String::new, std::string::ToString::to_string);
    let mut fields = Vec::new();
    for field in normalized_request.selection_set.fields.values() {
        fields.extend(analyze_field(field));
    }
    match normalized_request.ty {
        ast::OperationType::Query => GqlOperation::Query {
            operation_name,
            fields,
        },
        ast::OperationType::Mutation => GqlOperation::Mutation {
            operation_name,
            fields,
        },
        ast::OperationType::Subscription => GqlOperation::Subscription {
            operation_name,
            fields: vec![],
        },
    }
}

fn analyze_field<'s>(field: &'s normalized_ast::Field<'s, GDS>) -> Option<GqlField> {
    field.field_call().map_or_else(
        |_err| None,
        |field_call| {
            let alias = field.alias.to_string();
            let name = field_call.name.to_string();
            let mut arguments = Vec::new();
            for (argument_name, input_field) in &field_call.arguments {
                arguments.push(GqlInputField {
                    name: argument_name.to_string(),
                    fields: analyze_input_value(&input_field.value),
                    used: analyze_node_info(&input_field.info),
                });
            }
            let mut fields = Vec::new();
            for field in field.selection_set.fields.values() {
                fields.extend(analyze_field(field));
            }
            let used = analyze_node_info(&field_call.info);
            Some(GqlField {
                name,
                alias,
                arguments,
                fields,
                used,
            })
        },
    )
}

fn analyze_input_value<'s>(input_value: &'s normalized_ast::Value<'s, GDS>) -> Vec<GqlInputField> {
    let mut object_fields = Vec::new();
    match input_value {
        normalized_ast::Value::List(list) => {
            for value in list {
                object_fields.extend(analyze_input_value(value));
            }
        }
        normalized_ast::Value::Object(object) => {
            for (_name, field) in object {
                object_fields.push(analyze_input_field(field));
            }
        }
        normalized_ast::Value::SimpleValue(_) | normalized_ast::Value::Json(_) => {}
    }
    object_fields
}

fn analyze_input_field<'s>(input_field: &'s normalized_ast::InputField<'s, GDS>) -> GqlInputField {
    let name = input_field.name.to_string();
    let fields = analyze_input_value(&input_field.value);
    let used = analyze_node_info(&input_field.info);
    GqlInputField { name, fields, used }
}

fn analyze_node_info<'s>(
    node_info: &'s lang_graphql::schema::NodeInfo<'s, GDS>,
) -> Vec<OpenddObject> {
    let mut result = Vec::new();
    match node_info.generic {
        graphql_schema::Annotation::Output(output_annotation) => {
            result.extend(analyze_output_annotation(output_annotation));
        }
        graphql_schema::Annotation::Input(input_annotation) => {
            result.extend(analyze_input_annotation(input_annotation));
        }
    }
    match node_info.namespaced {
        None => {}
        Some(namespace_annotation) => {
            result.extend(analyze_namespace_annotation(namespace_annotation));
        }
    }
    result
}

fn analyze_input_annotation(annotation: &graphql_schema::InputAnnotation) -> Vec<OpenddObject> {
    let mut result = Vec::new();
    match annotation {
        graphql_schema::InputAnnotation::Model(model_input_annotation) => {
            result.extend(analyze_model_input_annotation(model_input_annotation));
        }
        graphql_schema::InputAnnotation::InputObjectField {
            field_name,
            parent_type,
            deprecated,
            ..
        } => {
            let DeprecatedDetails {
                is_deprecated,
                reason,
            } = get_deprecated_details(deprecated.as_ref());
            result.push(OpenddObject::Field(FieldUsage {
                name: field_name.to_owned(),
                opendd_type: parent_type.to_owned(),
                deprecated: is_deprecated,
                deprecated_reason: reason,
            }));
        }
        graphql_schema::InputAnnotation::BooleanExpression(
            graphql_schema::BooleanExpressionAnnotation::ObjectBooleanExpressionField(field),
        ) => match field {
            graphql_schema::ObjectBooleanExpressionField::Field {
                field_name,
                object_type,
                deprecated,
                object_field_kind: _,
            } => {
                let DeprecatedDetails {
                    is_deprecated,
                    reason,
                } = get_deprecated_details(deprecated.as_ref());
                result.push(OpenddObject::Field(FieldUsage {
                    name: field_name.to_owned(),
                    opendd_type: object_type.to_owned(),
                    deprecated: is_deprecated,
                    deprecated_reason: reason,
                }));
            }
            graphql_schema::ObjectBooleanExpressionField::RelationshipField(
                relationship_annotation,
            ) => {
                let DeprecatedDetails {
                    is_deprecated,
                    reason,
                } = get_deprecated_details(relationship_annotation.deprecated.as_ref());
                result.push(OpenddObject::Relationship(RelationshipUsage {
                    name: relationship_annotation.relationship_name.clone(),
                    source: relationship_annotation.source_type.clone(),
                    target: RelationshipTarget::Model {
                        model_name: relationship_annotation.target_model_name.clone(),
                        relationship_type: relationship_annotation.relationship_type.clone(),
                        opendd_type: relationship_annotation.target_type.clone(),
                        mapping: get_relationship_model_mappings(&relationship_annotation.mappings),
                    },
                    deprecated: is_deprecated,
                    deprecated_reason: reason,
                }));
            }
            graphql_schema::ObjectBooleanExpressionField::LogicalOperatorField(_) => {}
        },
        graphql_schema::InputAnnotation::BooleanExpression(
            graphql_schema::BooleanExpressionAnnotation::BooleanExpressionRootField
            | graphql_schema::BooleanExpressionAnnotation::ScalarBooleanExpressionField(_),
        )
        | graphql_schema::InputAnnotation::CommandArgument { .. }
        | graphql_schema::InputAnnotation::Relay(_)
        | graphql_schema::InputAnnotation::FieldArgument { argument_name: _ }
        | graphql_schema::InputAnnotation::ApolloFederationRepresentationsInput(_) => {}
    }
    result
}

fn analyze_model_input_annotation(
    annotation: &graphql_schema::ModelInputAnnotation,
) -> Vec<OpenddObject> {
    let mut result = Vec::new();
    match annotation {
        graphql_schema::ModelInputAnnotation::ModelOrderByArgument {
            field_name,
            parent_type,
            deprecated,
            ..
        } => {
            let DeprecatedDetails {
                is_deprecated,
                reason,
            } = get_deprecated_details(deprecated.as_ref());
            result.push(OpenddObject::Field(FieldUsage {
                name: field_name.to_owned(),
                opendd_type: parent_type.to_owned(),
                deprecated: is_deprecated,
                deprecated_reason: reason,
            }));
        }
        graphql_schema::ModelInputAnnotation::ModelOrderByRelationshipArgument(
            relationship_orderby,
        ) => {
            let DeprecatedDetails {
                is_deprecated,
                reason,
            } = get_deprecated_details(relationship_orderby.deprecated.as_ref());
            result.push(OpenddObject::Relationship(RelationshipUsage {
                name: relationship_orderby.relationship_name.clone(),
                source: relationship_orderby.source_type.clone(),
                target: RelationshipTarget::Model {
                    model_name: relationship_orderby.target_model_name.clone(),
                    relationship_type: relationship_orderby.relationship_type.clone(),
                    opendd_type: relationship_orderby.target_type.clone(),
                    mapping: get_relationship_model_mappings(&relationship_orderby.mappings),
                },
                deprecated: is_deprecated,
                deprecated_reason: reason,
            }));
        }
        graphql_schema::ModelInputAnnotation::ModelArgumentsExpression
        | graphql_schema::ModelInputAnnotation::ModelArgument { .. }
        | graphql_schema::ModelInputAnnotation::ModelOrderByExpression
        | graphql_schema::ModelInputAnnotation::ModelOrderByNestedExpression { .. }
        | graphql_schema::ModelInputAnnotation::ModelOrderByDirection { .. }
        | graphql_schema::ModelInputAnnotation::ModelLimitArgument
        | graphql_schema::ModelInputAnnotation::ModelOffsetArgument
        | graphql_schema::ModelInputAnnotation::ModelUniqueIdentifierArgument { .. }
        | graphql_schema::ModelInputAnnotation::ModelFilterInputArgument => {}
    }
    result
}

fn analyze_output_annotation(annotation: &graphql_schema::OutputAnnotation) -> Vec<OpenddObject> {
    let mut result = Vec::new();
    match annotation {
        graphql_schema::OutputAnnotation::RootField(root_field) => match root_field {
            graphql_schema::RootFieldAnnotation::Model { name, .. }
            | graphql_schema::RootFieldAnnotation::ModelSubscription { name, .. } => {
                result.push(OpenddObject::Model {
                    name: name.to_owned(),
                });
            }
            graphql_schema::RootFieldAnnotation::FunctionCommand { name, .. }
            | graphql_schema::RootFieldAnnotation::ProcedureCommand { name, .. } => {
                result.push(OpenddObject::Command {
                    name: name.to_owned(),
                });
            }
            graphql_schema::RootFieldAnnotation::Introspection
            | graphql_schema::RootFieldAnnotation::RelayNode { .. }
            | graphql_schema::RootFieldAnnotation::ApolloFederation(_) => {}
        },
        graphql_schema::OutputAnnotation::Field {
            name,
            parent_type,
            deprecated,
            ..
        } => {
            let DeprecatedDetails {
                is_deprecated,
                reason,
            } = get_deprecated_details(deprecated.as_ref());
            result.push(OpenddObject::Field(FieldUsage {
                name: name.to_owned(),
                opendd_type: parent_type.to_owned(),
                deprecated: is_deprecated,
                deprecated_reason: reason,
            }));
        }
        graphql_schema::OutputAnnotation::RelationshipToModel(relationship) => {
            let DeprecatedDetails {
                is_deprecated,
                reason,
            } = get_deprecated_details(relationship.deprecated.as_ref());
            result.push(OpenddObject::Relationship(RelationshipUsage {
                name: relationship.relationship_name.clone(),
                source: relationship.source_type.clone(),
                target: RelationshipTarget::Model {
                    model_name: relationship.target_model_name.clone(),
                    relationship_type: relationship.relationship_type.clone(),
                    opendd_type: relationship.target_type.clone(),
                    mapping: get_relationship_model_mappings(&relationship.mappings),
                },
                deprecated: is_deprecated,
                deprecated_reason: reason,
            }));
        }
        graphql_schema::OutputAnnotation::RelationshipToCommand(relationship) => {
            let DeprecatedDetails {
                is_deprecated,
                reason,
            } = get_deprecated_details(relationship.deprecated.as_ref());
            result.push(OpenddObject::Relationship(RelationshipUsage {
                name: relationship.relationship_name.clone(),
                source: relationship.source_type.clone(),
                target: RelationshipTarget::Command {
                    command_name: relationship.command_name.clone(),
                    opendd_type: relationship
                        .target_type
                        .get_underlying_type_name()
                        .to_untagged(),
                    mapping: get_relationship_command_mappings(&relationship.mappings),
                },
                deprecated: is_deprecated,
                deprecated_reason: reason,
            }));
        }
        graphql_schema::OutputAnnotation::RelationshipToModelAggregate(relationship) => {
            let DeprecatedDetails {
                is_deprecated,
                reason,
            } = get_deprecated_details(relationship.deprecated.as_ref());
            result.push(OpenddObject::Relationship(RelationshipUsage {
                name: relationship.relationship_name.clone(),
                source: relationship.source_type.clone(),
                target: RelationshipTarget::Model {
                    model_name: relationship.target_model_name.clone(),
                    relationship_type: RelationshipType::Array,
                    opendd_type: relationship.target_type.clone(),
                    mapping: get_relationship_model_mappings(&relationship.mappings),
                },
                deprecated: is_deprecated,
                deprecated_reason: reason,
            }));
        }
        graphql_schema::OutputAnnotation::GlobalIDField { .. }
        | graphql_schema::OutputAnnotation::RelayNodeInterfaceID { .. }
        | graphql_schema::OutputAnnotation::SDL
        | graphql_schema::OutputAnnotation::Aggregate(_) => {}
    }
    result
}

fn analyze_namespace_annotation(
    annotation: &graphql_schema::NamespaceAnnotation,
) -> Vec<OpenddObject> {
    let mut result = Vec::new();
    match annotation {
        graphql_schema::NamespaceAnnotation::Command(argument_presets) => {
            result.extend(analyze_argument_presets(argument_presets));
        }
        graphql_schema::NamespaceAnnotation::Model {
            filter,
            argument_presets,
            allow_subscriptions: _,
        } => {
            result.extend(analyze_filter_permission(filter));
            result.extend(analyze_argument_presets(argument_presets));
        }
        graphql_schema::NamespaceAnnotation::InputFieldPresets {
            presets_fields,
            type_name,
        } => result.push(OpenddObject::Permission(PermissionUsage::FieldPresets(
            FieldPresetsUsage {
                fields: presets_fields
                    .iter()
                    .map(|(field_name, deprecated)| {
                        let DeprecatedDetails {
                            is_deprecated,
                            reason,
                        } = get_deprecated_details(deprecated.as_ref());
                        FieldUsage {
                            name: field_name.to_owned(),
                            opendd_type: type_name.clone(),
                            deprecated: is_deprecated,
                            deprecated_reason: reason,
                        }
                    })
                    .collect(),
            },
        ))),
        graphql_schema::NamespaceAnnotation::NodeFieldTypeMappings(_)
        | graphql_schema::NamespaceAnnotation::EntityTypeMappings(_) => {}
    }
    result
}

fn analyze_argument_presets(
    argument_presets: &BTreeMap<
        ArgumentName,
        (
            metadata_resolve::QualifiedTypeReference,
            metadata_resolve::ValueExpressionOrPredicate,
        ),
    >,
) -> Option<OpenddObject> {
    if argument_presets.is_empty() {
        None
    } else {
        Some(OpenddObject::Permission(PermissionUsage::ArgumentPresets(
            ArgumentPresetsUsage {
                arguments: argument_presets.keys().cloned().collect(),
            },
        )))
    }
}

fn analyze_filter_permission(filter: &FilterPermission) -> Option<OpenddObject> {
    match filter {
        FilterPermission::AllowAll => None,
        FilterPermission::Filter(predicate) => Some(OpenddObject::Permission(
            PermissionUsage::FilterPredicate(analyze_model_predicate(predicate)),
        )),
    }
}

fn analyze_model_predicate(predicate: &ModelPredicate) -> FilterPredicateUsage {
    let mut fields = Vec::new();
    let mut relationships = Vec::new();
    analyze_model_predicate_internal(predicate, &mut fields, &mut relationships);
    FilterPredicateUsage {
        fields,
        relationships,
    }
}

fn analyze_model_predicate_internal(
    predicate: &ModelPredicate,
    fields: &mut Vec<FieldUsage>,
    relationships: &mut Vec<PredicateRelationshipUsage>,
) {
    match predicate {
        ModelPredicate::And(predicates) | ModelPredicate::Or(predicates) => {
            analyze_model_predicate_list(predicates, fields, relationships);
        }
        ModelPredicate::Not(predicate) => {
            analyze_model_predicate_internal(predicate, fields, relationships);
        }
        ModelPredicate::UnaryFieldComparison {
            field,
            field_parent_type,
            deprecated,
            ..
        }
        | ModelPredicate::BinaryFieldComparison {
            field,
            field_parent_type,
            deprecated,
            ..
        } => {
            let DeprecatedDetails {
                is_deprecated,
                reason,
            } = get_deprecated_details(deprecated.as_ref());
            fields.push(FieldUsage {
                name: field.to_owned(),
                opendd_type: field_parent_type.to_owned(),
                deprecated: is_deprecated,
                deprecated_reason: reason,
            });
        }
        ModelPredicate::Relationship {
            relationship_info,
            predicate,
            ..
        } => {
            relationships.push(PredicateRelationshipUsage {
                name: relationship_info.relationship_name.clone(),
                source: relationship_info.source_type.clone(),
                target: RelationshipTarget::Model {
                    model_name: relationship_info.target_model_name.clone(),
                    relationship_type: relationship_info.relationship_type.clone(),
                    opendd_type: relationship_info.target_type.clone(),
                    mapping: get_relationship_model_mappings(&relationship_info.mappings),
                },
                predicate_usage: Box::new(analyze_model_predicate(predicate)),
            });
        }
    }
}

fn analyze_model_predicate_list(
    predicates: &[ModelPredicate],
    fields: &mut Vec<FieldUsage>,
    relationships: &mut Vec<PredicateRelationshipUsage>,
) {
    for predicate in predicates {
        analyze_model_predicate_internal(predicate, fields, relationships);
    }
}

struct DeprecatedDetails {
    is_deprecated: bool,
    reason: Option<String>,
}

fn get_deprecated_details(deprecated: Option<&Deprecated>) -> DeprecatedDetails {
    let is_deprecated = deprecated.is_some();
    let reason = deprecated.as_ref().and_then(|d| d.reason.clone());
    DeprecatedDetails {
        is_deprecated,
        reason,
    }
}

fn get_relationship_model_mappings(
    mappings: &Vec<metadata_resolve::RelationshipModelMapping>,
) -> Vec<query_usage_analytics::RelationshipModelMapping> {
    let mut result = Vec::new();
    for mapping in mappings {
        result.push(query_usage_analytics::RelationshipModelMapping {
            source_field: mapping.source_field.field_name.clone(),
            target: match &mapping.target {
                metadata_resolve::RelationshipModelMappingTarget::ModelField(field_target) => {
                    query_usage_analytics::RelationshipModelMappingTarget::Field(
                        field_target.target_field.field_name.clone(),
                    )
                }
                metadata_resolve::RelationshipModelMappingTarget::Argument(argument_name) => {
                    query_usage_analytics::RelationshipModelMappingTarget::Argument(
                        argument_name.clone(),
                    )
                }
            },
        });
    }
    result
}

fn get_relationship_command_mappings(
    mappings: &Vec<metadata_resolve::RelationshipCommandMapping>,
) -> Vec<query_usage_analytics::RelationshipCommandMapping> {
    let mut result = Vec::new();
    for mapping in mappings {
        result.push(query_usage_analytics::RelationshipCommandMapping {
            source_field: mapping.source_field.field_name.clone(),
            target_argument: mapping.argument_name.clone(),
        });
    }
    result
}
