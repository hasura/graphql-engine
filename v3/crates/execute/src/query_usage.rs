use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast::{self, Operation};
use metadata_resolve::{FieldPresetInfo, FilterPermission, ModelPredicate};
use open_dds::relationships::RelationshipType;
use query_usage_analytics::{
    self, ArgumentPresetsUsage, FieldPresetsUsage, FieldUsage, FilterPredicateUsage, GqlField,
    GqlInputField, GqlOperation, OpenddObject, PermissionUsage, PredicateRelationshipUsage,
    RelationshipTarget, RelationshipUsage,
};
use schema::GDS;

pub(crate) fn analyze_query_usage<'s>(normalized_request: &'s Operation<'s, GDS>) -> GqlOperation {
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
        schema::Annotation::Output(output_annotation) => {
            result.extend(analyze_output_annotation(output_annotation));
        }
        schema::Annotation::Input(input_annotation) => {
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

fn analyze_input_annotation(annotation: &schema::InputAnnotation) -> Vec<OpenddObject> {
    let mut result = Vec::new();
    match annotation {
        schema::InputAnnotation::Model(model_input_annotation) => {
            result.extend(analyze_model_input_annotation(model_input_annotation));
        }
        schema::InputAnnotation::InputObjectField {
            field_name,
            parent_type,
            deprecated,
            ..
        } => {
            result.push(OpenddObject::Field(FieldUsage {
                name: field_name.to_owned(),
                opendd_type: parent_type.to_owned(),
                deprecated: deprecated.to_owned(),
            }));
        }
        schema::InputAnnotation::BooleanExpression(
            schema::BooleanExpressionAnnotation::BooleanExpressionArgument { field },
        ) => match field {
            schema::ModelFilterArgument::Field {
                field_name,
                object_type,
                deprecated,
            } => {
                result.push(OpenddObject::Field(FieldUsage {
                    name: field_name.to_owned(),
                    opendd_type: object_type.to_owned(),
                    deprecated: deprecated.to_owned(),
                }));
            }
            schema::ModelFilterArgument::RelationshipField(relationship_annotation) => {
                result.push(OpenddObject::Relationship(RelationshipUsage {
                    name: relationship_annotation.relationship_name.clone(),
                    source: relationship_annotation.source_type.clone(),
                    target: RelationshipTarget::Model {
                        model_name: relationship_annotation.target_model_name.clone(),
                        relationship_type: relationship_annotation.relationship_type.clone(),
                    },
                }));
            }
            schema::ModelFilterArgument::AndOp
            | schema::ModelFilterArgument::OrOp
            | schema::ModelFilterArgument::NotOp => {}
        },
        schema::InputAnnotation::BooleanExpression(
            schema::BooleanExpressionAnnotation::BooleanExpression,
        )
        | schema::InputAnnotation::CommandArgument { .. }
        | schema::InputAnnotation::Relay(_)
        | schema::InputAnnotation::FieldArgument
        | schema::InputAnnotation::ApolloFederationRepresentationsInput(_) => {}
    }
    result
}

fn analyze_model_input_annotation(annotation: &schema::ModelInputAnnotation) -> Vec<OpenddObject> {
    let mut result = Vec::new();
    match annotation {
        schema::ModelInputAnnotation::ModelOrderByArgument {
            field_name,
            parent_type,
            deprecated,
            ..
        } => {
            result.push(OpenddObject::Field(FieldUsage {
                name: field_name.to_owned(),
                opendd_type: parent_type.to_owned(),
                deprecated: deprecated.to_owned(),
            }));
        }
        schema::ModelInputAnnotation::ModelOrderByRelationshipArgument(relationship_orderby) => {
            result.push(OpenddObject::Relationship(RelationshipUsage {
                name: relationship_orderby.relationship_name.clone(),
                source: relationship_orderby.source_type.clone(),
                target: RelationshipTarget::Model {
                    model_name: relationship_orderby.target_model_name.clone(),
                    relationship_type: relationship_orderby.relationship_type.clone(),
                },
            }));
        }
        schema::ModelInputAnnotation::ModelArgumentsExpression
        | schema::ModelInputAnnotation::ModelArgument { .. }
        | schema::ModelInputAnnotation::ComparisonOperation { .. }
        | schema::ModelInputAnnotation::IsNullOperation
        | schema::ModelInputAnnotation::ModelOrderByExpression
        | schema::ModelInputAnnotation::ModelOrderByDirection { .. }
        | schema::ModelInputAnnotation::ModelLimitArgument
        | schema::ModelInputAnnotation::ModelOffsetArgument
        | schema::ModelInputAnnotation::ModelUniqueIdentifierArgument { .. }
        | schema::ModelInputAnnotation::ModelFilterInputArgument => {}
    }
    result
}

fn analyze_output_annotation(annotation: &schema::OutputAnnotation) -> Vec<OpenddObject> {
    let mut result = Vec::new();
    match annotation {
        schema::OutputAnnotation::RootField(root_field) => match root_field {
            schema::RootFieldAnnotation::Model { name, .. } => {
                result.push(OpenddObject::Model {
                    name: name.to_owned(),
                });
            }
            schema::RootFieldAnnotation::FunctionCommand { name, .. }
            | schema::RootFieldAnnotation::ProcedureCommand { name, .. } => {
                result.push(OpenddObject::Command {
                    name: name.to_owned(),
                });
            }
            schema::RootFieldAnnotation::Introspection
            | schema::RootFieldAnnotation::RelayNode { .. }
            | schema::RootFieldAnnotation::ApolloFederation(_) => {}
        },
        schema::OutputAnnotation::Field {
            name,
            parent_type,
            deprecated,
            ..
        } => {
            result.push(OpenddObject::Field(FieldUsage {
                name: name.to_owned(),
                opendd_type: parent_type.to_owned(),
                deprecated: deprecated.to_owned(),
            }));
        }
        schema::OutputAnnotation::RelationshipToModel(relationship) => {
            result.push(OpenddObject::Relationship(RelationshipUsage {
                name: relationship.relationship_name.clone(),
                source: relationship.source_type.clone(),
                target: RelationshipTarget::Model {
                    model_name: relationship.model_name.clone(),
                    relationship_type: relationship.relationship_type.clone(),
                },
            }));
        }
        schema::OutputAnnotation::RelationshipToCommand(relationship) => {
            result.push(OpenddObject::Relationship(RelationshipUsage {
                name: relationship.relationship_name.clone(),
                source: relationship.source_type.clone(),
                target: RelationshipTarget::Command {
                    command_name: relationship.command_name.clone(),
                },
            }));
        }
        schema::OutputAnnotation::RelationshipToModelAggregate(relationship) => {
            result.push(OpenddObject::Relationship(RelationshipUsage {
                name: relationship.relationship_name.clone(),
                source: relationship.source_type.clone(),
                target: RelationshipTarget::Model {
                    model_name: relationship.model_name.clone(),
                    relationship_type: RelationshipType::Array,
                },
            }));
        }
        schema::OutputAnnotation::GlobalIDField { .. }
        | schema::OutputAnnotation::RelayNodeInterfaceID { .. }
        | schema::OutputAnnotation::SDL
        | schema::OutputAnnotation::Aggregate(_) => {}
    }
    result
}

fn analyze_namespace_annotation(annotation: &schema::NamespaceAnnotation) -> Vec<OpenddObject> {
    let mut result = Vec::new();
    match annotation {
        schema::NamespaceAnnotation::Command(argument_presets) => {
            result.extend(analyze_argument_presets(argument_presets));
        }
        schema::NamespaceAnnotation::Model {
            filter,
            argument_presets,
        } => {
            result.extend(analyze_filter_permission(filter));
            result.extend(analyze_argument_presets(argument_presets));
        }
        schema::NamespaceAnnotation::InputFieldPresets {
            presets_fields,
            type_name,
        } => result.push(OpenddObject::Permission(PermissionUsage::FieldPresets(
            FieldPresetsUsage {
                fields: presets_fields
                    .iter()
                    .map(
                        |(
                            field_name,
                            FieldPresetInfo {
                                value: _,
                                deprecated,
                            },
                        )| FieldUsage {
                            name: field_name.to_owned(),
                            opendd_type: type_name.clone(),
                            deprecated: deprecated.to_owned(),
                        },
                    )
                    .collect(),
            },
        ))),
        schema::NamespaceAnnotation::NodeFieldTypeMappings(_)
        | schema::NamespaceAnnotation::EntityTypeMappings(_) => {}
    }
    result
}

fn analyze_argument_presets(argument_presets: &schema::ArgumentPresets) -> Option<OpenddObject> {
    let mut arguments = Vec::new();
    for argument in argument_presets.argument_presets.keys() {
        if let Some(connector_argument) = &argument.ndc_argument_name {
            // Only top level arguments are collected
            if argument.field_path.is_empty() {
                arguments.push(connector_argument.to_owned());
            }
        }
    }

    if arguments.is_empty() {
        None
    } else {
        Some(OpenddObject::Permission(PermissionUsage::ArgumentPresets(
            ArgumentPresetsUsage { arguments },
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
        } => fields.push(FieldUsage {
            name: field.to_owned(),
            opendd_type: field_parent_type.to_owned(),
            deprecated: deprecated.to_owned(),
        }),
        ModelPredicate::Relationship {
            relationship_info,
            predicate,
        } => {
            relationships.push(PredicateRelationshipUsage {
                name: relationship_info.relationship_name.clone(),
                source: relationship_info.source_type.clone(),
                target: RelationshipTarget::Model {
                    model_name: relationship_info.target_model_name.clone(),
                    relationship_type: relationship_info.relationship_type.clone(),
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
