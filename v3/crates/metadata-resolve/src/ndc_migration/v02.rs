use ndc_models as ndc_models_v02;

pub fn migrate_schema_response_from_v01(
    old_schema: ndc_models_v01::SchemaResponse,
) -> ndc_models_v02::SchemaResponse {
    ndc_models_v02::SchemaResponse {
        scalar_types: old_schema
            .scalar_types
            .into_iter()
            .map(|(name, old_scalar_type)| {
                (
                    migrate_scalar_type_name_from_v01(name),
                    migrate_scalar_type_from_v01(old_scalar_type),
                )
            })
            .collect(),
        object_types: old_schema
            .object_types
            .into_iter()
            .map(|(name, old_object_type)| {
                (
                    migrate_object_type_name_from_v01(name),
                    migrate_object_type_from_v01(old_object_type),
                )
            })
            .collect(),
        collections: old_schema
            .collections
            .into_iter()
            .map(migrate_collection_info_from_v01)
            .collect(),
        functions: old_schema
            .functions
            .into_iter()
            .map(migrate_function_info_from_v01)
            .collect(),
        procedures: old_schema
            .procedures
            .into_iter()
            .map(migrate_procedure_info_from_v01)
            .collect(),
        capabilities: None, // v0.1.x did not have schema capabilities
    }
}

pub fn migrate_scalar_type_from_v01(
    old_scalar_type: ndc_models_v01::ScalarType,
) -> ndc_models_v02::ScalarType {
    ndc_models_v02::ScalarType {
        aggregate_functions: old_scalar_type
            .aggregate_functions
            .into_iter()
            .map(|(name, old_aggregate_fn)| {
                (
                    ndc_models_v02::AggregateFunctionName::new(name.into_inner()),
                    migrate_aggregate_function_definition_from_v01(old_aggregate_fn),
                )
            })
            .collect(),
        representation: old_scalar_type
            .representation
            .map(migrate_type_representation_from_v01),
        comparison_operators: old_scalar_type
            .comparison_operators
            .into_iter()
            .map(|(name, old_comparison)| {
                (
                    ndc_models_v02::ComparisonOperatorName::new(name.into_inner()),
                    migrate_comparison_operation_definition_from_v01(old_comparison),
                )
            })
            .collect(),
    }
}

pub fn migrate_comparison_operation_definition_from_v01(
    old_comparison: ndc_models_v01::ComparisonOperatorDefinition,
) -> ndc_models_v02::ComparisonOperatorDefinition {
    match old_comparison {
        ndc_models_v01::ComparisonOperatorDefinition::Equal => {
            ndc_models_v02::ComparisonOperatorDefinition::Equal
        }
        ndc_models_v01::ComparisonOperatorDefinition::In => {
            ndc_models_v02::ComparisonOperatorDefinition::In
        }
        ndc_models_v01::ComparisonOperatorDefinition::Custom { argument_type } => {
            ndc_models_v02::ComparisonOperatorDefinition::Custom {
                argument_type: migrate_type_from_v01(argument_type),
            }
        }
    }
}

#[allow(deprecated)]
pub fn migrate_type_representation_from_v01(
    old_type_repr: ndc_models_v01::TypeRepresentation,
) -> ndc_models_v02::TypeRepresentation {
    match old_type_repr {
        ndc_models_v01::TypeRepresentation::Boolean => ndc_models_v02::TypeRepresentation::Boolean,
        ndc_models_v01::TypeRepresentation::String => ndc_models_v02::TypeRepresentation::String,
        ndc_models_v01::TypeRepresentation::Number => ndc_models_v02::TypeRepresentation::Number,
        ndc_models_v01::TypeRepresentation::Integer => ndc_models_v02::TypeRepresentation::Integer,
        ndc_models_v01::TypeRepresentation::Int8 => ndc_models_v02::TypeRepresentation::Int8,
        ndc_models_v01::TypeRepresentation::Int16 => ndc_models_v02::TypeRepresentation::Int16,
        ndc_models_v01::TypeRepresentation::Int32 => ndc_models_v02::TypeRepresentation::Int32,
        ndc_models_v01::TypeRepresentation::Int64 => ndc_models_v02::TypeRepresentation::Int64,
        ndc_models_v01::TypeRepresentation::Float32 => ndc_models_v02::TypeRepresentation::Float32,
        ndc_models_v01::TypeRepresentation::Float64 => ndc_models_v02::TypeRepresentation::Float64,
        ndc_models_v01::TypeRepresentation::BigInteger => {
            ndc_models_v02::TypeRepresentation::BigInteger
        }
        ndc_models_v01::TypeRepresentation::BigDecimal => {
            ndc_models_v02::TypeRepresentation::BigDecimal
        }
        ndc_models_v01::TypeRepresentation::UUID => ndc_models_v02::TypeRepresentation::UUID,
        ndc_models_v01::TypeRepresentation::Date => ndc_models_v02::TypeRepresentation::Date,
        ndc_models_v01::TypeRepresentation::Timestamp => {
            ndc_models_v02::TypeRepresentation::Timestamp
        }
        ndc_models_v01::TypeRepresentation::TimestampTZ => {
            ndc_models_v02::TypeRepresentation::TimestampTZ
        }
        ndc_models_v01::TypeRepresentation::Geography => {
            ndc_models_v02::TypeRepresentation::Geography
        }
        ndc_models_v01::TypeRepresentation::Geometry => {
            ndc_models_v02::TypeRepresentation::Geometry
        }
        ndc_models_v01::TypeRepresentation::Bytes => ndc_models_v02::TypeRepresentation::Bytes,
        ndc_models_v01::TypeRepresentation::JSON => ndc_models_v02::TypeRepresentation::JSON,
        ndc_models_v01::TypeRepresentation::Enum { one_of } => {
            ndc_models_v02::TypeRepresentation::Enum { one_of }
        }
    }
}

pub fn migrate_aggregate_function_definition_from_v01(
    old_aggregate_fn: ndc_models_v01::AggregateFunctionDefinition,
) -> ndc_models_v02::AggregateFunctionDefinition {
    ndc_models_v02::AggregateFunctionDefinition {
        result_type: migrate_type_from_v01(old_aggregate_fn.result_type),
    }
}

pub fn migrate_object_type_from_v01(
    old_object_type: ndc_models_v01::ObjectType,
) -> ndc_models_v02::ObjectType {
    ndc_models_v02::ObjectType {
        description: old_object_type.description,
        fields: old_object_type
            .fields
            .into_iter()
            .map(|(name, old_field)| {
                (
                    ndc_models_v02::FieldName::new(name.into_inner()),
                    map_object_field_from_v01(old_field),
                )
            })
            .collect(),
    }
}

pub fn map_object_field_from_v01(
    old_field: ndc_models_v01::ObjectField,
) -> ndc_models_v02::ObjectField {
    ndc_models_v02::ObjectField {
        arguments: old_field
            .arguments
            .into_iter()
            .map(|(name, old_argument)| {
                (
                    ndc_models_v02::ArgumentName::new(name.into_inner()),
                    migrate_argument_info_from_v01(old_argument),
                )
            })
            .collect(),
        description: old_field.description,
        r#type: migrate_type_from_v01(old_field.r#type),
    }
}

fn migrate_argument_info_from_v01(
    old_argument: ndc_models_v01::ArgumentInfo,
) -> ndc_models_v02::ArgumentInfo {
    ndc_models_v02::ArgumentInfo {
        argument_type: migrate_type_from_v01(old_argument.argument_type),
        description: old_argument.description,
    }
}

pub fn migrate_type_from_v01(old_type: ndc_models_v01::Type) -> ndc_models_v02::Type {
    match old_type {
        ndc_models_v01::Type::Named { name } => ndc_models_v02::Type::Named {
            name: ndc_models_v02::TypeName::new(name.into_inner()),
        },
        ndc_models_v01::Type::Nullable { underlying_type } => ndc_models_v02::Type::Nullable {
            underlying_type: Box::new(migrate_type_from_v01(*underlying_type)),
        },
        ndc_models_v01::Type::Array { element_type } => ndc_models_v02::Type::Array {
            element_type: Box::new(migrate_type_from_v01(*element_type)),
        },
        ndc_models_v01::Type::Predicate { object_type_name } => ndc_models_v02::Type::Predicate {
            object_type_name: migrate_object_type_name_from_v01(object_type_name),
        },
    }
}

fn migrate_collection_info_from_v01(
    old_collection: ndc_models_v01::CollectionInfo,
) -> ndc_models_v02::CollectionInfo {
    ndc_models_v02::CollectionInfo {
        arguments: old_collection
            .arguments
            .into_iter()
            .map(|(name, old_argument)| {
                (
                    ndc_models_v02::ArgumentName::new(name.into_inner()),
                    migrate_argument_info_from_v01(old_argument),
                )
            })
            .collect(),
        name: ndc_models_v02::CollectionName::new(old_collection.name.into_inner()),
        description: old_collection.description,
        collection_type: migrate_object_type_name_from_v01(old_collection.collection_type),
        uniqueness_constraints: old_collection
            .uniqueness_constraints
            .into_iter()
            .map(|(name, old_constraint)| {
                (name, migrate_uniqueness_constraint_from_v01(old_constraint))
            })
            .collect(),
        foreign_keys: old_collection
            .foreign_keys
            .into_iter()
            .map(|(name, old_constraint)| {
                (
                    name,
                    migrate_foreign_key_constraint_from_v01(old_constraint),
                )
            })
            .collect(),
    }
}

fn migrate_uniqueness_constraint_from_v01(
    old_constraint: ndc_models_v01::UniquenessConstraint,
) -> ndc_models_v02::UniquenessConstraint {
    ndc_models_v02::UniquenessConstraint {
        unique_columns: old_constraint
            .unique_columns
            .into_iter()
            .map(|name| ndc_models_v02::FieldName::new(name.into_inner()))
            .collect(),
    }
}

fn migrate_foreign_key_constraint_from_v01(
    old_constraint: ndc_models_v01::ForeignKeyConstraint,
) -> ndc_models_v02::ForeignKeyConstraint {
    ndc_models_v02::ForeignKeyConstraint {
        column_mapping: old_constraint
            .column_mapping
            .into_iter()
            .map(|(k, v)| {
                (
                    ndc_models_v02::FieldName::new(k.into_inner()),
                    ndc_models_v02::FieldName::new(v.into_inner()),
                )
            })
            .collect(),
        foreign_collection: ndc_models_v02::CollectionName::new(
            old_constraint.foreign_collection.into_inner(),
        ),
    }
}

fn migrate_function_info_from_v01(
    old_function: ndc_models_v01::FunctionInfo,
) -> ndc_models_v02::FunctionInfo {
    ndc_models_v02::FunctionInfo {
        arguments: old_function
            .arguments
            .into_iter()
            .map(|(name, old_argument)| {
                (
                    ndc_models_v02::ArgumentName::new(name.into_inner()),
                    migrate_argument_info_from_v01(old_argument),
                )
            })
            .collect(),
        name: migrate_function_name_from_v01(old_function.name),
        description: old_function.description,
        result_type: migrate_type_from_v01(old_function.result_type),
    }
}

fn migrate_procedure_info_from_v01(
    old_procedure: ndc_models_v01::ProcedureInfo,
) -> ndc_models_v02::ProcedureInfo {
    ndc_models_v02::ProcedureInfo {
        arguments: old_procedure
            .arguments
            .into_iter()
            .map(|(name, old_argument)| {
                (
                    ndc_models_v02::ArgumentName::new(name.into_inner()),
                    migrate_argument_info_from_v01(old_argument),
                )
            })
            .collect(),
        name: ndc_models_v02::ProcedureName::new(old_procedure.name.into_inner()),
        description: old_procedure.description,
        result_type: migrate_type_from_v01(old_procedure.result_type),
    }
}

pub fn migrate_capabilities_from_v01(
    old_capabilities: ndc_models_v01::Capabilities,
) -> ndc_models_v02::Capabilities {
    ndc_models_v02::Capabilities {
        query: migrate_query_capabilities_from_v01(old_capabilities.query),
        mutation: migrate_mutation_capabilities_from_v01(old_capabilities.mutation),
        relationships: old_capabilities
            .relationships
            .map(migrate_relationship_capabilities_from_v01),
    }
}

fn migrate_query_capabilities_from_v01(
    old_query_capabilities: ndc_models_v01::QueryCapabilities,
) -> ndc_models_v02::QueryCapabilities {
    ndc_models_v02::QueryCapabilities {
        aggregates: old_query_capabilities
            .aggregates
            .map(migrate_aggregate_capabilities_from_v01),
        variables: old_query_capabilities
            .variables
            .map(migrate_leaf_capability_from_v01),
        explain: old_query_capabilities
            .explain
            .map(migrate_leaf_capability_from_v01),
        nested_fields: migrate_nested_field_capabilities_from_v01(
            old_query_capabilities.nested_fields,
        ),
        exists: migrate_exists_capabilities_from_v01(old_query_capabilities.exists),
    }
}

#[allow(clippy::needless_pass_by_value)] // We want the value to be consumed and discarded here
fn migrate_aggregate_capabilities_from_v01(
    _old_leaf_capability: ndc_models_v01::LeafCapability,
) -> ndc_models_v02::AggregateCapabilities {
    ndc_models_v02::AggregateCapabilities {
        filter_by: None, // v0.1.x does not support filtering by aggregate predicates
        group_by: None,  // v0.1.x does not support group by
    }
}

#[allow(clippy::needless_pass_by_value)] // We want the value to be consumed and discarded here
fn migrate_leaf_capability_from_v01(
    _old_leaf_capability: ndc_models_v01::LeafCapability,
) -> ndc_models_v02::LeafCapability {
    ndc_models_v02::LeafCapability {}
}

fn migrate_exists_capabilities_from_v01(
    old_exists_capabilities: ndc_models_v01::ExistsCapabilities,
) -> ndc_models_v02::ExistsCapabilities {
    ndc_models_v02::ExistsCapabilities {
        named_scopes: None, // v0.1.x does not have named scopes
        unrelated: Some(ndc_models_v02::LeafCapability {}), // v0.1.x assumed this capability
        nested_collections: old_exists_capabilities
            .nested_collections
            .map(migrate_leaf_capability_from_v01),
    }
}

fn migrate_nested_field_capabilities_from_v01(
    old_nested_field_capabilities: ndc_models_v01::NestedFieldCapabilities,
) -> ndc_models_v02::NestedFieldCapabilities {
    ndc_models_v02::NestedFieldCapabilities {
        filter_by: old_nested_field_capabilities
            .filter_by
            .map(migrate_leaf_capability_from_v01),
        order_by: old_nested_field_capabilities
            .order_by
            .map(migrate_leaf_capability_from_v01),
        aggregates: old_nested_field_capabilities
            .aggregates
            .map(migrate_leaf_capability_from_v01),
        nested_collections: None, // v0.1.x does not support nested field collections
    }
}

fn migrate_mutation_capabilities_from_v01(
    old_mutation_capabilities: ndc_models_v01::MutationCapabilities,
) -> ndc_models_v02::MutationCapabilities {
    ndc_models_v02::MutationCapabilities {
        transactional: old_mutation_capabilities
            .transactional
            .map(migrate_leaf_capability_from_v01),
        explain: old_mutation_capabilities
            .explain
            .map(migrate_leaf_capability_from_v01),
    }
}

fn migrate_relationship_capabilities_from_v01(
    old_relationship_capabilities: ndc_models_v01::RelationshipCapabilities,
) -> ndc_models_v02::RelationshipCapabilities {
    ndc_models_v02::RelationshipCapabilities {
        relation_comparisons: old_relationship_capabilities
            .relation_comparisons
            .map(migrate_leaf_capability_from_v01),
        order_by_aggregate: old_relationship_capabilities
            .order_by_aggregate
            .map(migrate_leaf_capability_from_v01),
    }
}

fn migrate_scalar_type_name_from_v01(
    old_name: ndc_models_v01::ScalarTypeName,
) -> ndc_models_v02::ScalarTypeName {
    ndc_models_v02::ScalarTypeName::new(ndc_models_v02::TypeName::new(
        old_name.into_inner().into_inner(),
    ))
}

fn migrate_object_type_name_from_v01(
    old_name: ndc_models_v01::ObjectTypeName,
) -> ndc_models_v02::ObjectTypeName {
    ndc_models_v02::ObjectTypeName::new(ndc_models_v02::TypeName::new(
        old_name.into_inner().into_inner(),
    ))
}

fn migrate_function_name_from_v01(
    old_name: ndc_models_v01::FunctionName,
) -> ndc_models_v02::FunctionName {
    ndc_models_v02::FunctionName::new(ndc_models_v02::CollectionName::new(
        old_name.into_inner().into_inner(),
    ))
}
