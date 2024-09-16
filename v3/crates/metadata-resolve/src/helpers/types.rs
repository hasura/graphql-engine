use crate::stages::{
    boolean_expressions, graphql_config, object_boolean_expressions, relationships,
    scalar_boolean_expressions, scalar_types,
};
use crate::types::error::Error;

use crate::types::subgraph::{
    Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference,
};
use lang_graphql::ast::common as ast;

use open_dds::{
    data_connector::{DataConnectorColumnName, DataConnectorOperatorName},
    types::CustomTypeName,
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::str::FromStr;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub struct NdcColumnForComparison {
    pub column: DataConnectorColumnName,
    pub equal_operator: DataConnectorOperatorName,
}

/// try to add `new_graphql_type` to `existing_graphql_types`, returning an error
/// if there is a name conflict
pub fn store_new_graphql_type(
    existing_graphql_types: &mut BTreeSet<ast::TypeName>,
    new_graphql_type: Option<&ast::TypeName>,
) -> Result<(), graphql_config::GraphqlConfigError> {
    if let Some(new_graphql_type) = new_graphql_type {
        // Fail on conflicting graphql type names
        if !(existing_graphql_types.insert(new_graphql_type.clone())) {
            return Err(graphql_config::GraphqlConfigError::ConflictingGraphQlType {
                graphql_type_name: new_graphql_type.clone(),
            });
        }
    }
    Ok(())
}

#[derive(Debug)]
/// we do not want to store our types like this, but occasionally it is useful
/// for pattern matching
pub enum TypeRepresentation<'a, ObjectType> {
    Scalar(&'a scalar_types::ScalarTypeRepresentation),
    Object(&'a ObjectType),
    /// The old expression of boolean expression types
    BooleanExpression(&'a object_boolean_expressions::ObjectBooleanExpressionType),
    /// New object boolean expression type
    BooleanExpressionObject(&'a boolean_expressions::ResolvedObjectBooleanExpressionType),
    /// New scalar boolean expression type
    BooleanExpressionScalar(&'a scalar_boolean_expressions::ResolvedScalarBooleanExpressionType),
}

/// validate whether a given CustomTypeName exists within `object_types`, `scalar_types` or
/// `object_boolean_expression_types`
pub fn get_type_representation<'a, ObjectType>(
    custom_type_name: &Qualified<CustomTypeName>,
    object_types: &'a BTreeMap<Qualified<CustomTypeName>, ObjectType>,
    scalar_types: &'a BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &'a BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &'a boolean_expressions::BooleanExpressionTypes,
) -> Result<TypeRepresentation<'a, ObjectType>, Error> {
    object_types
        .get(custom_type_name)
        .map(|object_type_representation| TypeRepresentation::Object(object_type_representation))
        .or_else(|| {
            scalar_types
                .get(custom_type_name)
                .map(|scalar_type_representation| {
                    TypeRepresentation::Scalar(scalar_type_representation)
                })
        })
        .or_else(|| {
            object_boolean_expression_types.get(custom_type_name).map(
                |object_boolean_expression_type| {
                    TypeRepresentation::BooleanExpression(object_boolean_expression_type)
                },
            )
        })
        .or_else(|| {
            boolean_expression_types
                .objects
                .get(custom_type_name)
                .map(|boolean_expression_type| {
                    TypeRepresentation::BooleanExpressionObject(boolean_expression_type)
                })
        })
        .or_else(|| {
            boolean_expression_types
                .scalars
                .get(custom_type_name)
                .map(|boolean_expression_type| {
                    TypeRepresentation::BooleanExpressionScalar(boolean_expression_type)
                })
        })
        .ok_or_else(|| Error::UnknownType {
            data_type: custom_type_name.clone(),
        })
}

pub(crate) fn get_object_type_for_boolean_expression<'a>(
    boolean_expression_type: &boolean_expressions::ResolvedObjectBooleanExpressionType,
    object_types: &'a BTreeMap<
        Qualified<CustomTypeName>,
        relationships::ObjectTypeWithRelationships,
    >,
) -> Result<&'a relationships::ObjectTypeWithRelationships, Error> {
    object_types
        .get(&boolean_expression_type.object_type)
        .ok_or(Error::from(
        boolean_expressions::BooleanExpressionError::UnsupportedTypeInObjectBooleanExpressionType {
            type_name: boolean_expression_type.object_type.clone(),
        },
    ))
}

pub(crate) fn get_object_type_for_object_boolean_expression<'a>(
    object_boolean_expression_type: &object_boolean_expressions::ObjectBooleanExpressionType,
    object_types: &'a BTreeMap<
        Qualified<CustomTypeName>,
        relationships::ObjectTypeWithRelationships,
    >,
) -> Result<&'a relationships::ObjectTypeWithRelationships, Error> {
    object_types
        .get(&object_boolean_expression_type.object_type)
        .ok_or(Error::from(
            boolean_expressions::BooleanExpressionError::UnsupportedTypeInObjectBooleanExpressionType {
                type_name: object_boolean_expression_type.object_type.clone(),
            },
        ))
}

// Get the underlying object type by resolving Custom ObjectType, Array and
// Nullable container types
// check that `custom_type_name` exists in `object_types`
pub fn object_type_exists<ObjectType>(
    custom_type_name: &Qualified<CustomTypeName>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectType>,
) -> Result<Qualified<CustomTypeName>, Error> {
    object_types
        .get(custom_type_name)
        .map(|_| custom_type_name.clone())
        .ok_or_else(|| Error::UnknownObjectType {
            data_type: custom_type_name.clone(),
        })
}

/// given a type like `Thing!` or `[Thing!]!` - try and extract `Thing`
pub fn unwrap_custom_type_name(
    type_reference: &QualifiedTypeReference,
) -> Option<&Qualified<CustomTypeName>> {
    match &type_reference.underlying_type {
        QualifiedBaseType::List(inner_type) => unwrap_custom_type_name(inner_type),
        QualifiedBaseType::Named(type_name) => match type_name {
            QualifiedTypeName::Inbuilt(_) => None,
            QualifiedTypeName::Custom(custom_type_name) => Some(custom_type_name),
        },
    }
}

pub fn unwrap_qualified_type_name(type_reference: &QualifiedTypeReference) -> &QualifiedTypeName {
    match &type_reference.underlying_type {
        QualifiedBaseType::List(inner_type) => unwrap_qualified_type_name(inner_type),
        QualifiedBaseType::Named(type_name) => type_name,
    }
}

/// Helper function to create GraphQL compliant name
pub fn mk_name(name: &str) -> Result<ast::Name, graphql_config::GraphqlConfigError> {
    ast::Name::from_str(name).map_err(|_| graphql_config::GraphqlConfigError::InvalidGraphQlName {
        name: name.to_string(),
    })
}
