use crate::stages::{
    boolean_expressions, graphql_config, object_relationships, scalar_boolean_expressions,
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

/// Track the root fields of the GraphQL schema while resolving the metadata.
/// This is used to ensure that the schema has unique root fields for Query, Mutation and Subscription.
// NOTE: The `ast::Name` is cheap to clone, so storing them directly without references
pub struct TrackGraphQLRootFields {
    pub query: BTreeSet<ast::Name>,
    pub mutation: BTreeSet<ast::Name>,
    pub subscription: BTreeSet<ast::Name>,
}

#[derive(Debug, thiserror::Error)]
pub enum DuplicateRootFieldError {
    #[error("Cannot add query root field {0} as it already in use")]
    Query(ast::Name),
    #[error("Cannot add mutation root field {0} as it already in use")]
    Mutation(ast::Name),
    #[error("Cannot add subscription root field {0} as it already in use")]
    Subscription(ast::Name),
}

impl TrackGraphQLRootFields {
    pub fn new() -> Self {
        Self {
            query: BTreeSet::new(),
            mutation: BTreeSet::new(),
            subscription: BTreeSet::new(),
        }
    }
    pub fn track_query_root_field(
        &mut self,
        name: &ast::Name,
    ) -> Result<(), DuplicateRootFieldError> {
        if !self.query.insert(name.clone()) {
            return Err(DuplicateRootFieldError::Query(name.clone()));
        }
        Ok(())
    }
    pub fn track_mutation_root_field(
        &mut self,
        name: &ast::Name,
    ) -> Result<(), DuplicateRootFieldError> {
        if !self.mutation.insert(name.clone()) {
            return Err(DuplicateRootFieldError::Mutation(name.clone()));
        }
        Ok(())
    }

    #[allow(dead_code)] // to be used later
    pub fn track_subscription_root_field(
        &mut self,
        name: &ast::Name,
    ) -> Result<(), DuplicateRootFieldError> {
        if !self.subscription.insert(name.clone()) {
            return Err(DuplicateRootFieldError::Subscription(name.clone()));
        }
        Ok(())
    }
}

#[derive(Debug)]
/// we do not want to store our types like this, but occasionally it is useful
/// for pattern matching
pub enum TypeRepresentation<'a, ObjectType, ScalarType> {
    Scalar(&'a ScalarType),
    Object(&'a ObjectType),
    /// New object boolean expression type
    BooleanExpressionObject(&'a boolean_expressions::ResolvedObjectBooleanExpressionType),
    /// New scalar boolean expression type
    BooleanExpressionScalar(&'a scalar_boolean_expressions::ResolvedScalarBooleanExpressionType),
}

/// validate whether a given CustomTypeName exists within `object_types`, `scalar_types` or
/// `object_boolean_expression_types`
pub fn get_type_representation<'a, ObjectType, ScalarType>(
    custom_type_name: &Qualified<CustomTypeName>,
    object_types: &'a BTreeMap<Qualified<CustomTypeName>, ObjectType>,
    scalar_types: &'a BTreeMap<Qualified<CustomTypeName>, ScalarType>,
    boolean_expression_types: &'a boolean_expressions::BooleanExpressionTypes,
) -> Result<TypeRepresentation<'a, ObjectType, ScalarType>, Error> {
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
                .get(
                    &boolean_expressions::BooleanExpressionTypeIdentifier::FromBooleanExpressionType(
                        custom_type_name.clone(),
                    ),
                )
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
        object_relationships::ObjectTypeWithRelationships,
    >,
) -> Result<&'a object_relationships::ObjectTypeWithRelationships, Error> {
    object_types
        .get(&boolean_expression_type.object_type)
        .ok_or(Error::from(
        boolean_expressions::BooleanExpressionError::UnsupportedTypeInObjectBooleanExpressionType {
            type_name: boolean_expression_type.object_type.clone(),
            boolean_expression_type_name: boolean_expression_type.name.clone(),
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
