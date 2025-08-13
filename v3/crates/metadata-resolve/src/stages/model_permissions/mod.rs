mod error;
mod predicate;
mod types;
use crate::Conditions;
use crate::stages::{
    boolean_expressions, data_connector_scalar_types, models_graphql, object_relationships,
    scalar_types,
};
pub use error::{ModelPermissionError, NamedModelPermissionError};
use indexmap::IndexMap;
use open_dds::identifier::SubgraphName;
use open_dds::{data_connector::DataConnectorName, models::ModelName, types::CustomTypeName};
use std::collections::BTreeMap;
use types::ModelPermissions;
pub use types::{
    FilterPermission, ModelAuthorizationRule, ModelPermissionIssue, ModelPermissionsOutput,
    ModelPredicate, ModelTargetSource, ModelWithPermissions, PredicateRelationshipInfo,
    RelationalDeletePermission, RelationalInsertPermission, RelationalOperation,
    RelationalUpdatePermission, SelectPermission, UnaryComparisonOperator,
};
mod model_permission;
pub(crate) use predicate::resolve_model_predicate_with_type;

use crate::types::error::Error;

use crate::types::subgraph::Qualified;

use super::data_connectors;

/// resolve model permissions
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    conditions: &mut Conditions,
) -> Result<ModelPermissionsOutput, Vec<Error>> {
    let mut issues = Vec::new();
    let mut models_with_permissions: IndexMap<Qualified<ModelName>, ModelWithPermissions> = models
        .iter()
        .map(|(model_name, model)| {
            (
                model_name.clone(),
                ModelWithPermissions {
                    model: model.inner.clone(),
                    arguments: model.arguments.clone(),
                    filter_expression_type: model.filter_expression_type.clone(),
                    graphql_api: model.graphql_api.clone(),
                    permissions: ModelPermissions::new(),
                    description: model.description.clone(),
                },
            )
        })
        .collect();

    let mut results = vec![];

    // Note: Model permissions's predicate can include the relationship field,
    // hence Model permissions should be resolved after the relationships of a
    // model is resolved.
    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: permissions,
    } in &metadata_accessor.model_permissions
    {
        results.push(resolve_model_permissions(
            metadata_accessor,
            subgraph,
            data_connectors,
            data_connector_scalars,
            object_types,
            scalar_types,
            models,
            boolean_expression_types,
            permissions,
            &mut models_with_permissions,
            conditions,
            &mut issues,
        ));
    }

    partition_eithers::collect_any_errors(results).map(|_| ModelPermissionsOutput {
        permissions: models_with_permissions,
        issues,
    })
}

fn resolve_model_permissions(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    subgraph: &SubgraphName,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    permissions: &open_dds::permissions::ModelPermissionsV2,
    models_with_permissions: &mut IndexMap<Qualified<ModelName>, ModelWithPermissions>,
    conditions: &mut Conditions,
    issues: &mut Vec<ModelPermissionIssue>,
) -> Result<(), Error> {
    let model_name =
        Qualified::new(subgraph.clone(), permissions.model_name.clone()).transpose_spanned();

    let input_model =
        models
            .get(&model_name.value)
            .ok_or_else(|| Error::UnknownModelInModelPermissions {
                model_name: model_name.clone(),
            })?;

    let model = models_with_permissions
        .get_mut(&model_name.value)
        .ok_or_else(|| Error::UnknownModelInModelPermissions {
            model_name: model_name.clone(),
        })?;

    if model.permissions.is_empty() {
        let boolean_expression = model
            .filter_expression_type
            .as_ref()
            .map(derive_more::AsRef::as_ref);

        let permissions = model_permission::resolve_all_model_permissions(
            &metadata_accessor.flags,
            input_model,
            permissions,
            boolean_expression,
            data_connectors,
            data_connector_scalars,
            object_types,
            scalar_types,
            models,
            boolean_expression_types,
            conditions,
            issues,
        )?;

        model.permissions = permissions;
    } else {
        return Err(Error::DuplicateModelPermissions {
            model_name: model_name.clone(),
        });
    }
    Ok(())
}
