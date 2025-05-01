mod error;
mod predicate;
mod types;
use crate::stages::{
    boolean_expressions, data_connector_scalar_types, models_graphql, object_relationships,
    scalar_types,
};
pub use error::{ModelPermissionError, NamedModelPermissionError};
use indexmap::IndexMap;
use open_dds::identifier::SubgraphName;
use open_dds::{data_connector::DataConnectorName, models::ModelName, types::CustomTypeName};
use std::collections::BTreeMap;
pub use types::{
    FilterPermission, ModelPermissionIssue, ModelPermissionsOutput, ModelPredicate,
    ModelTargetSource, ModelWithPermissions, PredicateRelationshipInfo, SelectPermission,
    UnaryComparisonOperator,
};
mod model_permission;
pub(crate) use predicate::resolve_model_predicate_with_type;

use crate::types::error::Error;

use crate::types::subgraph::Qualified;

/// resolve model permissions
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
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
) -> Result<ModelPermissionsOutput, Vec<Error>> {
    let mut issues = Vec::new();
    let mut models_with_permissions: IndexMap<Qualified<ModelName>, ModelWithPermissions> = models
        .iter()
        .map(|(model_name, model)| {
            (
                model_name.clone(),
                ModelWithPermissions {
                    model: model.inner.clone(),
                    filter_expression_type: model.filter_expression_type.clone(),
                    graphql_api: model.graphql_api.clone(),
                    select_permissions: BTreeMap::new(),
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
            data_connector_scalars,
            object_types,
            scalar_types,
            models,
            boolean_expression_types,
            &mut models_with_permissions,
            permissions,
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
    models_with_permissions: &mut IndexMap<Qualified<ModelName>, ModelWithPermissions>,
    permissions: &open_dds::permissions::ModelPermissionsV1,
    issues: &mut Vec<ModelPermissionIssue>,
) -> Result<(), Error> {
    let model_name =
        Qualified::new(subgraph.clone(), permissions.model_name.clone()).transpose_spanned();

    let model = models_with_permissions
        .get_mut(&model_name.value)
        .ok_or_else(|| Error::UnknownModelInModelPermissions {
            model_name: model_name.clone(),
        })?;

    if model.select_permissions.is_empty() {
        let boolean_expression = model.filter_expression_type.as_ref();

        let select_permissions = model_permission::resolve_all_model_select_permissions(
            &metadata_accessor.flags,
            &model.model,
            permissions,
            boolean_expression,
            data_connector_scalars,
            object_types,
            scalar_types,
            models, // This is required to get the model for the relationship target
            boolean_expression_types,
            issues,
        )?;

        model.select_permissions = select_permissions;
    } else {
        return Err(Error::DuplicateModelPermissions {
            model_name: model_name.clone(),
        });
    }
    Ok(())
}
