mod types;
use crate::stages::{
    boolean_expressions, data_connector_scalar_types, data_connectors, models_graphql,
    object_boolean_expressions, object_relationships, scalar_types,
};
use indexmap::IndexMap;
use open_dds::{data_connector::DataConnectorName, models::ModelName, types::CustomTypeName};
use std::collections::BTreeMap;
pub use types::{
    FilterPermission, ModelPredicate, ModelTargetSource, ModelWithPermissions, SelectPermission,
    UnaryComparisonOperator,
};
mod model_permission;
pub(crate) use model_permission::resolve_model_predicate_with_type;

use crate::types::error::Error;

use crate::types::subgraph::Qualified;

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
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    flags: &open_dds::flags::Flags,
) -> Result<IndexMap<Qualified<ModelName>, ModelWithPermissions>, Error> {
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

    // Note: Model permissions's predicate can include the relationship field,
    // hence Model permissions should be resolved after the relationships of a
    // model is resolved.
    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: permissions,
    } in &metadata_accessor.model_permissions
    {
        let model_name = Qualified::new(subgraph.clone(), permissions.model_name.clone());
        let model = models_with_permissions
            .get_mut(&model_name)
            .ok_or_else(|| Error::UnknownModelInModelSelectPermissions {
                model_name: model_name.clone(),
            })?;

        if model.select_permissions.is_empty() {
            // `boolean_expression_fields` is Some for new `BooleanExpressionType` but None for
            // old `ObjectBooleanExpressionType`.
            let boolean_expression_fields =
                model
                    .filter_expression_type
                    .as_ref()
                    .and_then(|filter| match filter {
                        models_graphql::ModelExpressionType::BooleanExpressionType(
                            boolean_expression_type,
                        ) => boolean_expression_type.get_fields(flags),
                        models_graphql::ModelExpressionType::ObjectBooleanExpressionType(_) => None,
                    });

            let select_permissions = model_permission::resolve_model_select_permissions(
                &metadata_accessor.flags,
                &model.model,
                subgraph,
                permissions,
                boolean_expression_fields,
                data_connectors,
                data_connector_scalars,
                object_types,
                scalar_types,
                models, // This is required to get the model for the relationship target
                object_boolean_expression_types,
                boolean_expression_types,
            )?;

            model.select_permissions = select_permissions;
        } else {
            return Err(Error::DuplicateModelSelectPermission {
                model_name: model_name.clone(),
            });
        }
    }
    Ok(models_with_permissions)
}
