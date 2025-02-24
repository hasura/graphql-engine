use hasura_authn_core::SessionVariables;
use lang_graphql::schema;
use std::collections::BTreeMap;

use open_dds::types::CustomTypeName;

use crate::error;
use graphql_schema::GDS;
use metadata_resolve::{ObjectTypeWithRelationships, Qualified};
use plan::process_model_predicate;
use plan_types::{Expression, UsagesCounts};

/// Fetch filter expression from the namespace annotation
/// of the field call. If the filter predicate namespace annotation
/// is not found, then an error will be thrown.
pub(crate) fn get_select_filter_predicate<'s>(
    node_info: &schema::NodeInfo<'s, GDS>,
) -> Result<&'s metadata_resolve::FilterPermission, error::Error> {
    node_info
        .namespaced
        .as_ref()
        .and_then(|annotation| match annotation {
            graphql_schema::NamespaceAnnotation::Model { filter, .. } => Some(filter),
            graphql_schema::NamespaceAnnotation::NodeFieldTypeMappings(_)
            | graphql_schema::NamespaceAnnotation::EntityTypeMappings(_)
            | graphql_schema::NamespaceAnnotation::Command(_)
            | graphql_schema::NamespaceAnnotation::InputFieldPresets { .. } => None,
        })
        // If we're hitting this case, it means that the caller of this
        // function expects a filter predicate, but it was not annotated
        // when the V3 engine metadata was built
        .ok_or(error::Error::Internal(error::InternalError::Engine(
            error::InternalEngineError::ExpectedNamespaceAnnotationNotFound {
                namespace_annotation_type: "Filter".to_string(),
            },
        )))
}

pub fn build_model_permissions_filter_predicate<'s>(
    model_data_connector_link: &'s metadata_resolve::DataConnectorLink,
    model_type_mappings: &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    permissions_predicate: &'s metadata_resolve::FilterPermission,
    session_variables: &SessionVariables,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    usage_counts: &mut UsagesCounts,
) -> Result<Option<Expression<'s>>, error::Error> {
    match permissions_predicate {
        metadata_resolve::FilterPermission::AllowAll => Ok(None),
        metadata_resolve::FilterPermission::Filter(predicate) => {
            let permission_filter = process_model_predicate(
                model_data_connector_link,
                model_type_mappings,
                predicate,
                session_variables,
                object_types,
                usage_counts,
            )?;
            Ok(Some(permission_filter))
        }
    }
}
