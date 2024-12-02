//! Schema of the subscription root type

use hasura_authn_core::Role;
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::ast::common::TypeName;
use lang_graphql::schema as gql_schema;
use open_dds::aggregates::AggregateExpressionName;
use open_dds::types::FieldName;
use std::collections::BTreeMap;
use std::collections::HashMap;

use crate::aggregates::get_aggregate_select_output_type;
use crate::mk_deprecation_status;
use crate::model_arguments;
use crate::types;
use crate::types::output_type::get_custom_output_type;
use crate::types::output_type::get_object_type_representation;
use crate::Annotation;
use crate::GDS;

use super::query_root::{select_aggregate, select_many, select_one};

pub fn subscription_root_schema(
    builder: &mut gql_schema::Builder<GDS>,
    gds: &GDS,
    subscription_root_type_name: &TypeName,
) -> Result<gql_schema::Object<GDS>, crate::Error> {
    let mut fields = BTreeMap::new();
    for model in gds.metadata.models.values() {
        // Add select_one fields to the subscription root
        for select_unique in &model.graphql_api.select_uniques {
            if let Some(subscription) = &select_unique.subscription {
                let (field_name, field) = select_one_field(
                    gds,
                    builder,
                    model,
                    &select_unique.unique_identifier,
                    subscription,
                    subscription_root_type_name,
                )?;
                fields.insert(field_name, field);
            }
        }

        // Add select_many fields to the subscription root
        if let Some(select_many) = &model.graphql_api.select_many {
            if let Some(subscription) = &select_many.subscription {
                let (field_name, field) = select_many_field(
                    gds,
                    builder,
                    model,
                    subscription,
                    subscription_root_type_name,
                )?;
                fields.insert(field_name, field);
            }
        }

        // Add select_aggregate fields to the subscription root
        if let Some(select_aggregate) = &model.graphql_api.select_aggregate {
            if let Some(subscription) = &select_aggregate.subscription {
                let (field_name, field) = select_aggregate_field(
                    gds,
                    builder,
                    model,
                    &select_aggregate.aggregate_expression_name,
                    &select_aggregate.filter_input_field_name,
                    subscription,
                    subscription_root_type_name,
                )?;
                fields.insert(field_name, field);
            }
        }
    }
    Ok(gql_schema::Object::new(
        builder,
        subscription_root_type_name.clone(),
        None,
        fields,
        BTreeMap::new(),
        Vec::new(),
    ))
}

/// Generates schema for a 'select_one' operation
fn select_one_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithArgumentPresets,
    unique_identifier: &IndexMap<FieldName, metadata_resolve::UniqueIdentifierField>,
    subscription: &metadata_resolve::SubscriptionGraphQlDefinition,
    parent_type: &ast::TypeName,
) -> Result<
    (
        ast::Name,
        gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>,
    ),
    crate::Error,
> {
    let subscription_root_field = subscription.root_field.clone();
    let arguments = select_one::generate_select_one_arguments(
        gds,
        builder,
        model,
        subscription_root_field.clone(),
        unique_identifier,
        parent_type,
    )?;
    let object_type_representation = get_object_type_representation(gds, &model.model.data_type)?;

    let output_typename = get_custom_output_type(gds, builder, &model.model.data_type)?;
    let field_annotations =
        get_select_one_namespace_annotations(model, object_type_representation, unique_identifier)?;
    let field = builder.conditional_namespaced(
        gql_schema::Field::new(
            subscription_root_field.clone(),
            subscription.description.clone(),
            Annotation::Output(types::OutputAnnotation::RootField(
                types::RootFieldAnnotation::ModelSubscription {
                    data_type: model.model.data_type.clone(),
                    source: model.model.source.clone(),
                    kind: types::RootFieldKind::SelectOne,
                    name: model.model.name.clone(),
                    polling_interval_ms: subscription.polling_interval_ms,
                },
            )),
            ast::TypeContainer::named_null(output_typename),
            arguments,
            mk_deprecation_status(&subscription.deprecated),
        ),
        field_annotations,
    );
    Ok((subscription_root_field, field))
}

/// Generates schema for a 'select_many' operation
fn select_many_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithArgumentPresets,
    subscription: &metadata_resolve::SubscriptionGraphQlDefinition,
    parent_type: &ast::TypeName,
) -> Result<
    (
        ast::Name,
        gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>,
    ),
    crate::Error,
> {
    let subscription_root_field = subscription.root_field.clone();
    let mut arguments = select_many::generate_select_many_arguments(builder, model)?;

    model_arguments::add_model_arguments_field(
        &mut arguments,
        builder,
        model,
        &subscription.root_field,
        parent_type,
    )?;

    let field_type = ast::TypeContainer::list_null(ast::TypeContainer::named_non_null(
        get_custom_output_type(gds, builder, &model.model.data_type)?,
    ));

    let field = builder.conditional_namespaced(
        gql_schema::Field::new(
            subscription_root_field.clone(),
            subscription.description.clone(),
            Annotation::Output(types::OutputAnnotation::RootField(
                types::RootFieldAnnotation::ModelSubscription {
                    data_type: model.model.data_type.clone(),
                    source: model.model.source.clone(),
                    kind: types::RootFieldKind::SelectMany,
                    name: model.model.name.clone(),
                    polling_interval_ms: subscription.polling_interval_ms,
                },
            )),
            field_type,
            arguments,
            mk_deprecation_status(&subscription.deprecated),
        ),
        get_select_permissions_namespace_annotations(model)?,
    );
    Ok((subscription_root_field, field))
}

/// Generates schema for a 'aggregate' operation
fn select_aggregate_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithArgumentPresets,
    aggregate_expression_name: &metadata_resolve::Qualified<AggregateExpressionName>,
    filter_input_field_name: &ast::Name,
    subscription: &metadata_resolve::SubscriptionGraphQlDefinition,
    parent_type: &ast::TypeName,
) -> Result<
    (
        ast::Name,
        gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>,
    ),
    crate::Error,
> {
    let aggregate_expression = gds
        .metadata
        .aggregate_expressions
        .get(aggregate_expression_name)
        .ok_or_else(|| crate::Error::InternalAggregateExpressionNotFound {
            aggregate_expression: aggregate_expression_name.clone(),
        })?;

    let subscription_field_name = subscription.root_field.clone();

    let arguments = select_aggregate::generate_select_aggregate_arguments(
        builder,
        model,
        filter_input_field_name,
        &subscription.root_field,
        parent_type,
    )?;

    let field_permissions = get_select_permissions_namespace_annotations(model)?;

    let output_typename = get_aggregate_select_output_type(builder, aggregate_expression)?;

    let field = builder.conditional_namespaced(
        gql_schema::Field::new(
            subscription_field_name.clone(),
            subscription.description.clone(),
            Annotation::Output(types::OutputAnnotation::RootField(
                types::RootFieldAnnotation::ModelSubscription {
                    data_type: model.model.data_type.clone(),
                    source: model.model.source.clone(),
                    kind: types::RootFieldKind::SelectAggregate,
                    name: model.model.name.clone(),
                    polling_interval_ms: subscription.polling_interval_ms,
                },
            )),
            ast::TypeContainer::named_null(output_typename),
            arguments,
            mk_deprecation_status(&subscription.deprecated),
        ),
        field_permissions,
    );
    Ok((subscription_field_name, field))
}

/// Build namespace annotations for subscription select one root field.
///
/// This wrapper function combines the process of generating annotations and
/// applying subscription permission for a single root field.
fn get_select_one_namespace_annotations(
    model: &metadata_resolve::ModelWithArgumentPresets,
    object_type_representation: &metadata_resolve::ObjectTypeWithRelationships,
    unique_identifier: &IndexMap<FieldName, metadata_resolve::UniqueIdentifierField>,
) -> Result<HashMap<Role, Option<types::NamespaceAnnotation>>, crate::Error> {
    let annotations = super::permissions::get_select_one_namespace_annotations(
        model,
        object_type_representation,
        unique_identifier,
    )?;
    Ok(apply_subscription_permissions_model(annotations))
}

/// Build namespace annotations for subscription select root fields.
///
/// This wrapper function combines the process of generating annotations and
/// applying subscription permission for select root fields.
fn get_select_permissions_namespace_annotations(
    model: &metadata_resolve::ModelWithArgumentPresets,
) -> Result<HashMap<Role, Option<types::NamespaceAnnotation>>, crate::Error> {
    let annotations = super::permissions::get_select_permissions_namespace_annotations(model)?;
    Ok(apply_subscription_permissions_model(annotations))
}

/// Filters a HashMap of role-to-annotation mappings, retaining only those
/// where subscriptions are explicitly allowed.
fn apply_subscription_permissions_model(
    annotations: HashMap<Role, Option<types::NamespaceAnnotation>>,
) -> HashMap<Role, Option<types::NamespaceAnnotation>> {
    annotations
        .into_iter()
        .filter_map(|(role, annotation)| match annotation {
            Some(types::NamespaceAnnotation::Model {
                allow_subscriptions,
                ..
            }) if allow_subscriptions => Some((role, annotation)),
            _ => None,
        })
        .collect()
}
