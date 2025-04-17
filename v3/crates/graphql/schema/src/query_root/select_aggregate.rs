//! model_source.Schema for 'select_aggregate' operation
//!
//! A 'select_aggregate' operation fetches aggregations over the model's data
//!
use std::collections::BTreeMap;

use lang_graphql::{ast::common as ast, schema as gql_schema};

use crate::aggregates::get_aggregate_select_output_type;
use crate::types::{self, Annotation};
use crate::{
    Error, GDS, RootFieldAnnotation, RootFieldKind, mk_deprecation_status, model_arguments,
    model_filter_input, permissions,
};
use metadata_resolve;

pub(crate) fn select_aggregate_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithPermissions,
    select_aggregate: &metadata_resolve::SelectAggregateGraphQlDefinition,
    parent_type: &ast::TypeName,
) -> Result<
    (
        ast::Name,
        gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>,
    ),
    Error,
> {
    let aggregate_expression = gds
        .metadata
        .aggregate_expressions
        .get(&select_aggregate.aggregate_expression_name)
        .ok_or_else(|| Error::InternalAggregateExpressionNotFound {
            aggregate_expression: select_aggregate.aggregate_expression_name.clone(),
        })?;

    let root_field_name = select_aggregate.query_root_field.clone();

    let arguments = generate_select_aggregate_arguments(
        builder,
        model,
        &select_aggregate.filter_input_field_name,
        &select_aggregate.query_root_field,
        parent_type,
    )?;

    let field_permissions = permissions::get_select_permissions_namespace_annotations(model);

    let output_typename = get_aggregate_select_output_type(builder, aggregate_expression)?;

    let field = builder.conditional_namespaced(
        gql_schema::Field::new(
            root_field_name.clone(),
            select_aggregate.description.clone(),
            Annotation::Output(types::OutputAnnotation::RootField(
                RootFieldAnnotation::Model {
                    kind: RootFieldKind::SelectAggregate,
                    name: model.model.name.clone(),
                },
            )),
            ast::TypeContainer::named_null(output_typename),
            arguments,
            mk_deprecation_status(select_aggregate.deprecated.as_ref()),
        ),
        field_permissions,
    );
    Ok((root_field_name, field))
}

pub fn generate_select_aggregate_arguments(
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithPermissions,
    filter_input_field_name: &ast::Name,
    parent_field_name: &ast::Name,
    parent_type: &ast::TypeName,
) -> Result<BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>, Error> {
    let mut arguments =
        BTreeMap::<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>::new();

    model_arguments::add_model_arguments_field(
        &mut arguments,
        builder,
        model,
        parent_field_name,
        parent_type,
    )?;
    model_filter_input::add_filter_input_argument_field(
        &mut arguments,
        filter_input_field_name,
        builder,
        model,
    )?;

    Ok(arguments)
}
