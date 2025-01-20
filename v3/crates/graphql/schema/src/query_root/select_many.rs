//! model_source.Schema for 'select_many' operation
//!
//! A 'select_many' operation fetches zero or one row from a model

use lang_graphql::ast::common as ast;
use lang_graphql::ast::common::Name;
use lang_graphql::schema as gql_schema;
use std::collections::BTreeMap;

use crate::mk_deprecation_status;
use crate::model_arguments::add_model_arguments_field;
use crate::model_filter_input::{
    add_limit_input_field, add_offset_input_field, add_order_by_input_field, add_where_input_field,
};
use crate::{
    permissions,
    types::{self, output_type::get_custom_output_type, Annotation},
    GDS,
};
use metadata_resolve;

/// Generates the schema for the arguments of a model selection, which includes
/// limit, offset, order_by and where.
pub(crate) fn generate_select_many_arguments(
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithPermissions,
) -> Result<BTreeMap<Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>, crate::Error>
{
    let mut arguments = BTreeMap::new();

    add_limit_input_field(&mut arguments, builder, model)?;
    add_offset_input_field(&mut arguments, builder, model)?;
    add_order_by_input_field(&mut arguments, builder, model);
    add_where_input_field(&mut arguments, builder, model);

    Ok(arguments)
}

/// Generates schema for a 'select_many' operation
pub(crate) fn select_many_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithPermissions,
    select_many: &metadata_resolve::SelectManyGraphQlDefinition,
    parent_type: &ast::TypeName,
) -> Result<
    (
        ast::Name,
        gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>,
    ),
    crate::Error,
> {
    let query_root_field = select_many.query_root_field.clone();
    let mut arguments = generate_select_many_arguments(builder, model)?;

    add_model_arguments_field(
        &mut arguments,
        builder,
        model,
        &select_many.query_root_field,
        parent_type,
    )?;

    let field_type = ast::TypeContainer::list_null(ast::TypeContainer::named_non_null(
        get_custom_output_type(gds, builder, &model.model.data_type)?,
    ));

    let field = builder.conditional_namespaced(
        gql_schema::Field::new(
            query_root_field.clone(),
            select_many.description.clone(),
            Annotation::Output(types::OutputAnnotation::RootField(
                types::RootFieldAnnotation::Model {
                    data_type: model.model.data_type.clone(),
                    kind: types::RootFieldKind::SelectMany,
                    name: model.model.name.clone(),
                },
            )),
            field_type,
            arguments,
            mk_deprecation_status(select_many.deprecated.as_ref()),
        ),
        permissions::get_select_permissions_namespace_annotations(model),
    );
    Ok((query_root_field, field))
}
