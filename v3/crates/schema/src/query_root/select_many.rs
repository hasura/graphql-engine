//! model_source.Schema for 'select_many' operation
//!
//! A 'select_many' operation fetches zero or one row from a model

use lang_graphql::ast::common as ast;
use lang_graphql::ast::common::Name;
use lang_graphql::schema as gql_schema;
use std::collections::BTreeMap;

use crate::mk_deprecation_status;
use crate::{
    model_arguments,
    model_filter::get_where_expression_input_field,
    model_order_by::get_order_by_expression_input_field,
    permissions,
    types::{self, output_type::get_custom_output_type, Annotation, ModelInputAnnotation},
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

    // insert limit argument
    if let Some(limit_field) = &model.model.graphql_api.limit_field {
        let limit_argument = generate_int_input_argument(
            limit_field.field_name.as_str(),
            Annotation::Input(types::InputAnnotation::Model(
                ModelInputAnnotation::ModelLimitArgument,
            )),
        )?;
        arguments.insert(
            limit_argument.name.clone(),
            builder.allow_all_namespaced(limit_argument, None),
        );
    }

    // insert offset argument
    if let Some(offset_field) = &model.model.graphql_api.offset_field {
        let offset_argument = generate_int_input_argument(
            offset_field.field_name.as_str(),
            Annotation::Input(types::InputAnnotation::Model(
                ModelInputAnnotation::ModelOffsetArgument,
            )),
        )?;

        arguments.insert(
            offset_argument.name.clone(),
            builder.allow_all_namespaced(offset_argument, None),
        );
    }

    // generate and insert order_by argument
    if let Some(order_by_expression_info) = &model.model.graphql_api.order_by_expression {
        let order_by_argument = {
            get_order_by_expression_input_field(
                builder,
                model.model.name.clone(),
                order_by_expression_info,
            )
        };

        arguments.insert(
            order_by_argument.name.clone(),
            builder.allow_all_namespaced(order_by_argument, None),
        );
    }

    // generate and insert where argument
    if let Some(object_boolean_expression_type) = &model.model.filter_expression_type {
        if let Some(boolean_expression) = &object_boolean_expression_type.graphql {
            let where_argument = get_where_expression_input_field(
                builder,
                object_boolean_expression_type.name.clone(),
                boolean_expression,
            );

            arguments.insert(
                where_argument.name.clone(),
                builder.allow_all_namespaced(where_argument, None),
            );
        }
    }

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

    // Generate the `args` input object and add the model
    // arguments within it.
    if !model.model.arguments.is_empty() {
        let model_arguments_input =
            model_arguments::get_model_arguments_input_field(builder, model)?;

        let name = model_arguments_input.name.clone();

        let model_arguments = builder.conditional_namespaced(
            model_arguments_input,
            permissions::get_select_permissions_namespace_annotations(
                model,
                &gds.metadata.object_types,
            )?,
        );

        if arguments.insert(name.clone(), model_arguments).is_some() {
            return Err(crate::Error::GraphQlArgumentConflict {
                argument_name: name,
                field_name: query_root_field,
                type_name: parent_type.clone(),
            });
        }
    }

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
                    source: model.model.source.clone(),
                    kind: types::RootFieldKind::SelectMany,
                    name: model.model.name.clone(),
                },
            )),
            field_type,
            arguments,
            mk_deprecation_status(&select_many.deprecated),
        ),
        permissions::get_select_permissions_namespace_annotations(
            model,
            &gds.metadata.object_types,
        )?,
    );
    Ok((query_root_field, field))
}

///  Generates the input field for the arguments which are of type int.
pub(crate) fn generate_int_input_argument(
    name: &str,
    annotation: Annotation,
) -> Result<gql_schema::InputField<GDS>, crate::Error> {
    let input_field_name = metadata_resolve::mk_name(name)?;
    Ok(gql_schema::InputField::new(
        input_field_name,
        None,
        annotation,
        ast::TypeContainer::named_null(gql_schema::RegisteredTypeName::int()),
        None,
        gql_schema::DeprecationStatus::NotDeprecated,
    ))
}
