//! model_source.Schema for 'select_many' operation
//!
//! A 'select_many' operation fetches zero or one row from a model

use lang_graphql::ast::common as ast;
use lang_graphql::ast::common::Name;
use lang_graphql::schema as gql_schema;
use std::collections::HashMap;

use crate::metadata::resolved;
use crate::schema::{
    model_arguments,
    model_filter::get_where_expression_input_field,
    model_order_by::get_order_by_expression_input_field,
    permissions,
    types::{self, output_type::get_custom_output_type, Annotation, ModelInputAnnotation},
    GDS,
};

/// Generates the schema for the arguments of a model selection, which includes
/// limit, offset, order_by and where.
pub(crate) fn generate_select_many_arguments(
    builder: &mut gql_schema::Builder<GDS>,
    model: &resolved::model::Model,
) -> Result<
    HashMap<Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>,
    crate::schema::Error,
> {
    let mut arguments = HashMap::new();
    // insert limit argument
    let limit_argument = generate_int_input_argument(
        "limit".to_string(),
        Annotation::Input(types::InputAnnotation::Model(
            ModelInputAnnotation::ModelLimitArgument,
        )),
    )?;
    arguments.insert(
        limit_argument.name.clone(),
        builder.allow_all_namespaced(limit_argument, None),
    );
    // insert offset argument
    let offset_argument = generate_int_input_argument(
        "offset".to_string(),
        Annotation::Input(types::InputAnnotation::Model(
            ModelInputAnnotation::ModelOffsetArgument,
        )),
    )?;
    arguments.insert(
        offset_argument.name.clone(),
        builder.allow_all_namespaced(offset_argument, None),
    );

    // generate and insert order_by argument
    if let Some(order_by_expression_info) = &model.graphql_api.order_by_expression {
        let order_by_argument = {
            get_order_by_expression_input_field(
                builder,
                model.name.clone(),
                order_by_expression_info,
            )
        };

        arguments.insert(
            order_by_argument.name.clone(),
            builder.allow_all_namespaced(order_by_argument, None),
        );
    }

    // generate and insert where argument
    if let Some(filter_expression_info) = &model.graphql_api.filter_expression {
        let where_argument =
            get_where_expression_input_field(builder, model.name.clone(), filter_expression_info);
        arguments.insert(
            where_argument.name.clone(),
            builder.allow_all_namespaced(where_argument, None),
        );
    }

    Ok(arguments)
}

/// Generates schema for a 'select_many' operation
pub(crate) fn select_many_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    model: &resolved::model::Model,
    select_many: &resolved::model::SelectManyGraphQlDefinition,
    parent_type: &ast::TypeName,
) -> Result<
    (
        ast::Name,
        gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>,
    ),
    crate::schema::Error,
> {
    let query_root_field = select_many.query_root_field.clone();
    let mut arguments = generate_select_many_arguments(builder, model)?;

    // Generate the `args` input object and add the model
    // arguments within it.
    if !model.arguments.is_empty() {
        let model_arguments_input =
            model_arguments::get_model_arguments_input_field(builder, model)?;

        let name = model_arguments_input.name.clone();
        if arguments
            .insert(
                name.clone(),
                builder.allow_all_namespaced(model_arguments_input, None),
            )
            .is_some()
        {
            return Err(crate::schema::Error::GraphQlArgumentConflict {
                argument_name: name,
                field_name: query_root_field,
                type_name: parent_type.clone(),
            });
        }
    }

    let field_type = ast::TypeContainer::list_null(ast::TypeContainer::named_non_null(
        get_custom_output_type(gds, builder, &model.data_type)?,
    ));

    let field = builder.conditional_namespaced(
        gql_schema::Field::new(
            query_root_field.clone(),
            None,
            Annotation::Output(types::OutputAnnotation::RootField(
                types::RootFieldAnnotation::Model {
                    data_type: model.data_type.clone(),
                    source: model.source.clone(),
                    kind: types::RootFieldKind::SelectMany,
                    name: model.name.clone(),
                },
            )),
            field_type,
            arguments,
            gql_schema::DeprecationStatus::NotDeprecated,
        ),
        permissions::get_select_permissions_namespace_annotations(model),
    );
    Ok((query_root_field, field))
}

///  Generates the input field for the arguments which are of type int.
pub(crate) fn generate_int_input_argument(
    name: String,
    annotation: Annotation,
) -> Result<gql_schema::InputField<GDS>, crate::schema::Error> {
    let input_field_name = resolved::types::mk_name(&name)?;
    Ok(gql_schema::InputField::new(
        input_field_name,
        None,
        annotation,
        ast::TypeContainer::named_null(gql_schema::RegisteredTypeName::int()),
        None,
        gql_schema::DeprecationStatus::NotDeprecated,
    ))
}
