use std::collections::BTreeMap;

use lang_graphql::{ast::common as ast, schema as gql_schema};
use metadata_resolve::Qualified;

use crate::{
    model_filter::get_where_expression_input_field,
    model_order_by::get_order_by_expression_input_field,
    types::{self, TypeId},
    Annotation, Error, ModelInputAnnotation, GDS,
};

pub fn add_filter_input_argument_field(
    fields: &mut BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>,
    field_name: &ast::Name,
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithPermissions,
) -> Result<(), Error> {
    let filter_input_type_name = get_model_filter_input_type(builder, model)?;

    let filter_input_argument = gql_schema::InputField::new(
        field_name.clone(),
        None,
        Annotation::Input(types::InputAnnotation::Model(
            ModelInputAnnotation::ModelFilterInputArgument,
        )),
        ast::TypeContainer::named_null(filter_input_type_name),
        None,
        gql_schema::DeprecationStatus::NotDeprecated,
    );

    fields.insert(
        field_name.clone(),
        builder.allow_all_namespaced(filter_input_argument),
    );

    Ok(())
}

pub fn get_model_filter_input_type(
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithPermissions,
) -> Result<gql_schema::RegisteredTypeName, Error> {
    model
        .graphql_api
        .filter_input_type_name
        .as_ref()
        .ok_or(crate::Error::NoFilterInputTypeNameConfigNameForModel {
            model_name: model.model.name.clone(),
        })
        .map(|filter_input_type_name| {
            builder.register_type(TypeId::ModelFilterInputType {
                model_name: model.model.name.clone(),
                graphql_type_name: filter_input_type_name.clone(),
            })
        })
}

pub fn build_model_filter_input_type(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    model_name: &Qualified<open_dds::models::ModelName>,
    graphql_type_name: &ast::TypeName,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    let model_with_permissions =
        gds.metadata
            .models
            .get(model_name)
            .ok_or_else(|| Error::InternalModelNotFound {
                model_name: model_name.clone(),
            })?;

    let mut filter_input_type_fields = BTreeMap::new();

    add_limit_input_field(
        &mut filter_input_type_fields,
        builder,
        model_with_permissions,
    )?;
    add_offset_input_field(
        &mut filter_input_type_fields,
        builder,
        model_with_permissions,
    )?;
    add_order_by_input_field(
        &mut filter_input_type_fields,
        builder,
        model_with_permissions,
    );
    add_where_input_field(
        &mut filter_input_type_fields,
        builder,
        model_with_permissions,
    );

    Ok(gql_schema::TypeInfo::InputObject(
        gql_schema::InputObject::new(
            graphql_type_name.clone(),
            None,
            filter_input_type_fields,
            vec![], // Directives
        ),
    ))
}

pub fn add_limit_input_field(
    fields: &mut BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>,
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithPermissions,
) -> Result<(), Error> {
    if let Some(limit_field) = &model.graphql_api.limit_field {
        let limit_argument = generate_int_input_argument(
            limit_field.field_name.as_str(),
            Annotation::Input(types::InputAnnotation::Model(
                ModelInputAnnotation::ModelLimitArgument,
            )),
        )?;
        fields.insert(
            limit_argument.name.clone(),
            builder.allow_all_namespaced(limit_argument),
        );
    }

    Ok(())
}

pub fn add_offset_input_field(
    fields: &mut BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>,
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithPermissions,
) -> Result<(), Error> {
    if let Some(offset_field) = &model.graphql_api.offset_field {
        let offset_argument = generate_int_input_argument(
            offset_field.field_name.as_str(),
            Annotation::Input(types::InputAnnotation::Model(
                ModelInputAnnotation::ModelOffsetArgument,
            )),
        )?;

        fields.insert(
            offset_argument.name.clone(),
            builder.allow_all_namespaced(offset_argument),
        );
    }

    Ok(())
}

pub fn add_order_by_input_field(
    fields: &mut BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>,
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithPermissions,
) {
    if let Some(order_by_expression_info) = &model.graphql_api.order_by_expression {
        let order_by_argument =
            { get_order_by_expression_input_field(builder, order_by_expression_info) };

        fields.insert(
            order_by_argument.name.clone(),
            builder.allow_all_namespaced(order_by_argument),
        );
    }
}

pub fn add_where_input_field(
    fields: &mut BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>,
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithPermissions,
) {
    let boolean_expression_filter_type =
        &model
            .filter_expression_type
            .as_ref()
            .and_then(|boolean_expression_object_type| {
                boolean_expression_object_type
                    .graphql
                    .as_ref()
                    .map(|graphql_config| {
                        (
                            boolean_expression_object_type.name.clone(),
                            &graphql_config.type_name,
                            &graphql_config.field_config,
                        )
                    })
            });

    if let Some((
        boolean_expression_type_name,
        boolean_expression_graphql_type_name,
        boolean_expression_graphql_field_config,
    )) = boolean_expression_filter_type
    {
        let where_argument = get_where_expression_input_field(
            builder,
            boolean_expression_type_name.clone(),
            boolean_expression_graphql_field_config,
            boolean_expression_graphql_type_name,
        );

        fields.insert(
            where_argument.name.clone(),
            builder.allow_all_namespaced(where_argument),
        );
    }
}

///  Generates the input field for the arguments which are of type int.
fn generate_int_input_argument(
    name: &str,
    annotation: Annotation,
) -> Result<gql_schema::InputField<GDS>, crate::Error> {
    let input_field_name = metadata_resolve::mk_name(name)
        .map_err(metadata_resolve::Error::from)
        .map_err(metadata_resolve::WithContext::from)?;

    Ok(gql_schema::InputField::new(
        input_field_name,
        None,
        annotation,
        ast::TypeContainer::named_null(gql_schema::RegisteredTypeName::int()),
        None,
        gql_schema::DeprecationStatus::NotDeprecated,
    ))
}
