//! model_source.Schema for 'select_one' operation
//!
//! A 'select_one' operation fetches zero or one row from a model

use indexmap::IndexMap;
use lang_graphql::{ast::common as ast, schema as gql_schema};
use open_dds::types::FieldName;
use std::collections::BTreeMap;

use crate::types::output_type::get_object_type_representation;
use crate::{mk_deprecation_status, GDS};
use crate::{
    model_arguments, permissions,
    types::{
        self, input_type::get_input_type, output_type::get_custom_output_type, Annotation,
        ModelInputAnnotation,
    },
};
use metadata_resolve;
use metadata_resolve::mk_name;

/// Generates schema for a 'select_one' operation
pub(crate) fn select_one_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithPermissions,
    select_unique: &metadata_resolve::SelectUniqueGraphQlDefinition,
    parent_type: &ast::TypeName,
) -> Result<
    (
        ast::Name,
        gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>,
    ),
    crate::Error,
> {
    let query_root_field = select_unique.query_root_field.clone();

    let arguments = generate_select_one_arguments(
        gds,
        builder,
        model,
        query_root_field.clone(),
        &select_unique.unique_identifier,
        parent_type,
    )?;

    let object_type_representation = get_object_type_representation(gds, &model.model.data_type)?;
    let output_typename = get_custom_output_type(gds, builder, &model.model.data_type)?;

    let field_annotations = permissions::get_select_one_namespace_annotations(
        model,
        object_type_representation,
        &select_unique.unique_identifier,
    );

    let field = builder.conditional_namespaced(
        gql_schema::Field::new(
            query_root_field.clone(),
            select_unique.description.clone(),
            Annotation::Output(types::OutputAnnotation::RootField(
                types::RootFieldAnnotation::Model {
                    data_type: model.model.data_type.clone(),
                    kind: types::RootFieldKind::SelectOne,
                    name: model.model.name.clone(),
                },
            )),
            ast::TypeContainer::named_null(output_typename),
            arguments,
            mk_deprecation_status(select_unique.deprecated.as_ref()),
        ),
        field_annotations,
    );
    Ok((query_root_field, field))
}

pub(crate) fn generate_select_one_arguments(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithPermissions,
    root_field: ast::Name,
    unique_identifier: &IndexMap<FieldName, metadata_resolve::UniqueIdentifierField>,
    parent_type: &ast::TypeName,
) -> Result<
    BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>,
    crate::Error,
> {
    let mut arguments = BTreeMap::new();
    for (field_name, field) in unique_identifier {
        let graphql_field_name = mk_name(field_name.as_str())
            .map_err(metadata_resolve::Error::from)
            .map_err(metadata_resolve::WithContext::from)?;

        let argument = gql_schema::InputField::new(
            graphql_field_name,
            None,
            Annotation::Input(types::InputAnnotation::Model(
                ModelInputAnnotation::ModelUniqueIdentifierArgument {
                    field_name: field_name.clone(),
                    ndc_column: field.ndc_column.clone(),
                },
            )),
            get_input_type(gds, builder, &field.field_type)?,
            None,
            gql_schema::DeprecationStatus::NotDeprecated,
        );

        arguments.insert(
            argument.name.clone(),
            builder.allow_all_namespaced(argument),
        );
    }

    for (argument_field_name, argument_field) in
        model_arguments::build_model_argument_fields(gds, builder, model)?
    {
        if arguments
            .insert(argument_field_name.clone(), argument_field)
            .is_some()
        {
            return Err(crate::Error::GraphQlArgumentConflict {
                argument_name: argument_field_name,
                field_name: root_field,
                type_name: parent_type.clone(),
            });
        }
    }
    Ok(arguments)
}
