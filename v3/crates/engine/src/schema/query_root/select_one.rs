//! model_source.Schema for 'select_one' operation
//!
//! A 'select_one' operation fetches zero or one row from a model

use lang_graphql::{ast::common as ast, schema as gql_schema};
use std::collections::BTreeMap;

use crate::metadata::resolved;
use crate::metadata::resolved::types::mk_name;
use crate::schema::types::output_type::get_object_type_representation;
use crate::schema::{mk_deprecation_status, GDS};
use crate::schema::{
    model_arguments, permissions,
    types::{
        self, input_type::get_input_type, output_type::get_custom_output_type, Annotation,
        ModelInputAnnotation,
    },
};

/// Generates schema for a 'select_one' operation
pub(crate) fn select_one_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    model: &resolved::model::Model,
    select_unique: &resolved::model::SelectUniqueGraphQlDefinition,
    parent_type: &ast::TypeName,
) -> Result<
    (
        ast::Name,
        gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>,
    ),
    crate::schema::Error,
> {
    let query_root_field = select_unique.query_root_field.clone();

    let mut arguments = BTreeMap::new();
    for (field_name, field) in select_unique.unique_identifier.iter() {
        let graphql_field_name = mk_name(field_name.0.as_str())?;
        let argument = gql_schema::InputField::new(
            graphql_field_name,
            None,
            Annotation::Input(types::InputAnnotation::Model(
                ModelInputAnnotation::ModelUniqueIdentifierArgument {
                    ndc_column: field.ndc_column.clone(),
                },
            )),
            get_input_type(gds, builder, &field.field_type)?,
            None,
            gql_schema::DeprecationStatus::NotDeprecated,
        );

        arguments.insert(
            argument.name.clone(),
            builder.allow_all_namespaced(argument, None),
        );
    }

    for (argument_field_name, argument_field) in
        model_arguments::build_model_argument_fields(gds, builder, model)?
    {
        if arguments
            .insert(argument_field_name.clone(), argument_field)
            .is_some()
        {
            return Err(crate::schema::Error::GraphQlArgumentConflict {
                argument_name: argument_field_name,
                field_name: query_root_field,
                type_name: parent_type.clone(),
            });
        }
    }

    let object_type_representation = get_object_type_representation(gds, &model.data_type)?;
    let output_typename = get_custom_output_type(gds, builder, &model.data_type)?;

    let field_annotations = permissions::get_select_one_namespace_annotations(
        model,
        object_type_representation,
        select_unique,
        &gds.metadata.object_types,
    )?;

    let field = builder.conditional_namespaced(
        gql_schema::Field::new(
            query_root_field.clone(),
            select_unique.description.clone(),
            Annotation::Output(types::OutputAnnotation::RootField(
                types::RootFieldAnnotation::Model {
                    data_type: model.data_type.clone(),
                    source: model.source.clone(),
                    kind: types::RootFieldKind::SelectOne,
                    name: model.name.clone(),
                },
            )),
            ast::TypeContainer::named_null(output_typename),
            arguments,
            mk_deprecation_status(&select_unique.deprecated),
        ),
        field_annotations,
    );
    Ok((query_root_field, field))
}
