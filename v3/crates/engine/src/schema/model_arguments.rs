use lang_graphql::ast::common as ast;

use crate::metadata::resolved;
use crate::schema::GDS;
use lang_graphql::schema as gql_schema;
use open_dds::models::ModelName;
use std::collections::{BTreeMap, HashMap};

use super::types::input_type::get_input_type;
use super::types::{Annotation, InputAnnotation, ModelInputAnnotation, TypeId};
use crate::metadata::resolved::subgraph::Qualified;

/// Creates the `args` input object within which the model
/// arguments fields will live.
pub fn get_model_arguments_input_field(
    builder: &mut gql_schema::Builder<GDS>,
    model: &resolved::model::Model,
) -> Result<gql_schema::InputField<GDS>, crate::schema::Error> {
    model
        .graphql_api
        .arguments_input_config
        .as_ref()
        .ok_or(crate::schema::Error::NoArgumentsInputConfigForSelectMany {
            model_name: model.name.clone(),
        })
        .map(|arguments_input_config| {
            // This function call adds the model arguments to the
            // `args` input object
            builder.register_type(TypeId::ModelArgumentsInput {
                model_name: model.name.clone(),
                type_name: arguments_input_config.type_name.clone(),
            });

            gql_schema::InputField {
                name: arguments_input_config.field_name.clone(),
                description: None,
                info: Annotation::Input(InputAnnotation::Model(
                    ModelInputAnnotation::ModelArgumentsExpression,
                )),
                field_type: ast::TypeContainer::named_non_null(
                    arguments_input_config.type_name.clone(),
                ),
                default_value: None,
                deprecation_status: gql_schema::DeprecationStatus::NotDeprecated,
            }
        })
}

pub fn build_model_argument_fields(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    model: &resolved::model::Model,
) -> Result<
    BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>,
    crate::schema::Error,
> {
    model
        .arguments
        .iter()
        .map(|(argument_name, argument_type)| {
            let field_name = ast::Name::new(argument_name.0.as_str())?;
            let input_type = get_input_type(gds, builder, &argument_type.argument_type)?;

            let input_field = gql_schema::InputField::new(
                field_name.clone(),
                argument_type.description.clone(),
                Annotation::Input(InputAnnotation::Model(
                    ModelInputAnnotation::ModelArgument {
                        argument_type: argument_type.argument_type.clone(),
                        ndc_table_argument: model
                            .source
                            .as_ref()
                            .and_then(|model_source| {
                                model_source.argument_mappings.get(argument_name)
                            })
                            .cloned(),
                    },
                )),
                input_type,
                None,
                gql_schema::DeprecationStatus::NotDeprecated,
            );

            // if we have select_permissions, then work out which arguments to include in the schema
            match &model.select_permissions {
                // if no permissions apply, allow the argument through
                None => Ok((field_name, builder.allow_all_namespaced(input_field, None))),
                Some(permissions_by_namespace) => {
                    let mut namespaced_annotations = HashMap::new();

                    for (namespace, permission) in permissions_by_namespace {
                        // if there is a preset for this argument, remove it from the schema
                        // so the user cannot provide one
                        if permission.argument_presets.get(argument_name).is_none() {
                            namespaced_annotations.insert(namespace.clone(), None);
                        }
                    }

                    Ok((
                        field_name,
                        builder.conditional_namespaced(input_field, namespaced_annotations),
                    ))
                }
            }
        })
        .collect()
}

pub fn build_model_arguments_input_schema(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    type_name: &ast::TypeName,
    model_name: &Qualified<ModelName>,
) -> Result<gql_schema::TypeInfo<GDS>, crate::schema::Error> {
    let model = gds.metadata.models.get(model_name).ok_or_else(|| {
        crate::schema::Error::InternalModelNotFound {
            model_name: model_name.clone(),
        }
    })?;

    Ok(gql_schema::TypeInfo::InputObject(
        gql_schema::InputObject::new(
            type_name.clone(),
            None,
            build_model_argument_fields(gds, builder, model)?,
            Vec::new(),
        ),
    ))
}
