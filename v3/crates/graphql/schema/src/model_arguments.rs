use lang_graphql::ast::common as ast;

use crate::{
    permissions,
    types::{
        input_type::get_input_type, Annotation, InputAnnotation, ModelInputAnnotation, TypeId,
    },
    GDS,
};
use lang_graphql::schema as gql_schema;
use metadata_resolve::Qualified;
use open_dds::models::ModelName;
use std::collections::{BTreeMap, HashMap};

/// Creates the `args` input object within which the model
/// arguments fields will live.
pub fn get_model_arguments_input_field(
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithPermissions,
    include_empty_default: bool,
) -> Result<gql_schema::InputField<GDS>, crate::Error> {
    model
        .graphql_api
        .arguments_input_config
        .as_ref()
        .ok_or(crate::Error::NoArgumentsInputConfigForSelectMany {
            model_name: model.model.name.clone(),
        })
        .map(|arguments_input_config| {
            // This function call adds the model arguments to the
            // `args` input object
            builder.register_type(TypeId::ModelArgumentsInput {
                model_name: model.model.name.clone(),
                type_name: arguments_input_config.type_name.clone(),
            });

            // if there are no possible arguments, provide a default of `{}`
            // so that `args` can be omitted if the user chooses
            let default_value = if include_empty_default {
                Some(lang_graphql::ast::value::ConstValue::Object(vec![]))
            } else {
                None
            };

            gql_schema::InputField {
                name: arguments_input_config.field_name.clone(),
                description: None,
                info: Annotation::Input(InputAnnotation::Model(
                    ModelInputAnnotation::ModelArgumentsExpression,
                )),
                field_type: ast::TypeContainer::named_non_null(
                    arguments_input_config.type_name.clone(),
                ),
                default_value,
                deprecation_status: gql_schema::DeprecationStatus::NotDeprecated,
            }
        })
}

pub fn build_model_argument_fields(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithPermissions,
) -> Result<
    BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>,
    crate::Error,
> {
    model
        .model
        .arguments
        .iter()
        .map(|(argument_name, argument_type)| {
            let field_name = ast::Name::new(argument_name.as_str())?;
            let input_type = get_input_type(gds, builder, &argument_type.argument_type)?;

            let input_field = gql_schema::InputField::new(
                field_name.clone(),
                argument_type.description.clone(),
                Annotation::Input(InputAnnotation::Model(
                    ModelInputAnnotation::ModelArgument {
                        argument_name: argument_name.clone(),
                        argument_type: argument_type.argument_type.clone(),
                        argument_kind: argument_type.argument_kind.clone(),
                        ndc_table_argument: model
                            .model
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

            let mut namespaced_annotations = HashMap::new();

            for (namespace, permission) in &model.select_permissions {
                // if there is a preset for this argument, remove it from the schema
                // so the user cannot provide one
                if !permission.argument_presets.contains_key(argument_name) {
                    namespaced_annotations.insert(namespace.clone(), None);
                }
            }

            Ok((
                field_name,
                builder.conditional_namespaced(input_field, namespaced_annotations),
            ))
        })
        .collect()
}

pub fn build_model_arguments_input_schema(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    type_name: &ast::TypeName,
    model_name: &Qualified<ModelName>,
) -> Result<gql_schema::TypeInfo<GDS>, crate::Error> {
    let model =
        gds.metadata
            .models
            .get(model_name)
            .ok_or_else(|| crate::Error::InternalModelNotFound {
                model_name: model_name.clone(),
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

/// Generate the `args` input object and add the model arguments within it.
pub fn add_model_arguments_field(
    arguments: &mut BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>,
    builder: &mut gql_schema::Builder<GDS>,
    model: &metadata_resolve::ModelWithPermissions,
    parent_field_name: &ast::Name,
    parent_type: &ast::TypeName,
) -> Result<(), crate::Error> {
    // which arguments are actually available for the user to provide?
    let user_arguments: Vec<_> = model
        .model
        .arguments
        .keys()
        .filter(|argument_name| {
            for permission in model.select_permissions.values() {
                // if there is a preset for this argument, it will not be included in the schema
                if permission.argument_presets.contains_key(*argument_name) {
                    return false;
                }
            }
            // is the argument nullable? if so we don't _need_ it to be provided
            model
                .model
                .arguments
                .get(*argument_name)
                .is_some_and(|argument| !argument.argument_type.nullable)
        })
        .collect();

    if !model.model.arguments.is_empty() {
        let include_empty_default = user_arguments.is_empty();
        let model_arguments_input =
            get_model_arguments_input_field(builder, model, include_empty_default)?;

        let name = model_arguments_input.name.clone();

        let model_arguments = builder.conditional_namespaced(
            model_arguments_input,
            permissions::get_select_permissions_namespace_annotations(model),
        );

        if arguments.insert(name.clone(), model_arguments).is_some() {
            return Err(crate::Error::GraphQlArgumentConflict {
                argument_name: name,
                field_name: parent_field_name.clone(),
                type_name: parent_type.clone(),
            });
        }
    }

    Ok(())
}
