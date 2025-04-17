use indexmap::IndexMap;
use nonempty::NonEmpty;
use std::collections::BTreeMap;
use std::collections::HashMap;

use super::collect;
use super::error::*;
use super::input;
use super::variables;
use crate::ast::common as ast;
use crate::ast::executable;
use crate::ast::spanning;
use crate::ast::spanning::Spanning;
use crate::normalized_ast as normalized;
use crate::schema;

pub fn normalize_selection_set<
    'q,
    's,
    S: schema::SchemaContext,
    NSGet: schema::NamespacedGetter<S>,
>(
    namespaced_getter: &NSGet,
    schema: &'s schema::Schema<S>,
    fragments: &HashMap<&'q ast::Name, &'q executable::FragmentDefinition>,
    variables: &variables::Variables<'q, 's, S>,

    selection_type: &collect::SelectableType<'s, S>,
    selection_set: &'q executable::SelectionSet,
) -> Result<normalized::SelectionSet<'s, S>>
where
    's: 'q,
{
    let reachability = Vec::new();
    normalize_selection_sets(
        namespaced_getter,
        schema,
        fragments,
        variables,
        selection_type,
        Vec::from([(&reachability, Vec::from([&selection_set.items]))]),
    )
}

#[allow(clippy::type_complexity)]
fn normalize_selection_sets<'q, 's, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    namespaced_getter: &NSGet,
    schema: &'s schema::Schema<S>,
    fragments: &HashMap<&'q ast::Name, &'q executable::FragmentDefinition>,
    variables: &variables::Variables<'q, 's, S>,

    selection_type: &collect::SelectableType<'s, S>,
    // TODO, use a struct for this but with lifetime elision, is the current form more readable?
    selection_set_groups: Vec<(
        // field path
        &Vec<&'s ast::TypeName>,
        Vec<&'q Vec<spanning::Spanning<executable::Selection>>>,
    )>,
) -> Result<normalized::SelectionSet<'s, S>>
where
    's: 'q,
{
    let mut fields = Vec::new();
    for (path, selection_sets) in selection_set_groups {
        for selection_set in selection_sets {
            collect::collect_fields(
                namespaced_getter,
                schema,
                fragments,
                path,
                selection_type,
                selection_set,
                &mut fields,
            )?;
        }
    }
    let field_map = fields.iter().fold(IndexMap::new(), |mut acc, field| {
        let alias = field.alias;
        acc.entry(alias)
            .and_modify(
                |m: &mut (
                    &ast::Type,
                    HashMap<&Vec<&ast::TypeName>, NonEmpty<&collect::CollectedField<S>>>,
                )| {
                    m.1.entry(&field.field_path)
                        .and_modify(|v| v.push(field))
                        .or_insert_with(|| NonEmpty::new(field));
                },
            )
            // This additional type tracking can be avodided with a non-empty map
            .or_insert_with(|| {
                (
                    &field.info.generic.field_type,
                    HashMap::from([(&field.field_path, NonEmpty::new(field))]),
                )
            });
        acc
    });
    let mut normalized_fields = IndexMap::new();
    for (alias, (alias_type, typed_fields)) in field_map {
        let alias = ast::Alias(alias.clone());
        let (field_calls, selection_set) = merge_fields(
            namespaced_getter,
            schema,
            fragments,
            variables,
            selection_type.type_name,
            &alias,
            alias_type,
            typed_fields,
        )?;
        // if let normalized::FieldCalls::Conditional(conditional) = &field_calls {
        if !field_calls.is_empty() {
            let normalized_field = normalized::Field {
                alias: alias.clone(),
                field_calls,
                selection_set,
                type_container: alias_type.clone(),
            };
            normalized_fields.insert(alias, normalized_field);
        }
        // }
    }
    // TODO: this is a round about way of check if it is an object type
    let type_name_response = if selection_type.possible_types.len() == 1
        && selection_type
            .possible_types
            .contains(selection_type.type_name)
    {
        Some(selection_type.type_name)
    } else {
        None
    };

    if normalized_fields.is_empty() {
        Err(Error::FieldSelectionSetIsEmpty)?;
    }

    Ok(normalized::SelectionSet {
        fields: normalized_fields,
        type_name: type_name_response.cloned(),
    })
}

#[allow(clippy::too_many_arguments)]
fn merge_fields<'q, 's, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    namespaced_getter: &NSGet,
    schema: &'s schema::Schema<S>,
    fragments: &HashMap<&'q ast::Name, &'q executable::FragmentDefinition>,
    variables: &variables::Variables<'q, 's, S>,
    type_name: &ast::TypeName,
    alias: &ast::Alias,
    alias_type: &ast::Type,
    typed_fields: HashMap<&Vec<&'s ast::TypeName>, NonEmpty<&collect::CollectedField<'q, 's, S>>>,
) -> Result<(
    normalized::FieldCalls<'s, S>,
    normalized::SelectionSet<'s, S>,
)>
where
    's: 'q,
{
    let alias_type_info = schema
        .get_type(alias_type.underlying_type())
        .ok_or_else(|| Error::InternalTypeNotFound {
            type_name: alias_type.underlying_type().clone(),
        })?;
    let alias_selectable_type = alias_type_info.to_selectable_type();

    let mut alias_selection_sets = Vec::new();
    let mut field_calls = HashMap::new();
    for (reachability, fields) in typed_fields {
        let canonical_field = fields.head;

        let arguments = normalize_arguments(
            namespaced_getter,
            schema,
            variables,
            type_name,
            &canonical_field.info.generic.name,
            &canonical_field.info.generic.arguments,
            canonical_field.field.arguments.as_ref(),
        )?;
        let canonical_field_type = &canonical_field.info.generic.field_type;
        if canonical_field_type != alias_type {
            return Err(Error::FieldsConflictDifferingTypes {
                alias: alias.clone(),
                type1: alias_type.clone(),
                type2: canonical_field_type.clone(),
            });
        }
        let mut selection_sets = Vec::with_capacity(fields.len());
        if let Some(selection_set) = &canonical_field.field.selection_set {
            selection_sets.push(&selection_set.item.items);
        }
        for field in fields.tail() {
            if field.field.name.item != canonical_field.field.name.item {
                return Err(Error::FieldsConflictDifferentFields {
                    alias: alias.clone(),
                    field1: canonical_field.field.name.item.clone(),
                    field2: field.field.name.item.clone(),
                });
            }
            // TODO deal with directives
            // let this_directives = normalize_directives( schema, &field.field.directives)?;
            let this_arguments = normalize_arguments(
                namespaced_getter,
                schema,
                variables,
                type_name,
                &field.info.generic.name,
                &field.info.generic.arguments,
                field.field.arguments.as_ref(),
            )?;
            if arguments != this_arguments {
                return Err(Error::FieldsConflictDifferingArguments {
                    alias: alias.clone(),
                    location1: canonical_field.field.arguments.as_ref().map(|a| a.start),
                    location2: field.field.arguments.as_ref().map(|a| a.start),
                });
            }
            // the field can be merged so we collect the selection set
            if let Some(selection_set) = &field.field.selection_set {
                selection_sets.push(&selection_set.item.items);
            }
        }
        let field_call = normalized::FieldCall {
            name: canonical_field.field.name.item.clone(),
            info: schema::NodeInfo {
                generic: &canonical_field.info.generic.info,
                namespaced: canonical_field.info.namespaced,
            },
            arguments,
        };
        if canonical_field.reachable {
            field_calls.insert(reachability.iter().copied().cloned().collect(), field_call);
        }
        alias_selection_sets.push((reachability, selection_sets));
    }
    // TODO: this alias_selectable_type is empty when the underlying type is scalar,
    // maybe processing of alias_selection_sets can be cleaned up
    let normalized_selection_set = match alias_selectable_type {
        Some(selection_type) => normalize_selection_sets(
            namespaced_getter,
            schema,
            fragments,
            variables,
            &selection_type,
            alias_selection_sets,
        )?,
        None => normalized::SelectionSet {
            fields: IndexMap::new(),
            type_name: None,
        },
    };
    Ok((
        // normalized::FieldCalls::Conditional(field_calls),
        field_calls,
        normalized_selection_set,
    ))
}

fn normalize_arguments<'q, 's, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    namespaced_getter: &NSGet,
    schema: &'s schema::Schema<S>,
    variables: &variables::Variables<'q, 's, S>,
    type_name: &ast::TypeName,
    field_name: &ast::Name,
    arguments_schema: &'s BTreeMap<ast::Name, schema::Namespaced<S, schema::InputField<S>>>,
    arguments: Option<&'q Spanning<Vec<executable::Argument>>>,
) -> Result<IndexMap<ast::Name, normalized::InputField<'s, S>>> {
    let mut arguments_map = HashMap::new();
    if let Some(arguments) = arguments {
        for argument in &arguments.item {
            let name = &argument.item.key.item;
            // if the argument already exists, we throw an error
            if arguments_map
                .insert(name, &argument.item.value.item)
                .is_some()
            {
                return Err(Error::DuplicateArguments {
                    type_name: type_name.clone(),
                    field_name: field_name.clone(),
                    argument_name: name.clone(),
                });
            }
        }
    }
    let mut normalized_arguments = IndexMap::new();
    for (name, info) in arguments_schema {
        let argument_value = arguments_map.remove(name);

        // if the argument is allowed for the given namespace
        if let Some((argument_info, namespaced)) = namespaced_getter.get(info) {
            let argument_type = &argument_info.field_type;
            // get the information of the type in the base_type
            let argument_type_info = {
                let argument_type_info = schema
                    .types
                    .get(argument_type.underlying_type())
                    .ok_or_else(|| Error::InternalTypeNotFound {
                        type_name: argument_type.underlying_type().clone(),
                    })?;
                argument_type_info
                    .as_input_type()
                    .ok_or_else(|| Error::InternalNotInputType {
                        type_name: argument_type.underlying_type().clone(),
                        actual_type: argument_type_info.kind(),
                    })?
            };

            let normalized_field_value = match (argument_value, &argument_info.default_value) {
                // Neither argument value nor default value
                (None, None) => {
                    if argument_type.nullable {
                        None
                    } else {
                        return Err(Error::RequiredArgumentNotFound {
                            type_name: type_name.clone(),
                            field_name: field_name.clone(),
                            argument_name: name.clone(),
                        });
                    }
                }

                // if argument value is present, normalize it
                (Some(argument_value), _) => {
                    Some(input::normalize::normalize(
                        schema,
                        namespaced_getter,
                        variables,
                        argument_value,
                        // TODO, this has to change to field info
                        &input::source::LocationType::Field {
                            type_: &argument_info.field_type,
                            default_value: argument_info.default_value.as_ref(),
                        },
                        &argument_type_info,
                    )?)
                }

                // we fall back to the default value, ideally we should've a normalized
                // version of the default value but storing it as such in 'schema' adds
                // a lot of complexity
                (None, Some(default_value)) => {
                    Some(input::normalize::normalize(
                        schema,
                        namespaced_getter,
                        &(),
                        default_value,
                        // TODO, this has to change to field info
                        &input::source::LocationType::Field {
                            type_: &argument_info.field_type,
                            default_value: argument_info.default_value.as_ref(),
                        },
                        &argument_type_info,
                    )?)
                }
            };

            if let Some(normalized_field_value) = normalized_field_value {
                normalized_arguments.insert(
                    name.clone(),
                    normalized::InputField {
                        name: name.clone(),
                        info: schema::NodeInfo {
                            generic: &argument_info.info,
                            namespaced,
                        },
                        value: normalized_field_value,
                    },
                );
            }

        // if the argument isn't allowed for the given  we throw a more
        // useful error message
        } else if argument_value.is_some() {
            return Err(Error::ArgumentNotFound {
                type_name: type_name.clone(),
                field_name: field_name.clone(),
                argument_name: name.clone(),
            });
        }
    }

    // Finally we check if there are any arguments that aren't defined on the field
    if arguments_map.is_empty() {
        Ok(normalized_arguments)
    } else {
        Err(Error::ArgumentsNotFound {
            type_name: type_name.clone(),
            field_name: field_name.clone(),
            argument_names: arguments_map.keys().copied().cloned().collect(),
        })
    }
}
