use std::borrow::Cow;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;

use super::error::*;
use crate::ast::common as ast;
use crate::ast::executable;
use crate::ast::spanning;
use crate::schema;

// { # vec (typename, field)
//   node(id: "ZmlsbXM6MQ==") {
//     ... on Film {
//       ... on Node {
//         ... on Planet {
//           # Node.Film.Node.Planet
//           title: diameter
//         }
//       }
//       title
//     }
//   }
// }
//

#[derive(Debug)]
pub struct SelectableType<'s, S: schema::SchemaContext> {
    pub(super) type_name: &'s ast::TypeName,
    pub(super) possible_types: HashSet<&'s ast::TypeName>,
    fields: Option<&'s BTreeMap<ast::Name, schema::Namespaced<S, schema::Field<S>>>>,
}

impl<'s, S: schema::SchemaContext> SelectableType<'s, S> {
    fn lookup_field<NSGet: schema::NamespacedGetter<S>>(
        &self,
        namespaced_getter: &NSGet,
        field_name: &ast::Name,
    ) -> Result<FieldInfo<'s, S>> {
        let field_value =
            self.fields
                .and_then(|m| m.get(field_name))
                .ok_or_else(|| Error::NoFieldOnType {
                    type_name: self.type_name.clone(),
                    field_name: field_name.clone(),
                })?;
        let (generic, namespaced) =
            namespaced_getter
                .get(field_value)
                .ok_or_else(|| Error::NoFieldOnType {
                    type_name: self.type_name.clone(),
                    field_name: field_name.clone(),
                })?;
        Ok(FieldInfo {
            generic,
            namespaced,
        })
    }
}

impl<S: schema::SchemaContext> schema::Object<S> {
    fn to_selectable_type(&self) -> SelectableType<S> {
        SelectableType {
            type_name: &self.name,
            fields: Some(&self.fields),
            possible_types: self.possible_types(),
        }
    }
}

impl<S: schema::SchemaContext> schema::Interface<S> {
    fn to_selectable_type(&self) -> SelectableType<S> {
        SelectableType {
            type_name: &self.name,
            fields: Some(&self.fields),
            possible_types: self.possible_types(),
        }
    }
}

impl<S: schema::SchemaContext> schema::Union<S> {
    fn to_selectable_type(&self) -> SelectableType<S> {
        SelectableType {
            type_name: &self.name,
            fields: Some(self.get_fields()),
            possible_types: self.possible_types(),
        }
    }
}

impl<S: schema::SchemaContext> schema::TypeInfo<S> {
    pub(super) fn to_selectable_type(&self) -> Option<SelectableType<S>> {
        match self {
            schema::TypeInfo::Interface(interface) => Some(interface.to_selectable_type()),
            schema::TypeInfo::Object(object) => Some(object.to_selectable_type()),
            schema::TypeInfo::Union(union) => Some(union.to_selectable_type()),
            schema::TypeInfo::Scalar(_)
            | schema::TypeInfo::Enum(_)
            | schema::TypeInfo::InputObject(_) => None,
        }
    }
}

fn get_type_info<'s, S: schema::SchemaContext>(
    schema: &'s schema::Schema<S>,
    type_name: &ast::TypeName,
) -> Result<&'s schema::TypeInfo<S>> {
    schema
        .get_type(type_name)
        .ok_or_else(|| Error::UnknownType(type_name.clone()))
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct FieldInfo<'s, S: schema::SchemaContext> {
    pub generic: &'s schema::Field<S>,
    pub namespaced: &'s S::NamespacedNodeInfo,
}

pub(super) struct CollectedField<'q, 's, S: schema::SchemaContext> {
    pub alias: &'q ast::Name,
    pub info: FieldInfo<'s, S>,
    pub field_path: Vec<&'s ast::TypeName>,
    pub reachable: bool,
    pub field: &'q executable::Field,
}

#[allow(clippy::too_many_arguments)]
fn collect_fields_from_fragment<
    'q,
    's,
    S: schema::SchemaContext,
    NSGet: schema::NamespacedGetter<S>,
>(
    namespaced_getter: &NSGet,
    schema: &'s schema::Schema<S>,
    fragments: &HashMap<&'q ast::Name, &'q executable::FragmentDefinition>,

    field_path: &Vec<&'s ast::TypeName>,
    selection_type: &SelectableType<'s, S>,
    coerce_as: Option<&SelectableType<'s, S>>,
    selection_set_reachability: &HashSet<&'s ast::TypeName>,
    fragment_selection_type: &SelectableType<'s, S>,
    fragment_selection_set: &'q executable::SelectionSet,
    fields: &mut Vec<CollectedField<'q, 's, S>>,
) -> Result<()> {
    let common_types: HashSet<&ast::TypeName> = selection_type
        .possible_types
        .intersection(&fragment_selection_type.possible_types)
        .copied()
        .collect();
    if common_types.is_empty() {
        return Err(Error::FragmentCannotBeSpread {
            selection_type: selection_type.type_name.clone(),
            fragment_type: fragment_selection_type.type_name.clone(),
        });
    }
    let fragment_reachability = selection_set_reachability
        .intersection(&common_types)
        .copied()
        .collect();
    let mut fragment_field_path = Cow::Borrowed(field_path);
    let fragment_to_be_coerced_as = if fragment_selection_type.type_name == selection_type.type_name
    {
        coerce_as
    // is selection_type a subtype of fragment_selection_type? If so, we
    // can coerce the fragment as selecton_type
    // For example, in this query
    // {
    //   articleByPk(..): {
    //     ... on Node { id }
    //   }
    // }
    // we would like to coerce the selection on Node as Article
    // But the opposite isn't true
    // {
    //   node(..): {
    //     ... on Article { article_id title }
    //   }
    // }
    // we can't coerce the selection set on Article as Node
    //
    // TODO: Unfortunately the information captured in 'SelectionType' isn't
    // enough to make a subtype check, we'll need to also capture 'implements'
    // in SelectionSet to correctly determine subtyping.
    //
    // For now, this rudimentary check only handles object types implementing interfaces
    // ignoring the cases where interfaces implement other interfaces
    } else if fragment_selection_type
        .possible_types
        .contains(selection_type.type_name)
    {
        Some(coerce_as.unwrap_or(selection_type))
    } else {
        fragment_field_path
            .to_mut()
            .push(fragment_selection_type.type_name);
        None
    };
    collect_fields_internal(
        namespaced_getter,
        schema,
        fragments,
        &fragment_field_path,
        fragment_selection_type,
        &fragment_reachability,
        fragment_to_be_coerced_as,
        &fragment_selection_set.items,
        fields,
    )?;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(super) fn collect_fields<
    'q,
    's,
    S: schema::SchemaContext,
    NSGet: schema::NamespacedGetter<S>,
>(
    namespaced_getter: &NSGet,
    schema: &'s schema::Schema<S>,
    fragments: &HashMap<&'q ast::Name, &'q executable::FragmentDefinition>,
    field_path: &Vec<&'s ast::TypeName>,
    selection_type: &SelectableType<'s, S>,
    selection_set: &'q [spanning::Spanning<executable::Selection>],
    fields: &mut Vec<CollectedField<'q, 's, S>>,
) -> Result<()> {
    // let selection_set_field_path = SelectionSetfield_path::Unconditional {
    //     root_type: selection_type,
    //     reachable_types: selection_type.possible_types.clone(),
    // };
    collect_fields_internal(
        namespaced_getter,
        schema,
        fragments,
        field_path,
        selection_type,
        &selection_type.possible_types,
        None,
        selection_set,
        fields,
    )
}

#[allow(clippy::too_many_arguments)]
fn collect_fields_internal<'q, 's, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
    namespaced_getter: &NSGet,
    schema: &'s schema::Schema<S>,
    fragments: &HashMap<&'q ast::Name, &'q executable::FragmentDefinition>,
    field_path: &Vec<&'s ast::TypeName>,
    selection_type: &SelectableType<'s, S>,
    selection_set_reachability: &HashSet<&'s ast::TypeName>,
    selection_sub_type: Option<&SelectableType<'s, S>>,
    selection_set: &'q [spanning::Spanning<executable::Selection>],
    fields: &mut Vec<CollectedField<'q, 's, S>>,
) -> Result<()> {
    for selection in selection_set {
        match &selection.item {
            executable::Selection::Field(field) => {
                let field_info =
                    selection_type.lookup_field(namespaced_getter, &field.name.item)?;
                let alias = &field
                    .alias
                    .as_ref()
                    .map_or(&field.name.item, |alias| &alias.item.0);

                // refine the field info by sub_type if needed
                let refined_field_info = if let Some(sub_type) = selection_sub_type {
                    sub_type
                        .lookup_field(namespaced_getter, &field.name.item)
                        // this is an internal error because the subtype should
                        // definitely have the field
                        .map_err(|_| Error::InternalNoFieldOnSubtype {
                            type_name: sub_type.type_name.clone(),
                            sub_type_name: selection_type.type_name.clone(),
                            field_name: field.name.item.clone(),
                        })?
                } else {
                    field_info
                };

                fields.push(CollectedField {
                    alias,
                    field_path: field_path.clone(),
                    info: refined_field_info,
                    field,
                    reachable: !selection_set_reachability.is_empty(),
                });
            }
            executable::Selection::FragmentSpread(spread) => {
                let fragment_name = &spread.fragment_name.item;
                let fragment_definition = fragments
                    .get(&spread.fragment_name.item)
                    .ok_or_else(|| Error::UnknownFragment(fragment_name.clone()))?;
                let fragment_type_name = &fragment_definition.type_condition.item.on.item;
                let fragment_type_info = get_type_info(schema, fragment_type_name)?;
                let fragment_selection_type =
                    fragment_type_info.to_selectable_type().ok_or_else(|| {
                        Error::FragmentOnNonCompositeType {
                            fragment_name: Some(fragment_name.clone()),
                            type_name: selection_type.type_name.clone(),
                        }
                    })?;
                collect_fields_from_fragment(
                    namespaced_getter,
                    schema,
                    fragments,
                    field_path,
                    selection_type,
                    selection_sub_type,
                    selection_set_reachability,
                    &fragment_selection_type,
                    &fragment_definition.selection_set.item,
                    fields,
                )?;
            }
            executable::Selection::InlineFragment(spread) => {
                let fragment_selection_type = match &spread.type_condition {
                    Some(type_condition) => {
                        let fragment_type_name = &type_condition.item.on.item;
                        let fragment_type_info = get_type_info(schema, fragment_type_name)?;
                        Ok(Some(fragment_type_info.to_selectable_type().ok_or_else(
                            || Error::FragmentOnNonCompositeType {
                                fragment_name: None,
                                type_name: selection_type.type_name.clone(),
                            },
                        )?))
                    }
                    None => Ok(None),
                }?;
                collect_fields_from_fragment(
                    namespaced_getter,
                    schema,
                    fragments,
                    field_path,
                    selection_type,
                    selection_sub_type,
                    selection_set_reachability,
                    fragment_selection_type.as_ref().unwrap_or(selection_type),
                    &spread.selection_set.item,
                    fields,
                )?;
            }
        }
    }
    Ok(())
}
