use std::collections::HashMap;
use std::collections::HashSet;

use crate::ast::common as ast;
use crate::ast::executable;
use crate::http::VariableValues;
use crate::normalized_ast as normalized;
use crate::{http, schema};

mod collect;
mod error;
pub mod input;
pub mod selection_set;

pub use error::*;
use indexmap::IndexMap;
use indexmap::IndexSet;

pub fn normalize_request<'s, S: schema::SchemaContext>(
    namespace: &S::Namespace,
    schema: &'s schema::Schema<S>,
    request: &http::Request,
) -> Result<normalized::Operation<'s, S>> {
    let mut fragments = HashMap::new();
    let mut operations = HashMap::new();
    for definition in &request.query.items {
        match &definition.item {
            executable::ExecutableDefinition::Operation(operation) => {
                if operations
                    .insert(operation.name.as_ref().map(|n| &n.item), operation)
                    .is_some()
                {
                    match &operation.name {
                        Some(operation_name) => {
                            return Err(Error::DuplicateOperationDefinitions {
                                operation_name: operation_name.item.clone(),
                            })
                        }
                        None => return Err(Error::AnonymousOperationMustBeUnique),
                    }
                }
            }
            executable::ExecutableDefinition::Fragment(fragment) => {
                if fragments.insert(&fragment.name.item, fragment).is_some() {
                    return Err(Error::DuplicateFragmentDefinitions {
                        fragment_name: fragment.name.item.clone(),
                    });
                }
            }
        }
    }
    {
        let mut visited_fragments = HashSet::new();
        let mut fragment_path = IndexSet::new();
        for (fragment_name, fragment_definition) in &fragments {
            if !visited_fragments.contains(fragment_name) {
                check_fragment_cycles(
                    &mut visited_fragments,
                    &mut fragment_path,
                    &fragments,
                    &fragment_definition.selection_set.item,
                )?
            }
        }
    }
    // TODO, lots of validation cases to be handled here
    let operation_name = request.operation_name.as_ref();
    if let Some(&operation) = operations.get(&operation_name) {
        normalize_operation(namespace, schema, &fragments, operation, &request.variables)
    } else if let Some(operation_name) = operation_name {
        Err(Error::OperationNotFound {
            operation_name: operation_name.clone(),
        })
    } else if operations.len() == 1 {
        normalize_operation(
            namespace,
            schema,
            &fragments,
            operations.values().next().unwrap(),
            &request.variables,
        )
    } else {
        Err(Error::AnonymousOperationNotFound)
    }
}

pub fn check_fragment_cycles<'q>(
    all_referenced_fragments: &mut HashSet<&'q ast::Name>,
    fragment_path: &mut IndexSet<&'q ast::Name>,
    fragments: &HashMap<&'q ast::Name, &'q executable::FragmentDefinition>,
    current_selection_set: &'q executable::SelectionSet,
) -> Result<()> {
    for selection in &current_selection_set.items {
        match &selection.item {
            executable::Selection::Field(field) => {
                if let Some(field_selection_set) = &field.selection_set {
                    check_fragment_cycles(
                        all_referenced_fragments,
                        fragment_path,
                        fragments,
                        &field_selection_set.item,
                    )?
                }
            }
            executable::Selection::FragmentSpread(spread) => {
                let fragment_name = &spread.fragment_name.item;
                if fragment_path.contains(fragment_name) {
                    let mut path = fragment_path.iter().cloned().cloned().collect::<Vec<_>>();
                    path.push(fragment_name.clone());
                    return Err(Error::CycleDetected(path));
                }
                fragment_path.insert(fragment_name);
                all_referenced_fragments.insert(fragment_name);
                let fragment_definition = fragments
                    .get(&spread.fragment_name.item)
                    .ok_or_else(|| Error::UnknownFragment(fragment_name.clone()))?;
                check_fragment_cycles(
                    all_referenced_fragments,
                    fragment_path,
                    fragments,
                    &fragment_definition.selection_set.item,
                )?;
                fragment_path.pop();
            }
            executable::Selection::InlineFragment(inline_fragment) => check_fragment_cycles(
                all_referenced_fragments,
                fragment_path,
                fragments,
                &inline_fragment.selection_set.item,
            )?,
        }
    }
    Ok(())
}

pub fn normalize_operation<'q, 's, S: schema::SchemaContext>(
    namespace: &S::Namespace,
    schema: &'s schema::Schema<S>,
    fragments: &HashMap<&'q ast::Name, &'q executable::FragmentDefinition>,
    operation: &'q executable::OperationDefinition,
    variable_values: &'q VariableValues,
) -> Result<normalized::Operation<'s, S>> {
    let mut variables = HashMap::new();
    if let Some(variable_definitions) = &operation.variable_definitions {
        for definition in &variable_definitions.item {
            let definition = &definition.item;
            let variable_name = &definition.name.item;
            let variable_base_type = definition.var_type.item.underlying_type();
            let type_info = schema
                .get_type(variable_base_type)
                .ok_or_else(|| Error::UnknownType(variable_base_type.clone()))?
                .as_input_type()
                .ok_or_else(|| Error::NotInputType {
                    variable_name: variable_name.clone(),
                    type_name: variable_base_type.clone(),
                })?;
            if variables
                .insert(variable_name, (definition, type_info))
                .is_some()
            {
                return Err(Error::DuplicateVariableDeclarations {
                    variable_name: variable_name.clone(),
                });
            }
        }
    }
    let variables_context = input::value::Variables {
        definitions: &variables,
        values: variable_values,
    };

    let selection_set_type_name = match operation.ty {
        ast::OperationType::Query => &schema.query_type,
        ast::OperationType::Mutation => schema
            .mutation_type
            .as_ref()
            .ok_or(Error::NoMutationsAreDefined)?,
        ast::OperationType::Subscription => schema
            .mutation_type
            .as_ref()
            .ok_or(Error::NoSubscriptionsAreDefined)?,
    };

    let selection_set_type_info = schema
        .get_type(selection_set_type_name)
        .ok_or_else(|| Error::InternalTypeNotFound {
            type_name: selection_set_type_name.clone(),
        })?
        .to_selectable_type()
        .ok_or(Error::InternalSelectionRootIsNotObject)?;

    let normalized_selection_set = selection_set::normalize_selection_set(
        namespace,
        schema,
        fragments,
        &variables_context,
        &selection_set_type_info,
        &operation.selection_set.item,
    )?;
    Ok(normalized::Operation {
        ty: operation.ty,
        name: operation.name.as_ref().map(|name| name.item.clone()),
        directives: IndexMap::new(),
        selection_set: normalized_selection_set,
    })
}
