use crate::metadata::resolved::ndc_validation;
use crate::metadata::resolved::subgraph::{Qualified, QualifiedTypeReference};
use crate::metadata::resolved::types::{
    get_underlying_object_type_or_unknown_type, TypeMappingToResolve, TypeRepresentation,
};
use indexmap::IndexMap;
use itertools::Itertools;
use ndc_client as ndc;
use open_dds::arguments::ArgumentName;
use open_dds::types::CustomTypeName;
use std::collections::{BTreeMap, HashMap};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ArgumentMappingError {
    #[error(
        "the following arguments referenced in argument mappings are unknown: {}",
        comma_separate_argument_names(argument_names)
    )]
    UnknownArguments { argument_names: Vec<ArgumentName> },
    #[error("argument {argument_name:} is mapped to an unknown argument {ndc_argument_name:}")]
    UnknownNdcArgument {
        argument_name: ArgumentName,
        ndc_argument_name: String,
    },
    #[error("the mapping for argument {argument_name:} has been defined more than once")]
    DuplicateCommandArgumentMapping { argument_name: ArgumentName },
    #[error("{argument_name:} has the data type {data_type:} that has not been defined")]
    UnknownType {
        argument_name: ArgumentName,
        data_type: Qualified<CustomTypeName>,
    },
    #[error(
        "the type {unknown_ndc_type:} is not defined as an object type in the connector's schema. This type is being mapped to by the type {type_name:} used in argument {argument_name:} which is mapped to the data connector argument {ndc_argument_name:}"
    )]
    UnknownNdcType {
        argument_name: ArgumentName,
        ndc_argument_name: String,
        type_name: Qualified<CustomTypeName>,
        unknown_ndc_type: String,
    },
}

fn comma_separate_argument_names(argument_names: &[ArgumentName]) -> String {
    argument_names.iter().map(|a| a.0.as_str()).join(", ")
}

pub fn get_argument_mappings<'a>(
    arguments: &'a IndexMap<ArgumentName, QualifiedTypeReference>,
    argument_mapping: &HashMap<ArgumentName, String>,
    ndc_arguments: &'a BTreeMap<String, ndc::models::ArgumentInfo>,
    ndc_object_types: &'a BTreeMap<String, ndc::models::ObjectType>,
    all_type_representations: &'a HashMap<Qualified<CustomTypeName>, TypeRepresentation>,
) -> Result<(HashMap<ArgumentName, String>, Vec<TypeMappingToResolve<'a>>), ArgumentMappingError> {
    let mut unconsumed_argument_mappings: HashMap<&ArgumentName, &String> =
        HashMap::from_iter(argument_mapping.iter());
    let mut resolved_argument_mappings = HashMap::<ArgumentName, String>::new();
    let mut type_mappings_to_resolve = Vec::<TypeMappingToResolve>::new();
    for (argument_name, argument_type) in arguments {
        let mapped_to_ndc_argument_name = if let Some(mapped_to_ndc_argument_name) =
            unconsumed_argument_mappings.remove(&argument_name)
        {
            mapped_to_ndc_argument_name.clone()
        } else {
            // If there's no mapping defined for an argument, assume that it
            // implicitly maps to the same name
            argument_name.to_string()
        };

        let ndc_argument_info =
            ndc_arguments
                .get(&mapped_to_ndc_argument_name)
                .ok_or_else(|| ArgumentMappingError::UnknownNdcArgument {
                    argument_name: argument_name.clone(),
                    ndc_argument_name: mapped_to_ndc_argument_name.clone(),
                })?;

        let existing_mapping = resolved_argument_mappings
            .insert(argument_name.clone(), mapped_to_ndc_argument_name.clone());
        if existing_mapping.is_some() {
            return Err(ArgumentMappingError::DuplicateCommandArgumentMapping {
                argument_name: argument_name.clone(),
            });
        }

        if let Some(object_type_name) =
            get_underlying_object_type_or_unknown_type(argument_type, all_type_representations)
                .map_err(|custom_type_name| ArgumentMappingError::UnknownType {
                    argument_name: argument_name.clone(),
                    data_type: custom_type_name.clone(),
                })?
        {
            let underlying_ndc_argument_named_type =
                ndc_validation::get_underlying_named_type(&ndc_argument_info.argument_type);
            let ndc_argument_object_type = ndc_object_types
                .get(underlying_ndc_argument_named_type)
                .ok_or_else(|| ArgumentMappingError::UnknownNdcType {
                    argument_name: argument_name.clone(),
                    ndc_argument_name: mapped_to_ndc_argument_name.clone(),
                    type_name: object_type_name.clone(),
                    unknown_ndc_type: underlying_ndc_argument_named_type.into(),
                })?;

            type_mappings_to_resolve.push(TypeMappingToResolve {
                type_name: object_type_name,
                ndc_object_type_name: underlying_ndc_argument_named_type,
                ndc_object_type: ndc_argument_object_type,
            })
        }
    }

    // If any unconsumed argument mappings, these do not exist as actual arguments
    let unconsumed_argument_names = unconsumed_argument_mappings
        .into_keys()
        .cloned()
        .collect::<Vec<_>>();
    if !unconsumed_argument_names.is_empty() {
        return Err(ArgumentMappingError::UnknownArguments {
            argument_names: unconsumed_argument_names,
        });
    }

    Ok((resolved_argument_mappings, type_mappings_to_resolve))
}
