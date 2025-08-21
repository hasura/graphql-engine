use super::ndc_validation::NDCValidationError;
use crate::data_connectors::DataConnectorContext;
use crate::helpers::ndc_validation;
use crate::helpers::type_mappings;
use crate::helpers::type_validation;
use crate::helpers::typecheck::{
    TypecheckError, TypecheckIssue, typecheck_qualified_type_reference,
};
use crate::helpers::types::{
    TypeRepresentation, get_object_type_for_boolean_expression, get_type_representation,
    unwrap_custom_type_name,
};
use crate::stages::{
    boolean_expressions, data_connector_scalar_types, data_connectors, model_permissions,
    models_graphql, object_relationships, object_types, scalar_types, type_permissions,
};
use crate::types::error::ShouldBeAnError;
use crate::types::error::{Error, TypeError};
use crate::types::permission::ValueExpressionOrPredicate;
use crate::types::subgraph::{ArgumentInfo, ArgumentKind, Qualified, QualifiedTypeReference};

use hasura_authn_core::Role;
use indexmap::IndexMap;
use ndc_models;
use open_dds::arguments::ArgumentName;
use open_dds::data_connector::DataConnectorName;
use open_dds::identifier::SubgraphName;
use open_dds::models::ModelName;
use open_dds::types::DataConnectorArgumentName;
use open_dds::types::{BaseType, CustomTypeName, TypeName, TypeReference};
use std::collections::BTreeMap;
use std::collections::HashSet;

#[derive(Debug, thiserror::Error)]
pub enum ArgumentMappingError {
    #[error(
        "the following arguments referenced in argument mappings are unknown: {}",
        argument_names.join(", ")
    )]
    UnknownArguments { argument_names: Vec<ArgumentName> },
    #[error("argument {argument_name:} is mapped to an unknown argument {ndc_argument_name:}")]
    UnknownNdcArgument {
        argument_name: ArgumentName,
        ndc_argument_name: DataConnectorArgumentName,
    },
    #[error("the mapping for argument {argument_name:} has been defined more than once")]
    DuplicateCommandArgumentMapping { argument_name: ArgumentName },
    #[error("the data connector argument {ndc_argument_name} has been mapped to more than once")]
    DuplicateNdcArgumentMapping {
        ndc_argument_name: DataConnectorArgumentName,
    },
    #[error(
        "the argument {argument_name} is mapped to the data connector argument {ndc_argument_name} which is already used as an argument preset in the DataConnectorLink"
    )]
    ArgumentAlreadyPresetInDataConnectorLink {
        argument_name: ArgumentName,
        ndc_argument_name: DataConnectorArgumentName,
    },
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
        ndc_argument_name: DataConnectorArgumentName,
        type_name: Qualified<CustomTypeName>,
        unknown_ndc_type: String,
    },
    #[error("ndc validation error: {0}")]
    NDCValidationError(NDCValidationError),
}

#[derive(Debug, thiserror::Error)]
pub enum ArgumentMappingIssue {
    #[error(
        "the following data connector arguments are not mapped to an argument: {}",
        ndc_argument_names.join(", ")
    )]
    UnmappedNdcArguments {
        ndc_argument_names: Vec<DataConnectorArgumentName>,
    },
    #[error(
        "the type of argument '{argument_name:}' is not compatible with the type of the data connector argument '{ndc_argument_name:}': {issue:}"
    )]
    IncompatibleType {
        argument_name: ArgumentName,
        ndc_argument_name: DataConnectorArgumentName,
        issue: type_validation::TypeCompatibilityIssue,
    },
}

impl ShouldBeAnError for ArgumentMappingIssue {
    fn should_be_an_error(&self, flags: &open_dds::flags::OpenDdFlags) -> bool {
        match self {
            ArgumentMappingIssue::UnmappedNdcArguments { .. } => false,
            ArgumentMappingIssue::IncompatibleType { .. } => {
                flags.contains(open_dds::flags::Flag::ValidateArgumentMappingTypes)
            }
        }
    }
}

pub struct ArgumentMappingResults<'a> {
    pub argument_mappings: BTreeMap<ArgumentName, DataConnectorArgumentName>,
    pub data_connector_link_argument_presets:
        BTreeMap<DataConnectorArgumentName, data_connectors::ArgumentPresetValue>,
    pub argument_type_mappings_to_resolve: Vec<type_mappings::TypeMappingToCollect<'a>>,
    pub issues: Vec<ArgumentMappingIssue>,
}

pub fn get_argument_mappings<'a>(
    arguments: &'a IndexMap<ArgumentName, ArgumentInfo>,
    argument_mapping: &BTreeMap<ArgumentName, DataConnectorArgumentName>,
    ndc_arguments_types: &'a BTreeMap<DataConnectorArgumentName, ndc_models::Type>,
    data_connector_context: &DataConnectorContext,
    data_connector_scalars: &'a data_connector_scalar_types::DataConnectorScalars,
    object_types: &'a BTreeMap<
        Qualified<CustomTypeName>,
        type_permissions::ObjectTypeWithPermissions,
    >,
    scalar_types: &'a BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &'a boolean_expressions::BooleanExpressionTypes,
) -> Result<ArgumentMappingResults<'a>, ArgumentMappingError> {
    let mut issues = Vec::new();
    let mut unconsumed_argument_mappings: BTreeMap<&ArgumentName, &DataConnectorArgumentName> =
        argument_mapping.iter().collect();
    let mut unmapped_ndc_arguments: HashSet<&DataConnectorArgumentName> =
        ndc_arguments_types.keys().collect();

    let mut resolved_argument_mappings = BTreeMap::<ArgumentName, DataConnectorArgumentName>::new();

    let mut type_mappings_to_collect = Vec::<type_mappings::TypeMappingToCollect>::new();

    for (argument_name, argument_type) in arguments {
        let mapped_to_ndc_argument_name = if let Some(mapped_to_ndc_argument_name) =
            unconsumed_argument_mappings.remove(&argument_name)
        {
            mapped_to_ndc_argument_name.clone()
        } else {
            // If there's no mapping defined for an argument, assume that it
            // implicitly maps to the same name
            DataConnectorArgumentName::from(argument_name.as_str())
        };

        let ndc_argument_type = ndc_arguments_types
            .get(&mapped_to_ndc_argument_name)
            .ok_or_else(|| ArgumentMappingError::UnknownNdcArgument {
                argument_name: argument_name.clone(),
                ndc_argument_name: mapped_to_ndc_argument_name.clone(),
            })?;
        // Validate the type compatibility
        if let Some(issue) = type_validation::validate_type_compatibility(
            data_connector_scalars,
            &argument_type.argument_type,
            ndc_argument_type,
        ) {
            issues.push(ArgumentMappingIssue::IncompatibleType {
                argument_name: argument_name.clone(),
                ndc_argument_name: mapped_to_ndc_argument_name.clone(),
                issue,
            });
        }
        if !unmapped_ndc_arguments.remove(&mapped_to_ndc_argument_name) {
            return Err(ArgumentMappingError::DuplicateNdcArgumentMapping {
                ndc_argument_name: mapped_to_ndc_argument_name.clone(),
            });
        }

        let existing_mapping = resolved_argument_mappings
            .insert(argument_name.clone(), mapped_to_ndc_argument_name.clone());

        if existing_mapping.is_some() {
            return Err(ArgumentMappingError::DuplicateCommandArgumentMapping {
                argument_name: argument_name.clone(),
            });
        }

        // only do further checks if this is not a built-in type
        if let Some(object_type_name) = unwrap_custom_type_name(&argument_type.argument_type) {
            match get_type_representation(
                object_type_name,
                object_types,
                scalar_types,
                boolean_expression_types,
            )
            .map_err(|_| ArgumentMappingError::UnknownType {
                argument_name: argument_name.clone(),
                data_type: object_type_name.clone(),
            })? {
                TypeRepresentation::Object(_) => {
                    let underlying_ndc_argument_named_type =
                        ndc_validation::get_underlying_named_type(ndc_argument_type);

                    type_mappings_to_collect.push(type_mappings::TypeMappingToCollect {
                        type_name: object_type_name,
                        ndc_object_type_name: underlying_ndc_argument_named_type,
                    });
                }
                TypeRepresentation::Scalar(_) | TypeRepresentation::BooleanExpressionScalar(_) => {}
                TypeRepresentation::BooleanExpressionObject(boolean_expression_type) => {
                    let underlying_ndc_argument_named_type =
                        ndc_validation::get_underlying_named_type(ndc_argument_type);

                    // resolve the object type the boolean expression refers to
                    type_mappings_to_collect.push(type_mappings::TypeMappingToCollect {
                        type_name: &boolean_expression_type.object_type,
                        ndc_object_type_name: underlying_ndc_argument_named_type,
                    });
                }
            }
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

    // Mark off any ndc arguments that are preset at the data connector level via
    // DataConnectorLink argument presets
    let mut data_connector_link_argument_presets = BTreeMap::new();
    for argument_preset in &data_connector_context.argument_presets {
        if let Some((argument_name, ndc_argument_name)) = resolved_argument_mappings
            .iter()
            .find(|(_, ndc_argument_name)| *ndc_argument_name == &argument_preset.name)
        {
            return Err(
                ArgumentMappingError::ArgumentAlreadyPresetInDataConnectorLink {
                    argument_name: argument_name.clone(),
                    ndc_argument_name: ndc_argument_name.clone(),
                },
            );
        }

        // We don't care if the argument preset is for an argument that doesn't exist.
        // These presets are set for the whole connector and are skipped when the
        // function/procedure/collection doesn't take that argument.
        if unmapped_ndc_arguments.remove(&argument_preset.name) {
            data_connector_link_argument_presets
                .insert(argument_preset.name.clone(), argument_preset.value.clone());
        }
    }

    // If any unmapped ndc arguments, we have missing arguments or data connector link argument presets
    // We raise this as an issue because existing projects have this issue and we need to be backwards
    // compatible. Those existing projects will probably fail at query time, but they do build and start 😭
    if !unmapped_ndc_arguments.is_empty() {
        issues.push(ArgumentMappingIssue::UnmappedNdcArguments {
            ndc_argument_names: unmapped_ndc_arguments.into_iter().cloned().collect(),
        });
    }

    Ok(ArgumentMappingResults {
        argument_mappings: resolved_argument_mappings,
        data_connector_link_argument_presets,
        argument_type_mappings_to_resolve: type_mappings_to_collect,
        issues,
    })
}

/// resolve a value expression
/// as it may contain a predicate, we also need to provide a
/// type to validate it against to ensure the fields it refers to
/// exist etc
pub(crate) fn resolve_value_expression_for_argument(
    role: Option<&Role>, // this is only applicable for role-based permissions
    flags: &open_dds::flags::OpenDdFlags,
    argument_name: &open_dds::arguments::ArgumentName,
    value_expression: &open_dds::permissions::ValueExpressionOrPredicate,
    argument_type: &QualifiedTypeReference,
    data_connector_link: &data_connectors::DataConnectorLink,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
    data_connector_type_mappings: &BTreeMap<Qualified<CustomTypeName>, object_types::TypeMapping>,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    type_error_mapper: impl Fn(TypecheckError) -> Error,
) -> Result<(ValueExpressionOrPredicate, Vec<TypecheckIssue>), Error> {
    match value_expression {
        open_dds::permissions::ValueExpressionOrPredicate::SessionVariable(session_variable) => {
            Ok::<(ValueExpressionOrPredicate, Vec<TypecheckIssue>), Error>((
                ValueExpressionOrPredicate::SessionVariable(
                    hasura_authn_core::SessionVariableReference {
                        name: session_variable.clone(),
                        passed_as_json: flags.contains(open_dds::flags::Flag::JsonSessionVariables),
                        disallow_unknown_fields: flags
                            .contains(open_dds::flags::Flag::DisallowUnknownValuesInArguments),
                    },
                ),
                vec![],
            ))
        }
        open_dds::permissions::ValueExpressionOrPredicate::Literal(json_value) => {
            let mut issues = vec![];

            // first typecheck the values
            typecheck_qualified_type_reference(
                &object_types
                    .iter()
                    .map(|(field_name, object_type)| (field_name, &object_type.object_type))
                    .collect(), // Convert &BTreeMap<field_name, object_type> to BTreeMap<&field_name, &object_type>
                &boolean_expression_types.get_type_names(),
                argument_type,
                json_value,
                &mut issues,
            )
            .map_err(&type_error_mapper)?;

            // now we have a small problem - our user may be providing us a partial value to fill
            // it with presets etc
            // if they do, let's look at the presets and see if they're going to be filled in later
            let mut filtered_issues = vec![];

            for issue in issues {
                if match issue {
                    TypecheckIssue::ObjectTypeField {
                        error: TypecheckError::NullInNonNullableColumn,
                        ref field_name,
                        ref object_type,
                    } => {
                        let object_type_representation =
                            object_types.get(object_type).ok_or_else(|| {
                                Error::UnknownObjectType {
                                    data_type: object_type.clone(),
                                }
                            })?;

                        // is there a preset for this role and this field?
                        let has_preset = role.is_some_and(|role| {
                            object_type_representation
                                .type_input_permissions
                                .by_role
                                .get(role)
                                .is_some_and(|type_input_permission| {
                                    type_input_permission.field_presets.contains_key(field_name)
                                })
                        });

                        // if the field has no preset, then keep the error as it's legitimate
                        // TODO: consider making this a warning rather than than error when role ==
                        // None as we don't have a solid way of knowing which presets will apply
                        // with rules-based auth
                        !has_preset
                    }
                    _ => true,
                } {
                    filtered_issues.push(issue);
                }
            }

            Ok((
                ValueExpressionOrPredicate::Literal(json_value.clone()),
                filtered_issues,
            ))
        }
        open_dds::permissions::ValueExpressionOrPredicate::BooleanExpression(bool_exp) => {
            // get underlying object type name from argument type (ie, unwrap
            // array, nullability etc)
            let base_type =
                unwrap_custom_type_name(argument_type).ok_or_else(|| Error::ArgumentTypeError {
                    argument_name: argument_name.clone(),
                    type_error: TypeError::NoNamedTypeFound {
                        qualified_type_reference: argument_type.clone(),
                    },
                })?;

            // lookup the relevant boolean expression type and get the underlying object type
            let boolean_expression_type = boolean_expression_types
                .objects
                .get(base_type)
                .ok_or_else(|| Error::UnknownType {
                    data_type: base_type.clone(),
                })?;
            let object_type_representation =
                get_object_type_for_boolean_expression(boolean_expression_type, object_types)?;

            let resolved_model_predicate = model_permissions::resolve_model_predicate_with_type(
                flags,
                bool_exp,
                vec![],
                &boolean_expression_type.object_type,
                object_type_representation,
                Some(boolean_expression_type),
                data_connector_type_mappings,
                data_connector_link,
                data_connector_scalars,
                object_types,
                scalar_types,
                boolean_expression_types,
                models,
            )?;

            Ok((
                ValueExpressionOrPredicate::BooleanExpression(Box::new(resolved_model_predicate)),
                vec![],
            ))
        }
    }
}

// in short, should we convert this to an NDC expression before sending it
pub fn get_argument_kind(
    type_obj: &TypeReference,
    subgraph: &SubgraphName,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
) -> ArgumentKind {
    match &type_obj.underlying_type {
        BaseType::List(type_obj) => get_argument_kind(type_obj, subgraph, boolean_expression_types),
        BaseType::Named(type_name) => match type_name {
            TypeName::Inbuilt(_) => ArgumentKind::Other,
            TypeName::Custom(type_name) => {
                let qualified_type_name = Qualified::new(subgraph.clone(), type_name.to_owned());

                match get_type_representation::<
                    type_permissions::ObjectTypesWithPermissions,
                    scalar_types::ScalarTypeRepresentation,
                >(
                    &qualified_type_name,
                    &BTreeMap::new(),
                    &BTreeMap::new(),
                    boolean_expression_types,
                ) {
                    Ok(
                        TypeRepresentation::BooleanExpressionScalar(_)
                        | TypeRepresentation::BooleanExpressionObject(_),
                    ) => ArgumentKind::NDCExpression,
                    _ => ArgumentKind::Other,
                }
            }
        },
    }
}
