use std::collections::HashMap;

use crate::ast::common as ast;
use crate::ast::executable;
use crate::ast::value as gql;
use crate::http::VariableValues;
use crate::normalized_ast as normalized;
use crate::schema;
use crate::validation::error::*;

use super::input::normalize::*;
use super::input::source::*;

pub struct Variables<'q, 's, S: schema::SchemaContext> {
    variables: HashMap<&'q ast::Name, Variable<'q, 's, S>>,
}

pub(crate) struct Variable<'q, 's, S: schema::SchemaContext> {
    definition: &'q executable::VariableDefinition,
    input_schema: schema::InputType<'s, S>,
    value: Option<VariableValue<'q>>,
}

pub(crate) enum VariableValue<'q> {
    Provided(&'q serde_json::Value, &'q ast::Type),
    Default(&'q gql::ConstValue, &'q ast::Type),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NonNullGraphqlVariablesValidation {
    Validate,
    DoNotValidate,
}

impl VariableValue<'_> {
    /// Normalize the variable value based on the input schema.
    fn normalize<'s, S: schema::SchemaContext, NSGet: schema::NamespacedGetter<S>>(
        &self,
        input_schema: &schema::InputType<'s, S>,
        schema: &'s schema::Schema<S>,
        namespaced_getter: &NSGet,
        validate_non_null_graphql_variables: &NonNullGraphqlVariablesValidation,
    ) -> Result<normalized::Value<'s, S>> {
        match self {
            VariableValue::Provided(value, type_) => normalize(
                schema,
                namespaced_getter,
                &(),
                *value,
                &LocationType::NoLocation { type_ },
                input_schema,
                validate_non_null_graphql_variables,
            ),
            VariableValue::Default(value, type_) => normalize(
                schema,
                namespaced_getter,
                &(),
                *value,
                &LocationType::NoLocation { type_ },
                input_schema,
                validate_non_null_graphql_variables,
            ),
        }
    }
}

impl<'q, 's, S: schema::SchemaContext> Variables<'q, 's, S> {
    pub(crate) fn new(
        operation: &'q executable::OperationDefinition,
        variable_values: &'q VariableValues,
        schema: &'s schema::Schema<S>,
        validate_non_null_graphql_variables: &NonNullGraphqlVariablesValidation,
    ) -> Result<Self> {
        let mut variables = HashMap::new();
        if let Some(variable_definitions) = &operation.variable_definitions {
            for variable_definition in &variable_definitions.item {
                let definition = &variable_definition.item;
                let variable_name = &definition.name.item;
                let variable_type = &definition.var_type.item;
                let variable_base_type = variable_type.underlying_type();
                let input_schema = schema
                    .get_type(variable_base_type)
                    .ok_or_else(|| Error::UnknownType(variable_base_type.clone()))?
                    .as_input_type() // Get the input type information for the variable
                    .ok_or_else(|| Error::NotInputType {
                        variable_name: variable_name.clone(),
                        type_name: variable_base_type.clone(),
                    })?;
                let variable_nullable = variable_type.nullable;
                let value = match (
                    variable_values.get(variable_name), // Get the value from the variable values
                    &definition.default_value, // Get the default value from the variable definition
                ) {
                    (None, Some(default_value)) => {
                        // Only consider default values if the variable is not provided.
                        // Ref: https://spec.graphql.org/October2021/#sel-KANLLFCFFNABABC3vT
                        Some(VariableValue::Default(&default_value.item, variable_type))
                    }
                    (None, None) => {
                        if !variable_nullable
                            && *validate_non_null_graphql_variables
                                == NonNullGraphqlVariablesValidation::Validate
                        {
                            // If the variable is not provided and not nullable, return an error.
                            // Ref: https://spec.graphql.org/October2021/#sel-KANLLFCFFPABABJnxZ
                            return Err(Error::ExpectingValueForNonNullableVariable {
                                variable_name: variable_name.clone(),
                                variable_type: variable_type.clone(),
                            });
                        }
                        None
                    }
                    (Some(value), _) => {
                        // If a value is provided, use it.
                        if !variable_nullable
                            && value.is_null()
                            && *validate_non_null_graphql_variables
                                == NonNullGraphqlVariablesValidation::Validate
                        {
                            // If the variable is not nullable and the value is null, return an error.
                            // Ref: https://spec.graphql.org/October2021/#sel-KANLLFCFFPABABJnxZ
                            return Err(Error::NullValueForNonNullVariable {
                                variable_name: variable_name.clone(),
                                variable_type: variable_type.clone(),
                            });
                        }
                        Some(VariableValue::Provided(value, variable_type))
                    }
                };
                let variable = Variable {
                    definition,
                    input_schema,
                    value,
                };
                if variables.insert(variable_name, variable).is_some() {
                    // If the variable is defined more than once, return an error.
                    return Err(Error::DuplicateVariableDeclarations {
                        variable_name: variable_name.clone(),
                    });
                }
            }
        }
        Ok(Self { variables })
    }

    pub(crate) fn get<NSGet: schema::NamespacedGetter<S>>(
        &self,
        location_type: &LocationType<'q, 's>,
        variable_name: &ast::Name,
        schema: &'s schema::Schema<S>,
        namespaced_getter: &NSGet,
        validate_non_null_graphql_variables: &NonNullGraphqlVariablesValidation,
    ) -> Result<normalized::Value<'s, S>> {
        let variable =
            self.variables
                .get(variable_name)
                .ok_or_else(|| Error::VariableNotDefined {
                    variable_name: variable_name.clone(),
                })?;
        let variable_definition = &variable.definition;
        let variable_type = &variable_definition.var_type.item;

        // Ideally we would want to call are_types_compatible(location_type, variable_type) but a
        // default value can be provided despite variable_type being nullable through a variable's
        // default value or an argument's default value or a field's default value. So we check
        // for this condition first and then call 'are_types_compatible'
        //
        let variable_spread_allowed = if location_type.default_value().is_some()
            || variable_definition.default_value.is_some()
        {
            are_base_types_compatible(&variable_type.base, &location_type.type_().base)
        } else {
            are_types_compatible(variable_type, location_type.type_())
        };

        if variable_spread_allowed {
            match &variable.value {
                None => {
                    // If the variable is not provided, use the default value at the location.
                    if let Some(default_value) = location_type.default_value() {
                        // Normalize the default value.
                        normalize(
                            schema,
                            namespaced_getter,
                            &(),
                            default_value,
                            location_type,
                            &variable.input_schema,
                            validate_non_null_graphql_variables,
                        )
                    } else {
                        // If the variable is not provided and there is no default value, return null.
                        Ok(normalized::Value::SimpleValue(
                            normalized::SimpleValue::Null,
                        ))
                    }
                }
                Some(variable_value) => {
                    // Normalize the variable value.
                    variable_value.normalize(
                        &variable.input_schema,
                        schema,
                        namespaced_getter,
                        validate_non_null_graphql_variables,
                    )
                }
            }
        } else {
            Err(Error::VariableSpreadNotAllowed {
                variable_name: variable_name.clone(),
                variable_type: variable_type.clone(),
                location_type: location_type.type_().clone(),
            })
        }
    }
}

fn are_base_types_compatible(variable_type: &ast::BaseType, location_type: &ast::BaseType) -> bool {
    match (variable_type, location_type) {
        (ast::BaseType::Named(variable_type_name), ast::BaseType::Named(location_type_name)) => {
            variable_type_name == location_type_name
        }
        (ast::BaseType::List(variable_list_type), ast::BaseType::List(location_list_type)) => {
            are_types_compatible(variable_list_type, location_list_type)
        }
        _ => false,
    }
}

fn are_types_compatible(variable_type: &ast::Type, location_type: &ast::Type) -> bool {
    fn check_nullability(variable_nullability: bool, location_nullability: bool) -> bool {
        location_nullability || !variable_nullability
    }
    are_base_types_compatible(&variable_type.base, &location_type.base)
        && check_nullability(variable_type.nullable, location_type.nullable)
}
