use lang_graphql::validation::NonNullGraphqlVariablesValidation;

// feature flags for creating IR
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GraphqlIrFlags {
    // needed to ensure we treat nullability correctly. Once we no longer need the `ValidateNonNullGraphqlVariables`
    // flag delete this flag as it's threaded everywhere and makes quite a mess.
    pub validate_non_null_graphql_variables: NonNullGraphqlVariablesValidation,
}

impl GraphqlIrFlags {
    pub fn from_runtime_flags(runtime_flags: &metadata_resolve::RuntimeFlags) -> GraphqlIrFlags {
        GraphqlIrFlags {
            validate_non_null_graphql_variables: if runtime_flags.contains(
                metadata_resolve::flags::ResolvedRuntimeFlag::ValidateNonNullGraphqlVariables,
            ) {
                NonNullGraphqlVariablesValidation::Validate
            } else {
                NonNullGraphqlVariablesValidation::DoNotValidate
            },
        }
    }
}
