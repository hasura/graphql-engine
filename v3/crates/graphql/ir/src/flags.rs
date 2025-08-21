use lang_graphql::validation::NonNullGraphqlVariablesValidation;

// feature flags for creating IR
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GraphqlIrFlags {
    // needed to ensure we treat nullability correctly. Once we no longer need the `ValidateNonNullGraphqlVariables`
    // flag delete this flag as it's threaded everywhere and makes quite a mess.
    pub validate_non_null_graphql_variables: NonNullGraphqlVariablesValidation,
    // ensure we create the correct IR for filtering nested arrays
    pub fix_exists_in_nested_arrays: bool,
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
            fix_exists_in_nested_arrays: runtime_flags
                .contains(metadata_resolve::flags::ResolvedRuntimeFlag::FixExistsInNestedArrays),
        }
    }
}
