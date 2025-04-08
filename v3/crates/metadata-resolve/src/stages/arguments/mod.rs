mod error;
mod types;

use crate::helpers::boolean_expression::validate_data_connector_with_object_boolean_expression_type;
use crate::helpers::types::{TypeRepresentation, get_type_representation, unwrap_custom_type_name};
use crate::stages::{
    boolean_expressions, commands, data_connectors, models, object_relationships, scalar_types,
};
use crate::types::subgraph::{ArgumentInfo, Qualified};
pub use error::{ArgumentError, ArgumentSource, NamedArgumentError};
use indexmap::IndexMap;
use open_dds::arguments::ArgumentName;
use open_dds::commands::CommandName;
use open_dds::{models::ModelName, types::CustomTypeName};
use std::sync::Arc;
pub use types::ArgumentIssue;

use std::collections::BTreeMap;

use super::object_types;

/// resolve model and command arguments
/// combined because they currently work in exactly the same way
/// this is more of a validation step really, we don't add new info, just explode if things go wrong
pub fn resolve(
    commands: &IndexMap<Qualified<CommandName>, commands::Command>,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    flags: &open_dds::flags::OpenDdFlags,
) -> Result<Vec<ArgumentIssue>, Vec<NamedArgumentError>> {
    let mut results = vec![];
    for command in commands.values() {
        let data_connector_link = command.source.as_ref().map(|source| &source.data_connector);
        let type_mapping = command.source.as_ref().map(|source| &source.type_mappings);

        // check data source and arguments agree
        results.push(validate_arguments_with_source(
            &ArgumentSource::Command(command.name.clone()),
            &command.arguments,
            data_connector_link,
            type_mapping,
            object_types,
            scalar_types,
            boolean_expression_types,
            models,
            flags,
        ));
    }

    for model in models.values() {
        let data_connector_link = model.source.as_ref().map(|source| &source.data_connector);
        let type_mapping = model.source.as_ref().map(|source| &source.type_mappings);

        // check data source and arguments agree
        results.push(validate_arguments_with_source(
            &ArgumentSource::Model(model.name.clone()),
            &model.arguments,
            data_connector_link,
            type_mapping,
            object_types,
            scalar_types,
            boolean_expression_types,
            models,
            flags,
        ));
    }

    // return all errors or all issues
    partition_eithers::collect_any_errors(results).map(|issues_nested| issues_nested.concat())
}

// resolve arguments. if the source is available we check it against any boolean
// expressions used
pub fn validate_arguments_with_source(
    argument_source: &ArgumentSource,
    arguments: &IndexMap<ArgumentName, ArgumentInfo>,
    data_connector_link: Option<&Arc<data_connectors::DataConnectorLink>>,
    source_type_mapping: Option<&BTreeMap<Qualified<CustomTypeName>, object_types::TypeMapping>>,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    flags: &open_dds::flags::OpenDdFlags,
) -> Result<Vec<ArgumentIssue>, NamedArgumentError> {
    let mut issues = vec![];

    for (argument_name, argument_info) in arguments {
        // if our argument is a boolean expression type, we need to check it
        if let Some(custom_type_name) = unwrap_custom_type_name(&argument_info.argument_type) {
            let boolean_expression_type = match get_type_representation(
                custom_type_name,
                object_types,
                scalar_types,
                boolean_expression_types,
            )
            .map_err(|custom_type_name| NamedArgumentError {
                source: argument_source.clone(),
                argument_name: argument_name.clone(),
                error: ArgumentError::UnknownType {
                    type_name: custom_type_name,
                },
            })? {
                TypeRepresentation::BooleanExpressionObject(bool_exp) => Some(bool_exp),
                _ => None,
            };

            // if the type is a boolean expression and we have a data source, we can validate the
            // boolean expression against the data source
            if let (
                Some(boolean_expression_type),
                Some(data_connector_link),
                Some(source_type_mapping),
            ) = (
                boolean_expression_type,
                data_connector_link,
                source_type_mapping,
            ) {
                let data_connector_issues =
                    validate_data_connector_with_object_boolean_expression_type(
                        data_connector_link,
                        source_type_mapping,
                        boolean_expression_type,
                        boolean_expression_types,
                        object_types,
                        models,
                        flags,
                    )
                    .map_err(|boolean_expression_error| {
                        NamedArgumentError {
                            source: argument_source.clone(),
                            argument_name: argument_name.clone(),
                            error: ArgumentError::BooleanExpressionError(boolean_expression_error),
                        }
                    })?;

                issues.extend(data_connector_issues.into_iter().map(|issue| {
                    ArgumentIssue::BooleanExpressionIssue {
                        argument_name: argument_name.clone(),
                        issue,
                    }
                }));
            }
        }
    }

    Ok(issues)
}
