use crate::helpers::boolean_expression::validate_data_connector_with_object_boolean_expression_type;
use crate::helpers::types::{get_type_representation, unwrap_custom_type_name, TypeRepresentation};
use crate::stages::{
    boolean_expressions, commands, data_connectors, models, object_relationships, scalar_types,
};
use crate::types::error::Error;
use crate::types::subgraph::{ArgumentInfo, Qualified};
use indexmap::IndexMap;
use open_dds::arguments::ArgumentName;
use open_dds::commands::CommandName;
use std::sync::Arc;

use open_dds::{models::ModelName, types::CustomTypeName};

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
) -> Result<Vec<boolean_expressions::BooleanExpressionIssue>, Error> {
    let mut issues = vec![];
    for command in commands.values() {
        let data_connector_link = command.source.as_ref().map(|source| &source.data_connector);
        let type_mapping = command.source.as_ref().map(|source| &source.type_mappings);

        // check data source and arguments agree
        issues.extend(validate_arguments_with_source(
            &command.arguments,
            data_connector_link,
            type_mapping,
            object_types,
            scalar_types,
            boolean_expression_types,
            models,
            flags,
        )?);
    }

    for model in models.values() {
        let data_connector_link = model.source.as_ref().map(|source| &source.data_connector);
        let type_mapping = model.source.as_ref().map(|source| &source.type_mappings);

        // check data source and arguments agree
        issues.extend(validate_arguments_with_source(
            &model.arguments,
            data_connector_link,
            type_mapping,
            object_types,
            scalar_types,
            boolean_expression_types,
            models,
            flags,
        )?);
    }

    Ok(issues)
}

// resolve arguments. if the source is available we check it against any boolean
// expressions used
pub fn validate_arguments_with_source(
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
) -> Result<Vec<boolean_expressions::BooleanExpressionIssue>, Error> {
    let mut issues = vec![];

    for argument_info in arguments.values() {
        // if our argument is a boolean expression type, we need to check it
        if let Some(custom_type_name) = unwrap_custom_type_name(&argument_info.argument_type) {
            let boolean_expression_type = match get_type_representation(
                custom_type_name,
                object_types,
                scalar_types,
                boolean_expression_types,
            )? {
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
                    )?;
                issues.extend(data_connector_issues);
            }
        }
    }

    Ok(issues)
}
