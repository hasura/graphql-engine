//! Expsoses OpenDD commands as table valued functions

use async_trait::async_trait;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::{any::Any, sync::Arc};

use metadata_resolve::{self as resolved, Qualified};
use open_dds::arguments::ArgumentName;
use open_dds::identifier::SubgraphName;
use open_dds::{commands::CommandName, types::CustomTypeName};
use resolved::TypeMapping;

use super::model::WithSession;
use crate::execute::planner::command::{CommandOutput, CommandQuery};
use crate::execute::planner::scalar::{parse_datafusion_literal, parse_struct_literal};

mod datafusion {
    pub(super) use datafusion::{
        arrow::array::StructArray,
        arrow::datatypes::{DataType, Field, SchemaBuilder, SchemaRef},
        catalog::Session,
        common::{DFSchema, DFSchemaRef},
        datasource::{function::TableFunctionImpl, TableProvider, TableType},
        error::{DataFusionError, Result},
        logical_expr::ColumnarValue,
        logical_expr::{Expr, Extension, LogicalPlan},
        physical_plan::ExecutionPlan,
        scalar::ScalarValue,
    };
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ArgumentInfo {
    pub argument_type: datafusion::DataType,
    pub is_nullable: bool,
    pub description: Option<String>,
}

impl ArgumentInfo {
    fn from_resolved(
        metadata: &resolved::Metadata,
        command_source: &Option<resolved::CommandSource>,
        argument: &resolved::ArgumentInfo,
    ) -> Option<Self> {
        // TODO, ArgumentInfo doesn't have the underlying scalar representation
        let argument_type = super::model::to_arrow_type(
            metadata,
            &argument.argument_type.underlying_type,
            command_source.as_ref().map(|source| &source.type_mappings),
            None,
        )?;
        let argument_info = ArgumentInfo {
            argument_type,
            is_nullable: argument.argument_type.nullable,
            description: argument.description.clone(),
        };
        Some(argument_info)
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct Command {
    pub subgraph: SubgraphName,
    pub name: CommandName,

    pub description: Option<String>,

    pub arguments: IndexMap<ArgumentName, ArgumentInfo>,

    // Datafusion table schema
    pub schema: datafusion::SchemaRef,

    // For now, descriptions of fields
    pub columns: IndexMap<String, Option<String>>,

    // Output of this command
    pub output_type: CommandOutput,
}

// The conversion is as follows:
// 1. If the types is a list of objects, then it would be a table of those entities.
// 2. If the type is an object, it would be a table that returns a single row.
// The columns of the table are the fields of the type.
// 3. If the type is anything else, it'll be a table that returns one row
// and one column named 'result' (TODO)
//
#[allow(clippy::match_same_arms)]
fn return_type(
    metadata: &resolved::Metadata,
    output_type: &resolved::QualifiedTypeReference,
    type_mappings: Option<&BTreeMap<Qualified<CustomTypeName>, TypeMapping>>,
) -> Option<(
    // datafusion's table schema
    datafusion::SchemaRef,
    // description of the fields
    IndexMap<String, Option<String>>,
    // the expected output
    CommandOutput,
)> {
    match &output_type.underlying_type {
        resolved::QualifiedBaseType::Named(resolved::QualifiedTypeName::Inbuilt(inbuilt_type)) => {
            let data_type = match inbuilt_type {
                open_dds::types::InbuiltType::ID => datafusion::DataType::Utf8,
                open_dds::types::InbuiltType::Int => datafusion::DataType::Int32,
                open_dds::types::InbuiltType::Float => datafusion::DataType::Float32,
                open_dds::types::InbuiltType::Boolean => datafusion::DataType::Boolean,
                open_dds::types::InbuiltType::String => datafusion::DataType::Utf8,
            };
            scalar_type_to_table_schema(&data_type)
        }
        resolved::QualifiedBaseType::Named(resolved::QualifiedTypeName::Custom(custom_type)) => {
            if metadata.object_types.contains_key(custom_type) {
                let (schema, columns) =
                    super::model::object_type_table_schema(metadata, custom_type, type_mappings);
                Some((schema, columns, CommandOutput::Object(custom_type.clone())))
            } else {
                None
            }
        }
        resolved::QualifiedBaseType::List(element_type) => match &element_type.underlying_type {
            resolved::QualifiedBaseType::Named(resolved::QualifiedTypeName::Custom(
                custom_type,
            )) if metadata.object_types.contains_key(custom_type) => {
                let (schema, columns) =
                    super::model::object_type_table_schema(metadata, custom_type, type_mappings);
                Some((
                    schema,
                    columns,
                    CommandOutput::ListOfObjects(custom_type.clone()),
                ))
            }
            _ => None,
        },
    }
}

fn scalar_type_to_table_schema(
    _scalar_type: &datafusion::DataType,
) -> Option<(
    datafusion::SchemaRef,
    IndexMap<String, Option<String>>,
    CommandOutput,
)> {
    // TODO
    None
}

impl Command {
    // this returns None if any of the following is true
    // 1. if there are any argument presets defined for any role in permissions
    // 2. if a table schema cannot be generated for the command's output type
    // 3. if any of the arguments to the command cannot be converted to datafusion's types
    pub fn from_resolved_command(
        metadata: &resolved::Metadata,
        command: &resolved::CommandWithPermissions,
    ) -> Option<Self> {
        let argument_presets_defined = command
            .permissions
            .values()
            .any(|permission| !permission.argument_presets.is_empty());

        if argument_presets_defined {
            return None;
        }

        let type_mappings = command.command.source.as_ref().map(|o| &o.type_mappings);
        let (schema, columns, command_output) =
            return_type(metadata, &command.command.output_type, type_mappings)?;

        let arguments = command
            .command
            .arguments
            .iter()
            .map(|(argument_name, argument)| {
                let argument_info =
                    ArgumentInfo::from_resolved(metadata, &command.command.source, argument)?;
                Some((argument_name.clone(), argument_info))
            })
            // this returns None if any of the arguments cannot be converted
            .collect::<Option<IndexMap<_, _>>>()?;

        let command = Command {
            subgraph: command.command.name.subgraph.clone(),
            name: command.command.name.name.clone(),
            description: command.command.description.clone(),

            arguments,

            schema,
            output_type: command_output,
            columns,
        };
        Some(command)
    }
}

const STRUCT_CALLING_CONVENTION: bool = true;

impl datafusion::TableFunctionImpl for WithSession<Command> {
    fn call(
        &self,
        exprs: &[datafusion::Expr],
    ) -> datafusion::Result<Arc<dyn datafusion::TableProvider>> {
        let arguments = if STRUCT_CALLING_CONVENTION {
            call_struct_convention(&self.value.arguments, exprs)?
        } else {
            call_individual_args_convention(&self.value.arguments, exprs)?
        };

        Ok(Arc::new(CommandInvocation::new(
            self.value.clone(),
            arguments,
        )))
    }
}

fn call_struct_convention(
    argument_infos: &IndexMap<ArgumentName, ArgumentInfo>,
    exprs: &[datafusion::Expr],
) -> datafusion::Result<BTreeMap<ArgumentName, serde_json::Value>> {
    match (argument_infos.len(), exprs.len()) {
        (0, 0) => Ok(BTreeMap::new()), // No arguments expected or provided
        (_, 0) => Err(datafusion::DataFusionError::Plan(
            "Expected arguments, but none were provided".to_string(),
        )),
        (0, _) => Err(datafusion::DataFusionError::Plan(
            "No arguments expected, but some were provided".to_string(),
        )),
        (_, 1) => {
            let execution_props = ::datafusion::execution::context::ExecutionProps::new();
            let info = ::datafusion::logical_expr::simplify::SimplifyContext::new(&execution_props);
            let expr = ::datafusion::optimizer::simplify_expressions::ExprSimplifier::new(info)
                .simplify(exprs[0].clone())?;

            let struct_schema = build_struct_schema(argument_infos);
            let struct_array = extract_struct_array(&expr)?;

            let mut processed_arguments =
                parse_struct_literal(struct_schema.fields(), &struct_array)?;

            Ok(argument_infos
                .iter()
                .filter_map(|(arg_name, _)| {
                    processed_arguments
                        .remove(arg_name.as_str())
                        .map(|value| (arg_name.clone(), value))
                })
                .collect())
        }
        _ => Err(datafusion::DataFusionError::Plan(format!(
            "Expected 1 struct argument, but got {}",
            exprs.len()
        ))),
    }
}

fn call_individual_args_convention(
    argument_infos: &IndexMap<ArgumentName, ArgumentInfo>,
    exprs: &[datafusion::Expr],
) -> datafusion::Result<BTreeMap<ArgumentName, serde_json::Value>> {
    if exprs.len() != argument_infos.len() {
        return Err(datafusion::DataFusionError::Plan(format!(
            "Expected {} arguments, but got {}",
            argument_infos.len(),
            exprs.len()
        )));
    }

    let mut arguments = BTreeMap::new();

    for (expr, (arg_name, arg_info)) in exprs.iter().zip(argument_infos.iter()) {
        let execution_props = ::datafusion::execution::context::ExecutionProps::new();
        let info = ::datafusion::logical_expr::simplify::SimplifyContext::new(&execution_props);
        let expr = ::datafusion::optimizer::simplify_expressions::ExprSimplifier::new(info)
            .simplify(expr.clone())?;

        match expr {
            datafusion::Expr::Literal(lit) => {
                if lit == datafusion::ScalarValue::Null && arg_info.is_nullable {
                    // Skip null values for nullable arguments
                } else {
                    let arg_value = parse_datafusion_literal(&arg_info.argument_type, &lit)?;
                    arguments.insert(arg_name.clone(), arg_value);
                }
            }
            v => {
                return Err(datafusion::DataFusionError::Plan(format!(
                    "Only literal expressions are supported for function arguments: {v:?}"
                )))
            }
        };
    }

    Ok(arguments)
}

fn build_struct_schema(
    argument_infos: &IndexMap<ArgumentName, ArgumentInfo>,
) -> datafusion::SchemaRef {
    let mut builder = datafusion::SchemaBuilder::new();
    for (argument_name, argument) in argument_infos {
        builder.push(datafusion::Field::new(
            argument_name.as_str(),
            argument.argument_type.clone(),
            argument.is_nullable,
        ));
    }
    Arc::new(builder.finish())
}

fn extract_struct_array(
    expr: &datafusion::Expr,
) -> datafusion::Result<Arc<datafusion::StructArray>> {
    match expr {
        datafusion::Expr::Literal(datafusion::ScalarValue::Struct(struct_array)) => {
            Ok(struct_array.clone())
        }
        datafusion::Expr::ScalarFunction(scalar_function)
            if scalar_function.name() == "named_struct" =>
        {
            let args = scalar_function
                .args
                .iter()
                .map(|expr| match expr {
                    datafusion::Expr::Literal(scalar_value) => {
                        Ok(datafusion::ColumnarValue::Scalar(scalar_value.clone()))
                    }
                    datafusion::Expr::Column(column) => {
                        let name = datafusion::ScalarValue::Utf8(Some(column.name.clone()));
                        Ok(datafusion::ColumnarValue::Scalar(name))
                    }
                    _ => Err(datafusion::DataFusionError::Plan(format!(
                        "expected a name or a scalar value, but found {expr}",
                    ))),
                })
                .collect::<datafusion::Result<Vec<datafusion::ColumnarValue>>>()?;

            match scalar_function.func.invoke(&args)? {
                datafusion::ColumnarValue::Scalar(datafusion::ScalarValue::Struct(
                    struct_array,
                )) => Ok(struct_array),
                datafusion::ColumnarValue::Array(array) => {
                    if let Some(struct_array) =
                        array.as_any().downcast_ref::<datafusion::StructArray>()
                    {
                        Ok(Arc::new(struct_array.clone()))
                    } else {
                        Err(datafusion::DataFusionError::Plan(format!(
                            "unexpected return value of struct function: {array:?}",
                        )))
                    }
                }
                datafusion::ColumnarValue::Scalar(s) => Err(datafusion::DataFusionError::Plan(
                    format!("unexpected return value of struct function: {s}",),
                )),
            }
        }
        v => Err(datafusion::DataFusionError::Plan(format!(
            "Expecting a struct literal for function arguments: but found {v:?}"
        ))),
    }
}

/// This is an instatation of a command with concrete arguments. An instantiated command
/// is a source of rows and can implement 'TableProvider'
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct CommandInvocation {
    /// Metadata about the command
    pub(crate) command: Arc<Command>,
    /// This will be empty if the command doesn't take any arguments
    pub(crate) arguments: BTreeMap<ArgumentName, serde_json::Value>,
}

impl CommandInvocation {
    pub(crate) fn new(
        command: Arc<Command>,
        arguments: BTreeMap<ArgumentName, serde_json::Value>,
    ) -> Self {
        CommandInvocation { command, arguments }
    }
    pub(crate) fn to_logical_plan(
        &self,
        projected_schema: datafusion::DFSchemaRef,
    ) -> datafusion::Result<datafusion::LogicalPlan> {
        let model_query_node = CommandQuery::new(&self.command, &self.arguments, projected_schema)?;
        let logical_plan = datafusion::LogicalPlan::Extension(datafusion::Extension {
            node: Arc::new(model_query_node),
        });
        Ok(logical_plan)
    }
}

#[async_trait]
impl datafusion::TableProvider for CommandInvocation {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn schema(&self) -> datafusion::SchemaRef {
        self.command.schema.clone()
    }

    fn table_type(&self) -> datafusion::TableType {
        datafusion::TableType::View
    }

    async fn scan(
        &self,
        state: &dyn datafusion::Session,
        projection: Option<&Vec<usize>>,
        // filters and limit can be used here to inject some push-down operations if needed
        _filters: &[datafusion::Expr],
        _limit: Option<usize>,
    ) -> datafusion::Result<Arc<dyn datafusion::ExecutionPlan>> {
        let projected_schema = self.command.schema.project(projection.unwrap_or(&vec![]))?;
        let qualified_projected_schema = datafusion::DFSchema::from_unqualified_fields(
            projected_schema.fields,
            projected_schema.metadata,
        )?;
        let logical_plan = self.to_logical_plan(Arc::new(qualified_projected_schema))?;
        state.create_physical_plan(&logical_plan).await
    }
}
