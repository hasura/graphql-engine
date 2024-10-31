//! Expsoses OpenDD commands as table valued functions

use async_trait::async_trait;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::{any::Any, sync::Arc};
use thiserror::Error;

use metadata_resolve as resolved;
use open_dds::arguments::ArgumentName;
use open_dds::commands::CommandName;
use open_dds::identifier::SubgraphName;

use super::model::WithSession;
use super::types::{
    NormalizedType, StructType, StructTypeName, TypeRegistry, UnsupportedObject, UnsupportedType,
};
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

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ArgumentInfo {
    pub argument_type: datafusion::DataType,
    pub argument_type_normalized: NormalizedType,
    pub is_nullable: bool,
    pub description: Option<String>,
}

impl ArgumentInfo {
    fn from_resolved(
        type_registry: &TypeRegistry,
        argument: &resolved::ArgumentInfo,
    ) -> Result<Self, UnsupportedType> {
        // TODO, ArgumentInfo doesn't have the underlying scalar representation
        let (argument_type_normalized, argument_type) =
            type_registry.get_datafusion_type(&argument.argument_type.underlying_type)?;
        let argument_info = ArgumentInfo {
            argument_type,
            argument_type_normalized,
            is_nullable: argument.argument_type.nullable,
            description: argument.description.clone(),
        };
        Ok(argument_info)
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct Command {
    pub subgraph: SubgraphName,
    pub name: CommandName,

    pub description: Option<String>,

    pub arguments: IndexMap<ArgumentName, ArgumentInfo>,

    pub struct_type: StructTypeName,

    // Datafusion table schema
    pub schema: datafusion::SchemaRef,

    // Output of this command
    pub output_type: CommandOutput,
}

// These return types are not supported
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Error)]
pub enum UnsupportedReturnType {
    #[error("scalar return types are not supported")]
    Scalar,
    #[error("object type not supported: {0}")]
    ObjectNotSupported(UnsupportedObject),
    #[error("sist of scalar types as return type is not supported")]
    ListOfScalars,
    #[error("nested lists are not supported as return type")]
    ListOfLists,
}

// The conversion is as follows:
// 1. If the types is a list of objects, then it would be a table of those entities.
// 2. If the type is an object, it would be a table that returns a single row.
// The columns of the table are the fields of the type.
// 3. If the type is anything else, it'll be a table that returns one row
// and one column named 'result' (TODO)
//
#[allow(clippy::match_same_arms)]
fn return_type<'r>(
    registry: &'r TypeRegistry,
    output_type: &resolved::QualifiedTypeReference,
) -> Result<
    (
        // datafusion's table schema
        &'r StructType,
        // the expected output
        CommandOutput,
    ),
    UnsupportedReturnType,
> {
    match &output_type.underlying_type {
        resolved::QualifiedBaseType::Named(resolved::QualifiedTypeName::Inbuilt(_)) => {
            Err(UnsupportedReturnType::Scalar)
            // scalar_type_to_table_schema(&data_type)
        }
        resolved::QualifiedBaseType::Named(resolved::QualifiedTypeName::Custom(custom_type)) => {
            let object = registry
                .get_object(custom_type)
                .ok_or(UnsupportedReturnType::Scalar)?
                .as_ref()
                .map_err(|unsupported_object| {
                    UnsupportedReturnType::ObjectNotSupported(unsupported_object.clone())
                })?;

            Ok((object, CommandOutput::Object(custom_type.clone())))
        }
        resolved::QualifiedBaseType::List(element_type) => match &element_type.underlying_type {
            resolved::QualifiedBaseType::Named(resolved::QualifiedTypeName::Custom(
                custom_type,
            )) => {
                let object = registry
                    .get_object(custom_type)
                    .ok_or(UnsupportedReturnType::ListOfScalars)?
                    .as_ref()
                    .map_err(|unsupported_object| {
                        UnsupportedReturnType::ObjectNotSupported(unsupported_object.clone())
                    })?;

                Ok((object, CommandOutput::ListOfObjects(custom_type.clone())))
            }
            _ => Err(UnsupportedReturnType::ListOfLists),
        },
    }
}

// fn scalar_type_to_table_schema(
//     _scalar_type: &datafusion::DataType,
// ) -> Option<(
//     datafusion::SchemaRef,
//     IndexMap<String, Option<String>>,
//     CommandOutput,
// )> {
//     // TODO
//     None
// }
//
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Error)]
pub enum UnsupportedCommand {
    #[error("Command has argument presets defined")]
    ArgumentPresetsDefined,
    #[error("Return type not supported: {type_} (reason: {reason})")]
    ReturnTypeNotSupported {
        type_: resolved::QualifiedTypeReference,
        reason: UnsupportedReturnType,
    },
    #[error("Argument not supported: {argument} (reason: {reason:?})")]
    ArgumentNotSupported {
        argument: ArgumentName,
        reason: UnsupportedType,
    },
}

impl Command {
    // this returns None if any of the following is true
    // 1. if there are any argument presets defined for any role in permissions
    // 2. if a table schema cannot be generated for the command's output type
    // 3. if any of the arguments to the command cannot be converted to datafusion's types
    pub fn from_resolved_command(
        type_registry: &TypeRegistry,
        command: &resolved::CommandWithArgumentPresets,
    ) -> Result<Self, UnsupportedCommand> {
        let argument_presets_defined = command
            .permissions
            .values()
            .any(|permission| !permission.argument_presets.is_empty());

        if argument_presets_defined {
            return Err(UnsupportedCommand::ArgumentPresetsDefined);
        }

        let (struct_type, command_output) =
            return_type(type_registry, &command.command.output_type).map_err(|reason| {
                UnsupportedCommand::ReturnTypeNotSupported {
                    type_: command.command.output_type.clone(),
                    reason,
                }
            })?;

        let arguments =
            command
                .command
                .arguments
                .iter()
                .map(|(argument_name, argument)| {
                    let argument_info = ArgumentInfo::from_resolved(type_registry, argument)
                        .map_err(|reason| UnsupportedCommand::ArgumentNotSupported {
                            argument: argument_name.clone(),
                            reason,
                        })?;
                    Ok((argument_name.clone(), argument_info))
                })
                // this returns None if any of the arguments cannot be converted
                .collect::<Result<IndexMap<_, _>, _>>()?;

        let command = Command {
            subgraph: command.command.name.subgraph.clone(),
            name: command.command.name.name.clone(),
            description: command.command.description.clone(),
            arguments,
            struct_type: struct_type.name().clone(),
            schema: struct_type.schema.clone(),
            output_type: command_output,
        };
        Ok(command)
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
        (_, 0) => {
            // if all the arguments are nullable
            if argument_infos.values().all(|argument| argument.is_nullable) {
                Ok(BTreeMap::new())
            } else {
                Err(datafusion::DataFusionError::Plan(
                    "Expected arguments, but none were provided".to_string(),
                ))
            }
        }
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

#[cfg(test)]
mod tests {
    use crate::catalog::types::NormalizedTypeNamed;

    use self::datafusion::*;
    use super::*;
    use ::datafusion::arrow::array::Array;
    use ::datafusion::{
        arrow::{
            array::{ArrayRef, Int32Array, RecordBatch, StringArray},
            datatypes::Schema,
        },
        physical_plan::memory::MemoryExec,
        prelude::*,
    };
    use open_dds::identifier;
    use std::sync::Arc;

    #[derive(Clone, Debug)]
    struct TestCommand {
        name: String,
        arguments: IndexMap<ArgumentName, ArgumentInfo>,
    }
    impl TableFunctionImpl for TestCommand {
        fn call(&self, args: &[Expr]) -> Result<Arc<dyn TableProvider>> {
            let arg_values = call_struct_convention(&self.arguments, args)?;

            let schema = Arc::new(Schema::new(
                self.arguments
                    .iter()
                    .map(|(name, info)| {
                        Field::new(
                            name.to_string(),
                            info.argument_type.clone(),
                            info.is_nullable,
                        )
                    })
                    .collect::<Vec<_>>(),
            ));

            let null_value = serde_json::Value::Null;

            let columns: Vec<ArrayRef> = self
                .arguments
                .iter()
                .map(|(name, info)| {
                    let value = arg_values.get(name).unwrap_or(&null_value);
                    match &info.argument_type {
                        DataType::Int32 => {
                            let arr = match value {
                                serde_json::Value::Number(n) => {
                                    vec![Some(i32::try_from(n.as_i64().unwrap()).unwrap())]
                                }
                                serde_json::Value::Null => {
                                    vec![None]
                                }
                                _ => vec![Some(0)],
                            };
                            Arc::new(Int32Array::from(arr)) as ArrayRef
                        }
                        DataType::Utf8 => {
                            let arr = match value {
                                serde_json::Value::String(s) => vec![Some(s.as_str())],

                                serde_json::Value::Null => {
                                    vec![None]
                                }
                                _ => vec![Some("")],
                            };
                            Arc::new(StringArray::from(arr)) as ArrayRef
                        }
                        _ => unimplemented!("Unsupported data type: {:?}", info.argument_type),
                    }
                })
                .collect();

            let batch = if columns.is_empty() {
                RecordBatch::new_empty(schema.clone())
            } else {
                RecordBatch::try_new(schema.clone(), columns)?
            };

            let exec = MemoryExec::try_new(&[vec![batch]], schema.clone(), None)?;

            Ok(Arc::new(TestCommandTableProvider {
                schema,
                exec: Arc::new(exec),
            }))
        }
    }

    struct TestCommandTableProvider {
        schema: Arc<Schema>,
        exec: Arc<MemoryExec>,
    }

    #[async_trait::async_trait]
    impl TableProvider for TestCommandTableProvider {
        fn as_any(&self) -> &dyn std::any::Any {
            self
        }

        fn schema(&self) -> SchemaRef {
            self.schema.clone()
        }

        fn table_type(&self) -> TableType {
            TableType::Base
        }

        async fn scan(
            &self,
            _state: &dyn Session,
            _projection: Option<&Vec<usize>>,
            _filters: &[Expr],
            _limit: Option<usize>,
        ) -> Result<Arc<dyn ExecutionPlan>> {
            Ok(self.exec.clone())
        }
    }

    fn normalized_type(ty: DataType) -> NormalizedType {
        NormalizedType::Named(NormalizedTypeNamed::Scalar(ty))
    }

    async fn setup_and_run_query(command: TestCommand, query: &str) -> Result<Vec<RecordBatch>> {
        let name = command.name.clone();
        let ctx = SessionContext::new();
        ctx.register_udtf(&name, Arc::new(command));
        let df = ctx.sql(query).await?;
        df.collect().await
    }

    #[tokio::test]
    async fn test_non_nullable_args() -> Result<()> {
        let command = TestCommand {
            name: "test_non_nullable".to_string(),
            arguments: IndexMap::from([
                (
                    ArgumentName::from(identifier!("arg1")),
                    ArgumentInfo {
                        argument_type: DataType::Int32,
                        is_nullable: false,
                        description: None,
                        argument_type_normalized: normalized_type(DataType::Int32),
                    },
                ),
                (
                    ArgumentName::from(identifier!("arg2")),
                    ArgumentInfo {
                        argument_type: DataType::Utf8,
                        is_nullable: false,
                        description: None,
                        argument_type_normalized: normalized_type(DataType::Utf8),
                    },
                ),
            ]),
        };

        let results = setup_and_run_query(
            command,
            "SELECT * FROM test_non_nullable(STRUCT(42 as arg1, 'hello' as arg2))",
        )
        .await?;

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].num_columns(), 2);
        assert_eq!(
            results[0]
                .column(0)
                .as_any()
                .downcast_ref::<Int32Array>()
                .unwrap()
                .value(0),
            42
        );
        assert_eq!(
            results[0]
                .column(1)
                .as_any()
                .downcast_ref::<StringArray>()
                .unwrap()
                .value(0),
            "hello"
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_nullable_args() -> Result<()> {
        let command = TestCommand {
            name: "test_nullable".to_string(),
            arguments: IndexMap::from([
                (
                    ArgumentName::from(identifier!("arg1")),
                    ArgumentInfo {
                        argument_type: DataType::Int32,
                        is_nullable: true,
                        description: None,
                        argument_type_normalized: normalized_type(DataType::Int32),
                    },
                ),
                (
                    ArgumentName::from(identifier!("arg2")),
                    ArgumentInfo {
                        argument_type: DataType::Utf8,
                        is_nullable: true,
                        description: None,
                        argument_type_normalized: normalized_type(DataType::Utf8),
                    },
                ),
            ]),
        };

        let results = setup_and_run_query(
            command,
            "SELECT * FROM test_nullable(STRUCT(NULL as arg1, NULL as arg2))",
        )
        .await?;

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].num_columns(), 2);
        assert!(results[0]
            .column(0)
            .as_any()
            .downcast_ref::<Int32Array>()
            .unwrap()
            .is_null(0));
        assert!(results[0]
            .column(1)
            .as_any()
            .downcast_ref::<StringArray>()
            .unwrap()
            .is_null(0));

        Ok(())
    }

    #[tokio::test]
    async fn test_mixed_nullability() -> Result<()> {
        let command = TestCommand {
            name: "test_mixed".to_string(),
            arguments: IndexMap::from([
                (
                    ArgumentName::from(identifier!("arg1")),
                    ArgumentInfo {
                        argument_type: DataType::Int32,
                        is_nullable: false,
                        description: None,
                        argument_type_normalized: normalized_type(DataType::Int32),
                    },
                ),
                (
                    ArgumentName::from(identifier!("arg2")),
                    ArgumentInfo {
                        argument_type: DataType::Utf8,
                        is_nullable: true,
                        description: None,
                        argument_type_normalized: normalized_type(DataType::Utf8),
                    },
                ),
            ]),
        };

        let results = setup_and_run_query(
            command,
            "SELECT * FROM test_mixed(STRUCT(42 as arg1, NULL as arg2))",
        )
        .await?;

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].num_columns(), 2);
        assert_eq!(
            results[0]
                .column(0)
                .as_any()
                .downcast_ref::<Int32Array>()
                .unwrap()
                .value(0),
            42
        );
        assert!(results[0]
            .column(1)
            .as_any()
            .downcast_ref::<StringArray>()
            .unwrap()
            .is_null(0));

        Ok(())
    }

    #[tokio::test]
    async fn test_all_nullable_no_args() -> Result<()> {
        let command = TestCommand {
            name: "test_all_nullable".to_string(),
            arguments: IndexMap::from([
                (
                    ArgumentName::from(identifier!("arg1")),
                    ArgumentInfo {
                        argument_type: DataType::Int32,
                        is_nullable: true,
                        description: None,
                        argument_type_normalized: normalized_type(DataType::Int32),
                    },
                ),
                (
                    ArgumentName::from(identifier!("arg2")),
                    ArgumentInfo {
                        argument_type: DataType::Utf8,
                        is_nullable: true,
                        description: None,
                        argument_type_normalized: normalized_type(DataType::Utf8),
                    },
                ),
            ]),
        };

        let results = setup_and_run_query(command, "SELECT * FROM test_all_nullable()").await?;

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].num_columns(), 2);
        assert!(results[0]
            .column(0)
            .as_any()
            .downcast_ref::<Int32Array>()
            .unwrap()
            .is_null(0));
        assert!(results[0]
            .column(1)
            .as_any()
            .downcast_ref::<StringArray>()
            .unwrap()
            .is_null(0));

        Ok(())
    }

    #[tokio::test]
    async fn test_no_args() -> Result<()> {
        let command = TestCommand {
            name: "test_no_args".to_string(),
            arguments: IndexMap::new(),
        };

        let results = setup_and_run_query(command, "SELECT * FROM test_no_args()").await?;

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].num_columns(), 0);

        Ok(())
    }
}
