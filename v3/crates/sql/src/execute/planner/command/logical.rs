use core::fmt;
use datafusion::{
    common::DFSchemaRef,
    error::DataFusionError,
    logical_expr::{LogicalPlan, UserDefinedLogicalNodeCore},
};
use indexmap::IndexMap;
use std::{collections::BTreeMap, hash::Hash};

use open_dds::{
    arguments::ArgumentName,
    identifier::Identifier,
    query::{
        Alias, CommandSelection, CommandTarget, ObjectFieldSelection, ObjectFieldTarget,
        ObjectSubSelection,
    },
    types::FieldName,
};

pub(crate) use super::physical::CommandOutput;

// Acts as a logical node for a query on a command
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct CommandQuery {
    // fields selected on the command
    pub(crate) command_selection: CommandSelection,
    // schema of the output of the command selection
    pub(crate) schema: DFSchemaRef,
    // additional detail to be passed down to the physical layer
    // to process ndc response
    pub(crate) output: CommandOutput,
}

impl Hash for CommandQuery {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.command_selection.target.subgraph.hash(state);
        self.command_selection.target.command_name.hash(state);
        // Implementing a full hash function is hard because
        // you want to ignore the order of keys in hash maps.
        // So, for now, we only hash some basic information.
    }
}

impl Eq for CommandQuery {}

impl UserDefinedLogicalNodeCore for CommandQuery {
    fn name(&self) -> &str {
        "CommandQuery"
    }

    fn inputs(&self) -> Vec<&LogicalPlan> {
        vec![]
    }

    /// Schema for TopK is the same as the input
    fn schema(&self) -> &DFSchemaRef {
        &self.schema
    }

    fn expressions(&self) -> Vec<datafusion::logical_expr::Expr> {
        vec![]
    }

    /// For example: `TopK: k=10`
    fn fmt_for_explain(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let projection = if let Some(selection) = &self.command_selection.selection {
            format!(
                ", projection=[{}]",
                selection
                    .keys()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(",")
            )
        } else {
            String::new()
        };
        write!(
            f,
            "CommandQuery: command={}:{}{projection}",
            self.command_selection.target.subgraph, self.command_selection.target.command_name,
        )
    }

    fn with_exprs_and_inputs(
        &self,
        _exprs: Vec<datafusion::logical_expr::Expr>,
        _inputs: Vec<LogicalPlan>,
    ) -> datafusion::error::Result<Self> {
        Ok(self.clone())
    }
}

impl CommandQuery {
    pub(crate) fn new(
        command: &crate::catalog::command::Command,
        arguments: &BTreeMap<ArgumentName, serde_json::Value>,
        projected_schema: DFSchemaRef,
    ) -> datafusion::error::Result<Self> {
        let mut field_selection = IndexMap::new();
        for field in projected_schema.fields() {
            let field_name = {
                let field_name = Identifier::new(field.name().clone()).map_err(|e| {
                    DataFusionError::Internal(format!(
                        "field name conversion failed {}: {}",
                        field.name(),
                        e
                    ))
                })?;
                FieldName::new(field_name)
            };
            field_selection.insert(
                Alias::new(field_name.as_ref().clone()),
                ObjectSubSelection::Field(ObjectFieldSelection {
                    target: ObjectFieldTarget {
                        field_name,
                        arguments: IndexMap::new(),
                    },
                    selection: None,
                }),
            );
        }

        let command_selection = CommandSelection {
            target: CommandTarget {
                subgraph: command.subgraph.clone(),
                command_name: command.name.clone(),
                arguments: arguments
                    .iter()
                    .map(|(argument_name, value)| {
                        (
                            argument_name.clone(),
                            open_dds::query::Value::Literal(value.clone()),
                        )
                    })
                    .collect(),
            },
            selection: Some(field_selection),
        };
        let command_query_node = CommandQuery {
            output: command.output_type.clone(),
            command_selection,
            schema: projected_schema,
        };
        Ok(command_query_node)
    }
}
