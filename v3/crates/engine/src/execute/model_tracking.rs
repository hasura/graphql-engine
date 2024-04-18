//! Track the models that were used in query.

use super::ir::root_field::{self, RootField};
use crate::metadata::resolved::subgraph::Qualified;
use indexmap::IndexMap;
use lang_graphql::ast::common::Alias;
use open_dds::{commands::CommandName, models::ModelName};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UsagesCounts {
    pub models_used: Vec<ModelCount>,
    pub commands_used: Vec<CommandCount>,
}

impl Default for UsagesCounts {
    fn default() -> Self {
        Self::new()
    }
}

impl UsagesCounts {
    pub fn new() -> Self {
        UsagesCounts {
            models_used: Vec::new(),
            commands_used: Vec::new(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct CommandCount {
    pub command: Qualified<CommandName>,
    pub count: usize,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ModelCount {
    pub model: Qualified<ModelName>,
    pub count: usize,
}

// Get all the OpenDDS Models/commands that were for executing a query. We loop through
// each root field in the query and get the models/commands that were used in each root
// field. We then merge the models/commands used in each root field into a single map.
// That means, if the same model was used in multiple root fields, we will
// sum up the number of times that model was used.
pub fn get_all_usage_counts_in_query(ir: &IndexMap<Alias, RootField<'_, '_>>) -> UsagesCounts {
    let mut all_usage_counts = UsagesCounts::new();
    for ir_field in ir.values() {
        match ir_field {
            root_field::RootField::QueryRootField(ir) => match ir {
                root_field::QueryRootField::TypeName { .. } => {}
                root_field::QueryRootField::SchemaField { .. } => {}
                root_field::QueryRootField::TypeField { .. } => {}
                root_field::QueryRootField::ApolloFederation(
                    root_field::ApolloFederationRootFields::ServiceField { .. },
                ) => {}
                root_field::QueryRootField::ModelSelectOne { ir, .. } => {
                    let usage_counts = ir.usage_counts.clone();
                    extend_usage_count(usage_counts, &mut all_usage_counts);
                }
                root_field::QueryRootField::ModelSelectMany { ir, .. } => {
                    let usage_counts = ir.usage_counts.clone();
                    extend_usage_count(usage_counts, &mut all_usage_counts);
                }
                root_field::QueryRootField::NodeSelect(ir1) => match ir1 {
                    None => {}
                    Some(ir2) => {
                        let usage_counts = ir2.usage_counts.clone();
                        extend_usage_count(usage_counts, &mut all_usage_counts);
                    }
                },
                root_field::QueryRootField::FunctionBasedCommand { ir, .. } => {
                    let usage_counts = ir.command_info.usage_counts.clone();
                    extend_usage_count(usage_counts, &mut all_usage_counts);
                }
                root_field::QueryRootField::ApolloFederation(
                    root_field::ApolloFederationRootFields::EntitiesSelect(irs),
                ) => {
                    for ir in irs {
                        let usage_counts = ir.usage_counts.clone();
                        extend_usage_count(usage_counts, &mut all_usage_counts);
                    }
                }
            },
            root_field::RootField::MutationRootField(rf) => match rf {
                root_field::MutationRootField::TypeName { .. } => {}
                root_field::MutationRootField::ProcedureBasedCommand { ir, .. } => {
                    let usage_counts = ir.command_info.usage_counts.clone();
                    extend_usage_count(usage_counts, &mut all_usage_counts);
                }
            },
        }
    }
    all_usage_counts
}

fn extend_usage_count(usage_counts: UsagesCounts, all_usage_counts: &mut UsagesCounts) {
    for model_count in usage_counts.models_used.into_iter() {
        let countable_model = &model_count.model;
        match all_usage_counts
            .models_used
            .iter_mut()
            .find(|element| element.model == *countable_model)
        {
            None => {
                all_usage_counts.models_used.push(model_count);
            }
            Some(existing_count) => {
                existing_count.count += model_count.count;
            }
        }
    }
    for command_count in usage_counts.commands_used.into_iter() {
        let countable_model = &command_count.command;
        match all_usage_counts
            .commands_used
            .iter_mut()
            .find(|element| element.command == *countable_model)
        {
            None => {
                all_usage_counts.commands_used.push(command_count);
            }
            Some(existing_count) => {
                existing_count.count += command_count.count;
            }
        }
    }
}

pub fn count_model(model: &Qualified<ModelName>, all_usage_counts: &mut UsagesCounts) {
    match all_usage_counts
        .models_used
        .iter_mut()
        .find(|element| element.model == *model)
    {
        None => {
            let model_count = ModelCount {
                model: model.clone(),
                count: 1,
            };
            all_usage_counts.models_used.push(model_count);
        }
        Some(existing_model) => {
            existing_model.count += 1;
        }
    }
}

pub fn count_command(command: &Qualified<CommandName>, all_usage_counts: &mut UsagesCounts) {
    match all_usage_counts
        .commands_used
        .iter_mut()
        .find(|element| element.command == *command)
    {
        None => {
            let command_count = CommandCount {
                command: command.clone(),
                count: 1,
            };
            all_usage_counts.commands_used.push(command_count);
        }
        Some(existing_command) => {
            existing_command.count += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use open_dds::{commands::CommandName, identifier, models::ModelName};

    use crate::{
        execute::model_tracking::{
            count_command, count_model, extend_usage_count, CommandCount, ModelCount, UsagesCounts,
        },
        metadata::resolved::subgraph::Qualified,
    };

    #[test]
    fn test_extend_usage_count() {
        let model_count1 = ModelCount {
            model: Qualified::new("subgraph".to_string(), ModelName(identifier!("model1"))),
            count: 1,
        };
        let model_count2 = ModelCount {
            model: Qualified::new("subgraph".to_string(), ModelName(identifier!("model2"))),
            count: 5,
        };
        let model_count3 = ModelCount {
            model: Qualified::new("subgraph".to_string(), ModelName(identifier!("model3"))),
            count: 2,
        };
        let command_count1 = CommandCount {
            command: Qualified::new("subgraph".to_string(), CommandName(identifier!("command1"))),
            count: 2,
        };
        let command_count2 = CommandCount {
            command: Qualified::new("subgraph".to_string(), CommandName(identifier!("command2"))),
            count: 1,
        };
        let command_count3 = CommandCount {
            command: Qualified::new("subgraph".to_string(), CommandName(identifier!("command3"))),
            count: 3,
        };
        let usage_counts = UsagesCounts {
            models_used: vec![model_count1, model_count2.clone()],
            commands_used: vec![command_count1, command_count2.clone()],
        };
        let mut aggregator = UsagesCounts {
            models_used: vec![model_count2, model_count3],
            commands_used: vec![command_count2, command_count3],
        };
        extend_usage_count(usage_counts, &mut aggregator);
        let expected = UsagesCounts {
            models_used: vec![
                ModelCount {
                    model: Qualified::new("subgraph".to_string(), ModelName(identifier!("model2"))),
                    count: 10,
                },
                ModelCount {
                    model: Qualified::new("subgraph".to_string(), ModelName(identifier!("model3"))),
                    count: 2,
                },
                ModelCount {
                    model: Qualified::new("subgraph".to_string(), ModelName(identifier!("model1"))),
                    count: 1,
                },
            ],
            commands_used: vec![
                CommandCount {
                    command: Qualified::new(
                        "subgraph".to_string(),
                        CommandName(identifier!("command2")),
                    ),
                    count: 2,
                },
                CommandCount {
                    command: Qualified::new(
                        "subgraph".to_string(),
                        CommandName(identifier!("command3")),
                    ),
                    count: 3,
                },
                CommandCount {
                    command: Qualified::new(
                        "subgraph".to_string(),
                        CommandName(identifier!("command1")),
                    ),
                    count: 2,
                },
            ],
        };
        assert_eq!(aggregator, expected);
    }

    #[test]
    fn test_counter_functions() {
        let mut aggregator = UsagesCounts::new();
        count_command(
            &Qualified::new("subgraph".to_string(), CommandName(identifier!("command1"))),
            &mut aggregator,
        );
        assert_eq!(
            aggregator,
            UsagesCounts {
                models_used: Vec::new(),
                commands_used: vec![CommandCount {
                    command: Qualified::new(
                        "subgraph".to_string(),
                        CommandName(identifier!("command1"))
                    ),
                    count: 1,
                }]
            }
        );
        count_command(
            &Qualified::new("subgraph".to_string(), CommandName(identifier!("command1"))),
            &mut aggregator,
        );
        assert_eq!(
            aggregator,
            UsagesCounts {
                models_used: Vec::new(),
                commands_used: vec![CommandCount {
                    command: Qualified::new(
                        "subgraph".to_string(),
                        CommandName(identifier!("command1"))
                    ),
                    count: 2,
                }]
            }
        );
        count_model(
            &Qualified::new("subgraph".to_string(), ModelName(identifier!("model1"))),
            &mut aggregator,
        );
        assert_eq!(
            aggregator,
            UsagesCounts {
                models_used: vec![ModelCount {
                    model: Qualified::new("subgraph".to_string(), ModelName(identifier!("model1"))),
                    count: 1,
                }],
                commands_used: vec![CommandCount {
                    command: Qualified::new(
                        "subgraph".to_string(),
                        CommandName(identifier!("command1"))
                    ),
                    count: 2,
                }]
            }
        );
        count_model(
            &Qualified::new("subgraph".to_string(), ModelName(identifier!("model1"))),
            &mut aggregator,
        );
        assert_eq!(
            aggregator,
            UsagesCounts {
                models_used: vec![ModelCount {
                    model: Qualified::new("subgraph".to_string(), ModelName(identifier!("model1"))),
                    count: 2,
                }],
                commands_used: vec![CommandCount {
                    command: Qualified::new(
                        "subgraph".to_string(),
                        CommandName(identifier!("command1"))
                    ),
                    count: 2,
                }]
            }
        );
        count_model(
            &Qualified::new("subgraph".to_string(), ModelName(identifier!("model2"))),
            &mut aggregator,
        );
        assert_eq!(
            aggregator,
            UsagesCounts {
                models_used: vec![
                    ModelCount {
                        model: Qualified::new(
                            "subgraph".to_string(),
                            ModelName(identifier!("model1"))
                        ),
                        count: 2,
                    },
                    ModelCount {
                        model: Qualified::new(
                            "subgraph".to_string(),
                            ModelName(identifier!("model2"))
                        ),
                        count: 1,
                    }
                ],
                commands_used: vec![CommandCount {
                    command: Qualified::new(
                        "subgraph".to_string(),
                        CommandName(identifier!("command1"))
                    ),
                    count: 2,
                }]
            }
        );
        count_command(
            &Qualified::new("subgraph".to_string(), CommandName(identifier!("command2"))),
            &mut aggregator,
        );
        assert_eq!(
            aggregator,
            UsagesCounts {
                models_used: vec![
                    ModelCount {
                        model: Qualified::new(
                            "subgraph".to_string(),
                            ModelName(identifier!("model1"))
                        ),
                        count: 2,
                    },
                    ModelCount {
                        model: Qualified::new(
                            "subgraph".to_string(),
                            ModelName(identifier!("model2"))
                        ),
                        count: 1,
                    }
                ],
                commands_used: vec![
                    CommandCount {
                        command: Qualified::new(
                            "subgraph".to_string(),
                            CommandName(identifier!("command1"))
                        ),
                        count: 2,
                    },
                    CommandCount {
                        command: Qualified::new(
                            "subgraph".to_string(),
                            CommandName(identifier!("command2"))
                        ),
                        count: 1,
                    }
                ]
            }
        );
    }
}
