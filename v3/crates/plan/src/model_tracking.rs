//! Track the models that were used in query.

use metadata_resolve::Qualified;
use open_dds::{commands::CommandName, models::ModelName};
use plan_types::{CommandCount, ModelCount, UsagesCounts};

pub fn extend_usage_count(usage_counts: UsagesCounts, all_usage_counts: &mut UsagesCounts) {
    for model_count in usage_counts.models_used {
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
    for command_count in usage_counts.commands_used {
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
    use crate::model_tracking::{
        CommandCount, ModelCount, UsagesCounts, count_command, count_model, extend_usage_count,
    };
    use metadata_resolve::Qualified;
    use open_dds::{commands::CommandName, identifier, models::ModelName, subgraph_identifier};

    #[test]
    fn test_extend_usage_count() {
        let model_count1 = ModelCount {
            model: Qualified::new(
                subgraph_identifier!("subgraph"),
                ModelName::new(identifier!("model1")),
            ),
            count: 1,
        };
        let model_count2 = ModelCount {
            model: Qualified::new(
                subgraph_identifier!("subgraph"),
                ModelName::new(identifier!("model2")),
            ),
            count: 5,
        };
        let model_count3 = ModelCount {
            model: Qualified::new(
                subgraph_identifier!("subgraph"),
                ModelName::new(identifier!("model3")),
            ),
            count: 2,
        };
        let command_count1 = CommandCount {
            command: Qualified::new(
                subgraph_identifier!("subgraph"),
                CommandName::new(identifier!("command1")),
            ),
            count: 2,
        };
        let command_count2 = CommandCount {
            command: Qualified::new(
                subgraph_identifier!("subgraph"),
                CommandName::new(identifier!("command2")),
            ),
            count: 1,
        };
        let command_count3 = CommandCount {
            command: Qualified::new(
                subgraph_identifier!("subgraph"),
                CommandName::new(identifier!("command3")),
            ),
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
                    model: Qualified::new(
                        subgraph_identifier!("subgraph"),
                        ModelName::new(identifier!("model2")),
                    ),
                    count: 10,
                },
                ModelCount {
                    model: Qualified::new(
                        subgraph_identifier!("subgraph"),
                        ModelName::new(identifier!("model3")),
                    ),
                    count: 2,
                },
                ModelCount {
                    model: Qualified::new(
                        subgraph_identifier!("subgraph"),
                        ModelName::new(identifier!("model1")),
                    ),
                    count: 1,
                },
            ],
            commands_used: vec![
                CommandCount {
                    command: Qualified::new(
                        subgraph_identifier!("subgraph"),
                        CommandName::new(identifier!("command2")),
                    ),
                    count: 2,
                },
                CommandCount {
                    command: Qualified::new(
                        subgraph_identifier!("subgraph"),
                        CommandName::new(identifier!("command3")),
                    ),
                    count: 3,
                },
                CommandCount {
                    command: Qualified::new(
                        subgraph_identifier!("subgraph"),
                        CommandName::new(identifier!("command1")),
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
            &Qualified::new(
                subgraph_identifier!("subgraph"),
                CommandName::new(identifier!("command1")),
            ),
            &mut aggregator,
        );
        assert_eq!(
            aggregator,
            UsagesCounts {
                models_used: Vec::new(),
                commands_used: vec![CommandCount {
                    command: Qualified::new(
                        subgraph_identifier!("subgraph"),
                        CommandName::new(identifier!("command1"))
                    ),
                    count: 1,
                }]
            }
        );
        count_command(
            &Qualified::new(
                subgraph_identifier!("subgraph"),
                CommandName::new(identifier!("command1")),
            ),
            &mut aggregator,
        );
        assert_eq!(
            aggregator,
            UsagesCounts {
                models_used: Vec::new(),
                commands_used: vec![CommandCount {
                    command: Qualified::new(
                        subgraph_identifier!("subgraph"),
                        CommandName::new(identifier!("command1"))
                    ),
                    count: 2,
                }]
            }
        );
        count_model(
            &Qualified::new(
                subgraph_identifier!("subgraph"),
                ModelName::new(identifier!("model1")),
            ),
            &mut aggregator,
        );
        assert_eq!(
            aggregator,
            UsagesCounts {
                models_used: vec![ModelCount {
                    model: Qualified::new(
                        subgraph_identifier!("subgraph"),
                        ModelName::new(identifier!("model1"))
                    ),
                    count: 1,
                }],
                commands_used: vec![CommandCount {
                    command: Qualified::new(
                        subgraph_identifier!("subgraph"),
                        CommandName::new(identifier!("command1"))
                    ),
                    count: 2,
                }]
            }
        );
        count_model(
            &Qualified::new(
                subgraph_identifier!("subgraph"),
                ModelName::new(identifier!("model1")),
            ),
            &mut aggregator,
        );
        assert_eq!(
            aggregator,
            UsagesCounts {
                models_used: vec![ModelCount {
                    model: Qualified::new(
                        subgraph_identifier!("subgraph"),
                        ModelName::new(identifier!("model1"))
                    ),
                    count: 2,
                }],
                commands_used: vec![CommandCount {
                    command: Qualified::new(
                        subgraph_identifier!("subgraph"),
                        CommandName::new(identifier!("command1"))
                    ),
                    count: 2,
                }]
            }
        );
        count_model(
            &Qualified::new(
                subgraph_identifier!("subgraph"),
                ModelName::new(identifier!("model2")),
            ),
            &mut aggregator,
        );
        assert_eq!(
            aggregator,
            UsagesCounts {
                models_used: vec![
                    ModelCount {
                        model: Qualified::new(
                            subgraph_identifier!("subgraph"),
                            ModelName::new(identifier!("model1"))
                        ),
                        count: 2,
                    },
                    ModelCount {
                        model: Qualified::new(
                            subgraph_identifier!("subgraph"),
                            ModelName::new(identifier!("model2"))
                        ),
                        count: 1,
                    }
                ],
                commands_used: vec![CommandCount {
                    command: Qualified::new(
                        subgraph_identifier!("subgraph"),
                        CommandName::new(identifier!("command1"))
                    ),
                    count: 2,
                }]
            }
        );
        count_command(
            &Qualified::new(
                subgraph_identifier!("subgraph"),
                CommandName::new(identifier!("command2")),
            ),
            &mut aggregator,
        );
        assert_eq!(
            aggregator,
            UsagesCounts {
                models_used: vec![
                    ModelCount {
                        model: Qualified::new(
                            subgraph_identifier!("subgraph"),
                            ModelName::new(identifier!("model1"))
                        ),
                        count: 2,
                    },
                    ModelCount {
                        model: Qualified::new(
                            subgraph_identifier!("subgraph"),
                            ModelName::new(identifier!("model2"))
                        ),
                        count: 1,
                    }
                ],
                commands_used: vec![
                    CommandCount {
                        command: Qualified::new(
                            subgraph_identifier!("subgraph"),
                            CommandName::new(identifier!("command1"))
                        ),
                        count: 2,
                    },
                    CommandCount {
                        command: Qualified::new(
                            subgraph_identifier!("subgraph"),
                            CommandName::new(identifier!("command2"))
                        ),
                        count: 1,
                    }
                ]
            }
        );
    }
}
