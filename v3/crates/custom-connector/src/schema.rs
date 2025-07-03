use core::option::Option::None;

use ndc_models;

use crate::{collections, functions, procedures, state::AppState, types};

pub fn get_schema() -> ndc_models::SchemaResponse {
    ndc_models::SchemaResponse {
        scalar_types: types::scalar_types(),
        object_types: types::object_types(),
        collections: collections::get_collections(),
        functions: functions::get_functions(),
        procedures: procedures::get_procedures(),
        capabilities: Some(ndc_models::CapabilitySchemaInfo {
            query: Some(ndc_models::QueryCapabilitiesSchemaInfo {
                aggregates: Some(ndc_models::AggregateCapabilitiesSchemaInfo {
                    count_scalar_type: ndc_models::ScalarTypeName::from("Int"),
                }),
            }),
        }),
        request_arguments: None,
    }
}

pub fn get_capabilities(state: &AppState) -> ndc_models::CapabilitiesResponse {
    ndc_models::CapabilitiesResponse {
        version: ndc_models::VERSION.to_owned(),
        capabilities: ndc_models::Capabilities {
            mutation: ndc_models::MutationCapabilities {
                transactional: None,
                explain: None,
            },
            query: ndc_models::QueryCapabilities {
                explain: None,
                aggregates: Some(ndc_models::AggregateCapabilities {
                    filter_by: None,
                    group_by: Some(ndc_models::GroupByCapabilities {
                        filter: Some(ndc_models::LeafCapability {}),
                        order: Some(ndc_models::LeafCapability {}),
                        paginate: Some(ndc_models::LeafCapability {}),
                    }),
                }),
                variables: Some(ndc_models::LeafCapability {}),
                nested_fields: ndc_models::NestedFieldCapabilities {
                    aggregates: Some(ndc_models::LeafCapability {}),
                    filter_by: Some(ndc_models::NestedFieldFilterByCapabilities {
                        nested_arrays: None,
                    }),
                    order_by: Some(ndc_models::LeafCapability {}),
                    nested_collections: None,
                },
                exists: ndc_models::ExistsCapabilities {
                    named_scopes: None,
                    unrelated: Some(ndc_models::LeafCapability {}),
                    nested_collections: Some(ndc_models::LeafCapability {}),
                    nested_scalar_collections: Some(ndc_models::LeafCapability {}),
                },
            },
            relationships: if state.enable_relationship_support {
                Some(ndc_models::RelationshipCapabilities {
                    relation_comparisons: Some(ndc_models::LeafCapability {}),
                    order_by_aggregate: Some(ndc_models::LeafCapability {}),
                    nested: Some(ndc_models::NestedRelationshipCapabilities {
                        array: Some(ndc_models::LeafCapability {}),
                        filtering: Some(ndc_models::LeafCapability {}),
                        ordering: Some(ndc_models::LeafCapability {}),
                    }),
                })
            } else {
                None
            },
            relational_query: Some(ndc_models::RelationalQueryCapabilities {
                project: ndc_models::RelationalProjectionCapabilities {
                    expression: expression_capabilities(),
                },
                filter: Some(expression_capabilities()),
                sort: Some(ndc_models::RelationalSortCapabilities {
                    expression: expression_capabilities(),
                }),
                join: Some(ndc_models::RelationalJoinCapabilities {
                    expression: expression_capabilities(),
                    join_types: ndc_models::RelationalJoinTypeCapabilities {
                        left: Some(ndc_models::LeafCapability {}),
                        right: Some(ndc_models::LeafCapability {}),
                        inner: Some(ndc_models::LeafCapability {}),
                        full: Some(ndc_models::LeafCapability {}),
                        left_semi: Some(ndc_models::LeafCapability {}),
                        left_anti: Some(ndc_models::LeafCapability {}),
                        right_semi: Some(ndc_models::LeafCapability {}),
                        right_anti: Some(ndc_models::LeafCapability {}),
                    },
                }),
                aggregate: Some(ndc_models::RelationalAggregateCapabilities {
                    expression: expression_capabilities(),
                    group_by: Some(ndc_models::LeafCapability {}),
                }),
                window: Some(ndc_models::RelationalWindowCapabilities {
                    expression: expression_capabilities(),
                }),
                union: Some(ndc_models::LeafCapability {}),
            }),
            relational_mutation: Some(ndc_models::RelationalMutationCapabilities {
                insert: Some(ndc_models::LeafCapability {}),
                update: Some(ndc_models::LeafCapability {}),
                delete: Some(ndc_models::LeafCapability {}),
            }),
        },
    }
}

fn expression_capabilities() -> ndc_models::RelationalExpressionCapabilities {
    ndc_models::RelationalExpressionCapabilities {
        conditional: ndc_models::RelationalConditionalExpressionCapabilities {
            case: Some(ndc_models::LeafCapability {}),
            nullif: Some(ndc_models::LeafCapability {}),
        },
        comparison: ndc_models::RelationalComparisonExpressionCapabilities {
            like: Some(ndc_models::LeafCapability {}),
            ilike: Some(ndc_models::LeafCapability {}),
            between: Some(ndc_models::LeafCapability {}),
            contains: Some(ndc_models::LeafCapability {}),
            in_list: Some(ndc_models::LeafCapability {}),
            is_nan: Some(ndc_models::LeafCapability {}),
            is_zero: Some(ndc_models::LeafCapability {}),
            greater_than_eq: Some(ndc_models::LeafCapability {}),
            greater_than: Some(ndc_models::LeafCapability {}),
            is_false: Some(ndc_models::LeafCapability {}),
            is_null: Some(ndc_models::LeafCapability {}),
            is_true: Some(ndc_models::LeafCapability {}),
            less_than_eq: Some(ndc_models::LeafCapability {}),
            less_than: Some(ndc_models::LeafCapability {}),
        },
        scalar: ndc_models::RelationalScalarExpressionCapabilities {
            abs: Some(ndc_models::LeafCapability {}),
            array_element: Some(ndc_models::LeafCapability {}),
            binary_concat: Some(ndc_models::LeafCapability {}),
            btrim: Some(ndc_models::LeafCapability {}),
            ceil: Some(ndc_models::LeafCapability {}),
            character_length: Some(ndc_models::LeafCapability {}),
            coalesce: Some(ndc_models::LeafCapability {}),
            concat: Some(ndc_models::LeafCapability {}),
            cos: Some(ndc_models::LeafCapability {}),
            current_date: Some(ndc_models::LeafCapability {}),
            current_time: Some(ndc_models::LeafCapability {}),
            current_timestamp: Some(ndc_models::LeafCapability {}),
            date_part: Some(ndc_models::DatePartScalarExpressionCapability {
                year: Some(ndc_models::LeafCapability {}),
                quarter: Some(ndc_models::LeafCapability {}),
                month: Some(ndc_models::LeafCapability {}),
                week: Some(ndc_models::LeafCapability {}),
                day_of_week: Some(ndc_models::LeafCapability {}),
                day_of_year: Some(ndc_models::LeafCapability {}),
                day: Some(ndc_models::LeafCapability {}),
                hour: Some(ndc_models::LeafCapability {}),
                minute: Some(ndc_models::LeafCapability {}),
                second: Some(ndc_models::LeafCapability {}),
                microsecond: Some(ndc_models::LeafCapability {}),
                millisecond: Some(ndc_models::LeafCapability {}),
                nanosecond: Some(ndc_models::LeafCapability {}),
                epoch: Some(ndc_models::LeafCapability {}),
            }),
            date_trunc: Some(ndc_models::LeafCapability {}),
            exp: Some(ndc_models::LeafCapability {}),
            floor: Some(ndc_models::LeafCapability {}),
            get_field: Some(ndc_models::LeafCapability {}),
            greatest: Some(ndc_models::LeafCapability {}),
            least: Some(ndc_models::LeafCapability {}),
            left: Some(ndc_models::LeafCapability {}),
            ln: Some(ndc_models::LeafCapability {}),
            log: Some(ndc_models::LeafCapability {}),
            log10: Some(ndc_models::LeafCapability {}),
            log2: Some(ndc_models::LeafCapability {}),
            lpad: Some(ndc_models::LeafCapability {}),
            ltrim: Some(ndc_models::LeafCapability {}),
            nvl: Some(ndc_models::LeafCapability {}),
            power: Some(ndc_models::LeafCapability {}),
            random: Some(ndc_models::LeafCapability {}),
            replace: Some(ndc_models::LeafCapability {}),
            reverse: Some(ndc_models::LeafCapability {}),
            right: Some(ndc_models::LeafCapability {}),
            round: Some(ndc_models::LeafCapability {}),
            rpad: Some(ndc_models::LeafCapability {}),
            rtrim: Some(ndc_models::LeafCapability {}),
            sqrt: Some(ndc_models::LeafCapability {}),
            str_pos: Some(ndc_models::LeafCapability {}),
            substr: Some(ndc_models::LeafCapability {}),
            substr_index: Some(ndc_models::LeafCapability {}),
            tan: Some(ndc_models::LeafCapability {}),
            to_date: Some(ndc_models::LeafCapability {}),
            to_timestamp: Some(ndc_models::LeafCapability {}),
            trunc: Some(ndc_models::LeafCapability {}),
            to_lower: Some(ndc_models::LeafCapability {}),
            to_upper: Some(ndc_models::LeafCapability {}),
            and: Some(ndc_models::LeafCapability {}),
            divide: Some(ndc_models::LeafCapability {}),
            minus: Some(ndc_models::LeafCapability {}),
            modulo: Some(ndc_models::LeafCapability {}),
            multiply: Some(ndc_models::LeafCapability {}),
            negate: Some(ndc_models::LeafCapability {}),
            not: Some(ndc_models::LeafCapability {}),
            or: Some(ndc_models::LeafCapability {}),
            plus: Some(ndc_models::LeafCapability {}),
        },
        aggregate: ndc_models::RelationalAggregateExpressionCapabilities {
            avg: Some(ndc_models::LeafCapability {}),
            bool_and: None,
            bool_or: None,
            count: Some(ndc_models::RelationalAggregateFunctionCapabilities {
                distinct: Some(ndc_models::LeafCapability {}),
            }),
            first_value: None,
            last_value: None,
            max: Some(ndc_models::LeafCapability {}),
            median: None,
            min: Some(ndc_models::LeafCapability {}),
            string_agg: None,
            sum: Some(ndc_models::LeafCapability {}),
            var: None,
            stddev: Some(ndc_models::LeafCapability {}),
            stddev_pop: Some(ndc_models::LeafCapability {}),
            approx_percentile_cont: Some(ndc_models::LeafCapability {}),
            approx_distinct: Some(ndc_models::LeafCapability {}),
            array_agg: Some(ndc_models::LeafCapability {}),
        },
        window: ndc_models::RelationalWindowExpressionCapabilities {
            row_number: Some(ndc_models::LeafCapability {}),
            dense_rank: None,
            ntile: Some(ndc_models::LeafCapability {}),
            rank: None,
            cume_dist: None,
            percent_rank: None,
        },
    }
}
