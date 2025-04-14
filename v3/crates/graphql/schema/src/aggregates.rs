use hasura_authn_core::Role;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use strum_macros::Display;

use lang_graphql::{
    ast::common::{self as ast, TypeContainer},
    schema as gql_schema,
};
use metadata_resolve::{
    AggregateExpression, DataConnectorAggregationFunctionInfo, ObjectTypeWithRelationships,
    Qualified, QualifiedTypeName, mk_name,
};
use open_dds::{
    aggregates::{AggregateExpressionName, AggregationFunctionName},
    types::{CustomTypeName, FieldName},
};

use crate::{
    Annotation, Error, GDS, NamespaceAnnotation, mk_deprecation_status,
    types::{TypeId, output_type},
};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Display)]
pub enum AggregateOutputAnnotation {
    AggregationFunctionField(AggregationFunctionAnnotation),
    AggregatableField {
        field_name: FieldName,
        aggregate_operand_type: QualifiedTypeName,
    },
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Display)]
pub enum AggregationFunctionAnnotation {
    Count,
    CountDistinct,
    Function {
        function_name: AggregationFunctionName,
        data_connector_functions: Vec<DataConnectorAggregationFunctionInfo>,
    },
}

pub fn get_aggregate_select_output_type(
    builder: &mut gql_schema::Builder<GDS>,
    aggregate_expression: &metadata_resolve::AggregateExpression,
) -> Result<gql_schema::RegisteredTypeName, Error> {
    Ok(builder.register_type(TypeId::AggregateSelectOutputType {
        aggregate_expression_name: aggregate_expression.name.clone(),
        graphql_type_name: aggregate_expression
            .graphql
            .as_ref()
            .map(|graphql| &graphql.select_output_type_name)
            .ok_or_else(|| Error::NoGraphQlSelectTypeNameForAggregateExpression {
                aggregate_expression: aggregate_expression.name.clone(),
            })?
            .clone(),
    }))
}

pub fn build_aggregate_select_output_type(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    aggregate_expression_name: &Qualified<AggregateExpressionName>,
    graphql_type_name: &ast::TypeName,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    let aggregate_expression = gds
        .metadata
        .aggregate_expressions
        .get(aggregate_expression_name)
        .ok_or_else(|| Error::InternalAggregateExpressionNotFound {
            aggregate_expression: aggregate_expression_name.clone(),
        })?;

    let mut aggregate_select_output_type_fields = BTreeMap::new();

    if let Some((object_type_name, object_type)) =
        get_object_type(gds, &aggregate_expression.operand.aggregated_type)
    {
        add_aggregatable_fields(
            &mut aggregate_select_output_type_fields,
            gds,
            builder,
            aggregate_expression,
            object_type_name,
            object_type,
        )?;
    }

    add_count_aggregation_fields(
        &mut aggregate_select_output_type_fields,
        gds,
        builder,
        aggregate_expression,
    )?;

    add_aggregation_functions(
        &mut aggregate_select_output_type_fields,
        gds,
        builder,
        aggregate_expression,
    )?;

    Ok(gql_schema::TypeInfo::Object(gql_schema::Object::new(
        builder,
        graphql_type_name.clone(),
        aggregate_expression.description.clone(),
        aggregate_select_output_type_fields,
        BTreeMap::new(), // Interfaces
        vec![],          // Directives
    )))
}

fn add_aggregatable_fields(
    type_fields: &mut BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>>,
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    aggregate_expression: &AggregateExpression,
    object_type_name: &Qualified<CustomTypeName>,
    object_type: &ObjectTypeWithRelationships,
) -> Result<(), Error> {
    for aggregatable_field_info in &aggregate_expression.operand.aggregatable_fields {
        let field_def = object_type
            .object_type
            .fields
            .get(&aggregatable_field_info.field_name)
            .ok_or_else(|| Error::InternalObjectTypeFieldNotFound {
                type_name: object_type_name.clone(),
                field_name: aggregatable_field_info.field_name.clone(),
            })?;

        let field_aggregate_expression = gds
            .metadata
            .aggregate_expressions
            .get(&aggregatable_field_info.aggregate_expression)
            .ok_or_else(|| Error::InternalAggregateExpressionNotFound {
                aggregate_expression: aggregatable_field_info.aggregate_expression.clone(),
            })?;

        let field_graphql_name = mk_name(aggregatable_field_info.field_name.as_str())
            .map_err(metadata_resolve::Error::from)
            .map_err(metadata_resolve::WithContext::from)?;

        let field = gql_schema::Field::<GDS>::new(
            field_graphql_name.clone(),
            aggregatable_field_info.description.clone(),
            Annotation::Output(super::OutputAnnotation::Aggregate(
                AggregateOutputAnnotation::AggregatableField {
                    field_name: aggregatable_field_info.field_name.clone(),
                    aggregate_operand_type: field_aggregate_expression
                        .operand
                        .aggregated_type
                        .clone(),
                },
            )),
            TypeContainer::named_non_null(get_aggregate_select_output_type(
                builder,
                field_aggregate_expression,
            )?),
            BTreeMap::new(),                                      // Arguments
            mk_deprecation_status(field_def.deprecated.as_ref()), // Use the field's deprecated status; if the field is deprecated the aggregation of it should be too
        );

        // Only allow access to aggregations of the field if the type permissions allow it
        let allowed_roles = object_type
            .type_output_permissions
            .iter()
            .filter(|(_role, perms)| {
                perms
                    .allowed_fields
                    .contains(&aggregatable_field_info.field_name)
            })
            .map(|(role, _perms)| (role.clone(), None))
            .collect::<HashMap<Role, Option<NamespaceAnnotation>>>();
        let namespaced_field = builder.conditional_namespaced(field, allowed_roles);

        if type_fields
            .insert(field_graphql_name.clone(), namespaced_field)
            .is_some()
        {
            return Err(Error::InternalDuplicateAggregatableField {
                aggregate_expression: aggregate_expression.name.clone(),
                field_name: field_graphql_name,
            });
        }
    }

    Ok(())
}

fn add_count_aggregation_fields(
    type_fields: &mut BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>>,
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    aggregate_expression: &AggregateExpression,
) -> Result<(), Error> {
    // Add the _count aggregation, if enabled and a graphql name has been specified
    if aggregate_expression.count.enable {
        if let Some(count_field_name) = aggregate_expression
            .graphql
            .as_ref()
            .map(|graphql| &graphql.count_field_name)
        {
            let field = gql_schema::Field::<GDS>::new(
                count_field_name.clone(),
                aggregate_expression.count.description.clone(),
                Annotation::Output(super::OutputAnnotation::Aggregate(
                    AggregateOutputAnnotation::AggregationFunctionField(
                        AggregationFunctionAnnotation::Count,
                    ),
                )),
                ast::TypeContainer {
                    base: output_type::get_base_type_container(
                        gds,
                        builder,
                        &aggregate_expression.count.result_type,
                    )?,
                    nullable: false,
                },
                BTreeMap::new(), // Arguments
                mk_deprecation_status(None),
            );

            // All roles can use the count aggregation
            let namespaced_field = builder.allow_all_namespaced(field);

            if type_fields
                .insert(count_field_name.clone(), namespaced_field)
                .is_some()
            {
                return Err(Error::AggregationFunctionFieldNameConflict {
                    aggregate_expression: aggregate_expression.name.clone(),
                    field_name: count_field_name.clone(),
                });
            }
        }
    }

    // Add the _count_distinct aggregation, if enabled and a graphql name has been specified
    if aggregate_expression.count_distinct.enable {
        if let Some(count_distinct_field_name) = aggregate_expression
            .graphql
            .as_ref()
            .map(|graphql| &graphql.count_distinct_field_name)
        {
            let field = gql_schema::Field::<GDS>::new(
                count_distinct_field_name.clone(),
                aggregate_expression.count_distinct.description.clone(),
                Annotation::Output(super::OutputAnnotation::Aggregate(
                    AggregateOutputAnnotation::AggregationFunctionField(
                        AggregationFunctionAnnotation::CountDistinct,
                    ),
                )),
                ast::TypeContainer {
                    base: output_type::get_base_type_container(
                        gds,
                        builder,
                        &aggregate_expression.count_distinct.result_type,
                    )?,
                    nullable: false,
                },
                BTreeMap::new(), // Arguments
                mk_deprecation_status(None),
            );

            // All roles can use the count distinct aggregation
            let namespaced_field = builder.allow_all_namespaced(field);

            if type_fields
                .insert(count_distinct_field_name.clone(), namespaced_field)
                .is_some()
            {
                return Err(Error::AggregationFunctionFieldNameConflict {
                    aggregate_expression: aggregate_expression.name.clone(),
                    field_name: count_distinct_field_name.clone(),
                });
            }
        }
    }

    Ok(())
}

fn add_aggregation_functions(
    type_fields: &mut BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>>,
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    aggregate_expression: &AggregateExpression,
) -> Result<(), Error> {
    for aggregatable_function_info in &aggregate_expression.operand.aggregation_functions {
        let field_graphql_name = mk_name(aggregatable_function_info.name.as_str())
            .map_err(metadata_resolve::Error::from)
            .map_err(metadata_resolve::WithContext::from)?;

        let field = gql_schema::Field::<GDS>::new(
            field_graphql_name.clone(),
            aggregatable_function_info.description.clone(),
            Annotation::Output(super::OutputAnnotation::Aggregate(
                AggregateOutputAnnotation::AggregationFunctionField(
                    AggregationFunctionAnnotation::Function {
                        function_name: aggregatable_function_info.name.clone(),
                        data_connector_functions: aggregatable_function_info
                            .data_connector_functions
                            .clone(),
                    },
                ),
            )),
            output_type::get_output_type(gds, builder, &aggregatable_function_info.return_type)?,
            BTreeMap::new(), // Arguments
            mk_deprecation_status(None),
        );

        // All roles can access all functions
        let namespaced_field = builder.allow_all_namespaced(field);

        if type_fields
            .insert(field_graphql_name.clone(), namespaced_field)
            .is_some()
        {
            return Err(Error::AggregationFunctionFieldNameConflict {
                aggregate_expression: aggregate_expression.name.clone(),
                field_name: field_graphql_name,
            });
        }
    }

    Ok(())
}

fn get_object_type<'a>(
    gds: &'a GDS,
    type_name: &'a QualifiedTypeName,
) -> Option<(
    &'a Qualified<CustomTypeName>,
    &'a ObjectTypeWithRelationships,
)> {
    match type_name {
        QualifiedTypeName::Inbuilt(_) => None,
        QualifiedTypeName::Custom(custom_type_name) => gds
            .metadata
            .object_types
            .get(custom_type_name)
            .map(|obj_type| (custom_type_name, obj_type)),
    }
}
