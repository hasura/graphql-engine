pub use super::error::ModelsError;
use lang_graphql::ast::common as ast;
use open_dds::identifier::SubgraphName;

use crate::helpers::types::store_new_graphql_type;
use crate::mk_name;

use crate::stages::{order_by_expressions, type_permissions};
use crate::types::subgraph::Qualified;

use open_dds::models::ModelName;

use std::collections::{BTreeMap, BTreeSet};

use super::order_by_expressions::{
    OrderByExpressionError, OrderByExpressionGraphqlConfig, OrderByExpressionIdentifier,
    OrderByExpressions, OrderableField, OrderableRelationships, OrderableScalarField,
    ScalarOrderByExpression,
};

pub fn make_order_by_expression(
    model_v1: &open_dds::models::ModelV1,
    object_type_representation: &type_permissions::ObjectTypeWithPermissions,
    subgraph: &SubgraphName,
    graphql_types: &mut BTreeSet<ast::TypeName>,
    qualified_model_name: &Qualified<ModelName>,
    order_by_expressions: &mut OrderByExpressions,
) -> Result<Qualified<OrderByExpressionIdentifier>, ModelsError> {
    let identifier = Qualified::new(
        subgraph.clone(),
        OrderByExpressionIdentifier::FromModel(model_v1.name.clone()),
    );
    let ordered_type = Qualified::new(subgraph.clone(), model_v1.object_type.clone());

    // we have to generate a new leaf type per field for the model
    let mut new_scalar_order_by_expressions = BTreeMap::new();

    // collect fields, creating scalar order by expressions as we go
    let mut orderable_fields = BTreeMap::new();
    for orderable_field in &model_v1.orderable_fields {
        let identifier = Qualified::new(
            subgraph.clone(),
            OrderByExpressionIdentifier::FromModelField(
                qualified_model_name.name.clone(),
                orderable_field.field_name.clone(),
            ),
        );

        // does field actually exist?
        if !object_type_representation
            .object_type
            .fields
            .contains_key(&orderable_field.field_name)
        {
            return Err(ModelsError::ModelV1OrderableFieldsError {
                model_name: qualified_model_name.clone(),
                error: OrderByExpressionError::InvalidOrderByExpressionOrderableField {
                    field_name: orderable_field.field_name.clone(),
                },
            });
        }

        let scalar_order_by_expression = ScalarOrderByExpression {
            identifier,
            enable_order_by_directions: orderable_field.order_by_directions.clone(),
            description: None,
            graphql: None,
        };

        new_scalar_order_by_expressions.insert(
            scalar_order_by_expression.identifier.clone(),
            scalar_order_by_expression.clone(),
        );

        orderable_fields.insert(
            orderable_field.field_name.clone(),
            OrderableField::Scalar(OrderableScalarField {
                order_by_expression_identifier: scalar_order_by_expression.identifier,
            }),
        );
    }

    let graphql = model_v1
        .graphql
        .as_ref()
        .and_then(|g| {
            g.order_by_expression_type.as_ref().map(|type_name| {
                let expression_type_name = mk_name(type_name.as_str()).map(ast::TypeName)?;
                store_new_graphql_type(graphql_types, Some(&expression_type_name))?;
                Ok::<_, ModelsError>(OrderByExpressionGraphqlConfig {
                    expression_type_name,
                })
            })
        })
        .transpose()?;

    let object_order_by_expression = order_by_expressions::ObjectOrderByExpression {
        identifier: identifier.clone(),
        ordered_type,
        orderable_fields,
        orderable_relationships: OrderableRelationships::ModelV1AllowAll,
        graphql,
        description: Some(format!(
            "OrderByExpression for Model {qualified_model_name}"
        )),
    };

    order_by_expressions
        .objects
        .insert(identifier.clone(), object_order_by_expression);

    Ok(identifier)
}
