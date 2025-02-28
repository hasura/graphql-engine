//! Usage analytics, like model, command, field usage analytics, from a GraphQL query

use metadata_resolve::{Qualified, UnTaggedQualifiedTypeName};
use open_dds::{
    arguments::ArgumentName,
    commands::CommandName,
    models::ModelName,
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, FieldName},
};
use schemars::JsonSchema;
use serde::Serialize;

/// This is the data to emit (serlialized) for analytics, when a GraphQL
/// operation is executed.
#[derive(Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum GqlOperation {
    Query {
        operation_name: String,
        fields: Vec<GqlField>,
    },
    Mutation {
        operation_name: String,
        fields: Vec<GqlField>,
    },
    Subscription {
        operation_name: String,
        fields: Vec<GqlField>,
    },
}

/// A GraphQL field appearing in the query
#[derive(Serialize, JsonSchema)]
pub struct GqlField {
    /// Name of the GraphQL field
    pub name: String,
    /// Alias of this field used in the query
    pub alias: String,
    /// Arguments of this field
    pub arguments: Vec<GqlInputField>,
    /// Fields in its selection set
    pub fields: Vec<GqlField>,
    /// Which OpenDD objects it is using
    pub used: Vec<OpenddObject>,
}

#[derive(Serialize, JsonSchema)]
/// A GraphQL input field
pub struct GqlInputField {
    /// Name of the input field
    pub name: String,
    /// Fields of this input field
    pub fields: Vec<GqlInputField>,
    /// Which OpenDD objects it is using
    pub used: Vec<OpenddObject>,
}

/// All kinds of OpenDD objects that could be used in a GraphQL operation
#[derive(Serialize, JsonSchema, Clone)]
#[serde(rename_all = "snake_case")]
pub enum OpenddObject {
    Model { name: Qualified<ModelName> },
    Command { name: Qualified<CommandName> },
    Field(FieldUsage),
    Permission(PermissionUsage),
    Relationship(RelationshipUsage),
}

#[derive(Serialize, JsonSchema, Clone)]
pub struct FieldUsage {
    pub name: FieldName,
    pub opendd_type: Qualified<CustomTypeName>,
    pub deprecated: bool,
    pub deprecated_reason: Option<String>,
}

#[derive(Serialize, JsonSchema, Clone)]
#[serde(rename_all = "snake_case")]
pub enum PermissionUsage {
    FieldPresets(FieldPresetsUsage),
    FilterPredicate(FilterPredicateUsage),
    ArgumentPresets(ArgumentPresetsUsage),
}

#[derive(Serialize, JsonSchema, Clone)]
pub struct FieldPresetsUsage {
    pub fields: Vec<FieldUsage>,
}

#[derive(Serialize, JsonSchema, Clone)]
pub struct FilterPredicateUsage {
    pub fields: Vec<FieldUsage>,
    pub relationships: Vec<PredicateRelationshipUsage>,
}

#[derive(Serialize, JsonSchema, Clone)]
pub struct ArgumentPresetsUsage {
    pub arguments: Vec<ArgumentName>,
}

#[derive(Serialize, JsonSchema, Clone)]
pub struct RelationshipUsage {
    pub name: RelationshipName,
    pub source: Qualified<CustomTypeName>,
    pub target: RelationshipTarget,
    pub deprecated: bool,
    pub deprecated_reason: Option<String>,
}

#[derive(Serialize, JsonSchema, Clone)]
pub struct PredicateRelationshipUsage {
    pub name: RelationshipName,
    pub source: Qualified<CustomTypeName>,
    pub target: RelationshipTarget,
    pub predicate_usage: Box<FilterPredicateUsage>,
}

#[derive(Serialize, JsonSchema, Clone)]
#[serde(rename_all = "snake_case")]
pub enum RelationshipTarget {
    Model {
        model_name: Qualified<ModelName>,
        opendd_type: Qualified<CustomTypeName>,
        relationship_type: RelationshipType,
        mapping: Vec<RelationshipModelMapping>,
    },
    Command {
        command_name: Qualified<CommandName>,
        // We don't want to use QualifiedTypeReference here as it is serialized with lots of unnecessary information and
        // we don't need it for analytics. We are using the untaged qualified type name instead.
        opendd_type: UnTaggedQualifiedTypeName,
        mapping: Vec<RelationshipCommandMapping>,
    },
}

#[derive(Serialize, JsonSchema, Clone)]
#[serde(rename_all = "snake_case")]
pub struct RelationshipModelMapping {
    pub source_field: FieldName,
    pub target: RelationshipModelMappingTarget,
}

#[derive(Serialize, JsonSchema, Clone)]
#[serde(rename_all = "snake_case")]
pub enum RelationshipModelMappingTarget {
    Field(FieldName),
    Argument(ArgumentName),
}

#[derive(Serialize, JsonSchema, Clone)]
#[serde(rename_all = "snake_case")]
pub struct RelationshipCommandMapping {
    pub source_field: FieldName,
    pub target_argument: ArgumentName,
}

#[cfg(test)]
mod tests {
    use super::*;
    use goldenfile::Mint;
    use open_dds::relationships::RelationshipType;
    use open_dds::{identifier, subgraph_identifier};
    use schemars::schema_for;
    use std::{io::Write, path::PathBuf};

    #[test]
    fn test_json_schema() {
        let mut mint = Mint::new(PathBuf::from(env!("CARGO_MANIFEST_DIR")));
        let mut expected = mint
            .new_goldenfile("query_usage_analytics.jsonschema")
            .unwrap();
        let schema = schema_for!(super::GqlOperation);
        write!(
            expected,
            "{}",
            serde_json::to_string_pretty(&schema).unwrap()
        )
        .unwrap();
    }

    #[test]
    // just a dummy serialize test for now to visualize the output
    #[allow(clippy::print_stdout)]
    fn dummy_serialize() {
        /*
        * Consider we are serializing usage analytics for the following query
           query MyQuery {
             orders: app_orders(
               where: {id: {_eq: 5}, products: {price: {_gt: 100}}}
               order_by: {product: {price: asc}}
             ) {
               date
               address { # nested object field; not model/command
                 address_line_1
                 address_line_2
               }
               products(where: {quantity: {_gt: 2}}, order_by: {quantity: desc}) {
                 name
               }
             }
           }
        */
        let product_relationship = OpenddObject::Relationship(RelationshipUsage {
            name: RelationshipName::new(identifier!("product")),
            source: Qualified::new(
                subgraph_identifier!("app"),
                CustomTypeName(identifier!("Order")),
            ),
            target: RelationshipTarget::Model {
                model_name: Qualified::new(
                    subgraph_identifier!("app"),
                    ModelName::new(identifier!("Products")),
                ),
                relationship_type: RelationshipType::Object,
                opendd_type: Qualified::new(
                    subgraph_identifier!("app"),
                    CustomTypeName(identifier!("Products")),
                ),
                mapping: vec![RelationshipModelMapping {
                    source_field: FieldName::new(identifier!("product_id")),
                    target: RelationshipModelMappingTarget::Field(FieldName::new(identifier!(
                        "id"
                    ))),
                }],
            },
            deprecated: false,
            deprecated_reason: None,
        });

        // id: {_eq: 5}
        let product_id_filter = GqlInputField {
            name: "id".to_string(),
            used: vec![OpenddObject::Field(FieldUsage {
                name: FieldName::new(identifier!("id")),
                opendd_type: Qualified::new(
                    subgraph_identifier!("app"),
                    CustomTypeName(identifier!("Order")),
                ),
                deprecated: false,
                deprecated_reason: None,
            })],
            fields: vec![GqlInputField {
                name: "_eq".to_string(),
                used: vec![],
                fields: vec![],
            }],
        };
        // products: {price: {_gt: 100}}
        let products_price_filter = GqlInputField {
            name: "products".to_string(),
            fields: vec![GqlInputField {
                name: "price".to_string(),
                used: vec![OpenddObject::Field(FieldUsage {
                    name: FieldName::new(identifier!("price")),
                    opendd_type: Qualified::new(
                        subgraph_identifier!("app"),
                        CustomTypeName(identifier!("Product")),
                    ),
                    deprecated: false,
                    deprecated_reason: None,
                })],
                fields: vec![GqlInputField {
                    name: "_gt".to_string(),
                    used: vec![],
                    fields: vec![],
                }],
            }],
            used: vec![product_relationship.clone()],
        };
        // where: {id: {_eq: 5}, products: {price: {_gt: 100}}}
        let where_argument = GqlInputField {
            name: "where".to_string(),
            fields: vec![product_id_filter, products_price_filter],
            used: vec![],
        };

        // order_by: {product: {price: asc}}
        let order_by_argument = GqlInputField {
            name: "order_by".to_string(),
            fields: vec![GqlInputField {
                name: "product".to_string(),
                fields: vec![GqlInputField {
                    name: "price".to_string(),
                    fields: vec![],
                    used: vec![OpenddObject::Field(FieldUsage {
                        name: FieldName::new(identifier!("price")),
                        opendd_type: Qualified::new(
                            subgraph_identifier!("app"),
                            CustomTypeName(identifier!("Product")),
                        ),
                        deprecated: false,
                        deprecated_reason: None,
                    })],
                }],
                used: vec![product_relationship.clone()],
            }],
            used: vec![],
        };

        // quantity: {_gt: 2}
        let products_quantity_filter = GqlInputField {
            name: "quantity".to_string(),
            used: vec![OpenddObject::Field(FieldUsage {
                name: FieldName::new(identifier!("quantity")),
                opendd_type: Qualified::new(
                    subgraph_identifier!("app"),
                    CustomTypeName(identifier!("Product")),
                ),
                deprecated: false,
                deprecated_reason: None,
            })],
            fields: vec![GqlInputField {
                name: "_gt".to_string(),
                used: vec![],
                fields: vec![],
            }],
        };
        // where: {quantity: {_gt: 2}}
        let products_where_argument = GqlInputField {
            name: "where".to_string(),
            fields: vec![products_quantity_filter],
            used: vec![],
        };
        // order_by: {quantity: desc}
        let products_order_by_argument = GqlInputField {
            name: "order_by".to_string(),
            fields: vec![GqlInputField {
                name: "quantity".to_string(),
                fields: vec![],
                used: vec![OpenddObject::Field(FieldUsage {
                    name: FieldName::new(identifier!("quantity")),
                    opendd_type: Qualified::new(
                        subgraph_identifier!("app"),
                        CustomTypeName(identifier!("Product")),
                    ),
                    deprecated: false,
                    deprecated_reason: None,
                })],
            }],
            used: vec![],
        };

        let operation = GqlOperation::Query {
            operation_name: "MyQuery".to_string(),
            fields: vec![GqlField {
                name: "app_orders".to_string(),
                alias: "orders".to_string(),
                arguments: vec![where_argument, order_by_argument],
                used: vec![
                    OpenddObject::Model {
                        name: Qualified::new(
                            subgraph_identifier!("app"),
                            ModelName::new(identifier!("Orders")),
                        ),
                    },
                    OpenddObject::Permission(PermissionUsage::FilterPredicate(
                        FilterPredicateUsage {
                            fields: vec![FieldUsage {
                                name: FieldName::new(identifier!("id")),
                                opendd_type: Qualified::new(
                                    subgraph_identifier!("app"),
                                    CustomTypeName(identifier!("Order")),
                                ),
                                deprecated: false,
                                deprecated_reason: None,
                            }],
                            relationships: vec![],
                        },
                    )),
                ],
                fields: vec![
                    GqlField {
                        name: "date".to_string(),
                        alias: "date".to_string(),
                        used: vec![OpenddObject::Field(FieldUsage {
                            name: FieldName::new(identifier!("date")),
                            opendd_type: Qualified::new(
                                subgraph_identifier!("app"),
                                CustomTypeName(identifier!("Order")),
                            ),
                            deprecated: false,
                            deprecated_reason: None,
                        })],
                        fields: vec![],
                        arguments: vec![],
                    },
                    GqlField {
                        name: "address".to_string(),
                        alias: "address".to_string(),
                        arguments: vec![],
                        fields: vec![
                            GqlField {
                                name: "address_line_1".to_string(),
                                alias: "address_line_1".to_string(),
                                arguments: vec![],
                                fields: vec![],
                                used: vec![OpenddObject::Field(FieldUsage {
                                    name: FieldName::new(identifier!("address_line_1")),
                                    opendd_type: Qualified::new(
                                        subgraph_identifier!("app"),
                                        CustomTypeName(identifier!("Address")),
                                    ),
                                    deprecated: false,
                                    deprecated_reason: None,
                                })],
                            },
                            GqlField {
                                name: "address_line_2".to_string(),
                                alias: "address_line_2".to_string(),
                                arguments: vec![],
                                fields: vec![],
                                used: vec![OpenddObject::Field(FieldUsage {
                                    name: FieldName::new(identifier!("address_line_2")),
                                    opendd_type: Qualified::new(
                                        subgraph_identifier!("app"),
                                        CustomTypeName(identifier!("Address")),
                                    ),
                                    deprecated: false,
                                    deprecated_reason: None,
                                })],
                            },
                        ],
                        used: vec![OpenddObject::Field(FieldUsage {
                            name: FieldName::new(identifier!("address")),
                            opendd_type: Qualified::new(
                                subgraph_identifier!("app"),
                                CustomTypeName(identifier!("Order")),
                            ),
                            deprecated: false,
                            deprecated_reason: None,
                        })],
                    },
                    GqlField {
                        name: "product".to_string(),
                        alias: "product".to_string(),
                        arguments: vec![products_where_argument, products_order_by_argument],
                        used: vec![product_relationship],
                        fields: vec![GqlField {
                            name: "name".to_string(),
                            alias: "name".to_string(),
                            used: vec![OpenddObject::Field(FieldUsage {
                                name: FieldName::new(identifier!("name")),
                                opendd_type: Qualified::new(
                                    subgraph_identifier!("app"),
                                    CustomTypeName(identifier!("Product")),
                                ),
                                deprecated: false,
                                deprecated_reason: None,
                            })],
                            arguments: vec![],
                            fields: vec![],
                        }],
                    },
                ],
            }],
        };
        let actual = serde_json::to_string_pretty(&operation).unwrap();
        println!("{actual}");
    }
}
