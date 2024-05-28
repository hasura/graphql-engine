//! Usage analytics, like model, command, field usage analytics, from a GraphQL query

use metadata_resolve::Qualified;
use open_dds::{
    commands::CommandName,
    models::ModelName,
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, FieldName},
};
use serde::Serialize;

/// This is the data to emit (serlialized) for analytics, when a GraphQL
/// operation is executed.
#[derive(Serialize)]
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
}

/// A GraphQL field appearing in the query
#[derive(Serialize)]
pub struct GqlField {
    /// Name of the GraphQL field
    pub name: String,
    /// Alias of this field used in the query
    pub alias: String,
    /// Arguments of this field
    pub arguments: Vec<GqlFieldArgument>,
    /// Fields in its selection set
    pub fields: Vec<GqlField>,
    /// Which OpenDD object it is using
    pub used: OpenddObject,
}

#[derive(Serialize)]
/// A GraphQL input field
pub struct GqlInputField {
    /// Name of the input field
    pub name: String,
    /// Fields of this input field
    pub fields: Vec<GqlInputField>,
    /// Which OpenDD object it is using
    pub used: Option<OpenddObject>,
}

/// Arguments of a GraphQL field
#[derive(Serialize)]
pub struct GqlFieldArgument {
    pub name: String,
    pub fields: Vec<GqlInputField>,
}

/// All kinds of OpenDD objects that could be used in a GraphQL operation
#[derive(Serialize, Clone)]
#[serde(rename_all = "snake_case")]
pub enum OpenddObject {
    Model { name: Qualified<ModelName> },
    Command { name: Qualified<CommandName> },
    Field(FieldUsage),
    Permission(PermissionsUsage),
    Relationship(RelationshipUsage),
}

#[derive(Serialize, Clone)]
pub struct FieldUsage {
    pub name: FieldName,
    pub opendd_type: Qualified<CustomTypeName>,
}

#[derive(Serialize, Clone)]
pub struct PermissionsUsage {
    pub fields: Vec<FieldUsage>,
}

#[derive(Serialize, Clone)]
pub struct RelationshipUsage {
    pub name: Qualified<RelationshipName>,
    pub source: Qualified<CustomTypeName>,
    pub target: RelationshipTarget,
}

#[derive(Serialize, Clone)]
#[serde(rename_all = "snake_case")]
pub enum RelationshipTarget {
    Model {
        model_name: Qualified<ModelName>,
        relationship_type: RelationshipType,
    },
    Command {
        command_name: Qualified<CommandName>,
        relationship_type: RelationshipType,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use open_dds::identifier;
    use open_dds::relationships::RelationshipType;

    #[test]
    // just a dummy serialize test for now to visualize the output
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
            name: Qualified::new("app".to_string(), RelationshipName(identifier!("product"))),
            source: Qualified::new("app".to_string(), CustomTypeName(identifier!("Order"))),
            target: RelationshipTarget::Model {
                model_name: Qualified::new("app".to_string(), ModelName(identifier!("Products"))),
                relationship_type: RelationshipType::Object,
            },
        });

        // id: {_eq: 5}
        let product_id_filter = GqlInputField {
            name: "id".to_string(),
            used: Some(OpenddObject::Field(FieldUsage {
                name: FieldName(identifier!("id")),
                opendd_type: Qualified::new(
                    "app".to_string(),
                    CustomTypeName(identifier!("Order")),
                ),
            })),
            fields: vec![GqlInputField {
                name: "_eq".to_string(),
                used: None,
                fields: vec![],
            }],
        };
        // products: {price: {_gt: 100}}
        let products_price_filter = GqlInputField {
            name: "products".to_string(),
            fields: vec![GqlInputField {
                name: "price".to_string(),
                used: Some(OpenddObject::Field(FieldUsage {
                    name: FieldName(identifier!("price")),
                    opendd_type: Qualified::new(
                        "app".to_string(),
                        CustomTypeName(identifier!("Product")),
                    ),
                })),
                fields: vec![GqlInputField {
                    name: "_gt".to_string(),
                    used: None,
                    fields: vec![],
                }],
            }],
            used: Some(product_relationship.clone()),
        };
        // where: {id: {_eq: 5}, products: {price: {_gt: 100}}}
        let where_argument = GqlFieldArgument {
            name: "where".to_string(),
            fields: vec![product_id_filter, products_price_filter],
        };

        // order_by: {product: {price: asc}}
        let order_by_argument = GqlFieldArgument {
            name: "order_by".to_string(),
            fields: vec![GqlInputField {
                name: "product".to_string(),
                fields: vec![GqlInputField {
                    name: "price".to_string(),
                    fields: vec![],
                    used: Some(OpenddObject::Field(FieldUsage {
                        name: FieldName(identifier!("price")),
                        opendd_type: Qualified::new(
                            "app".to_string(),
                            CustomTypeName(identifier!("Product")),
                        ),
                    })),
                }],
                used: Some(product_relationship.clone()),
            }],
        };

        // quantity: {_gt: 2}
        let products_quantity_filter = GqlInputField {
            name: "quantity".to_string(),
            used: Some(OpenddObject::Field(FieldUsage {
                name: FieldName(identifier!("quantity")),
                opendd_type: Qualified::new(
                    "app".to_string(),
                    CustomTypeName(identifier!("Product")),
                ),
            })),
            fields: vec![GqlInputField {
                name: "_gt".to_string(),
                used: None,
                fields: vec![],
            }],
        };
        // where: {quantity: {_gt: 2}}
        let products_where_argument = GqlFieldArgument {
            name: "where".to_string(),
            fields: vec![products_quantity_filter],
        };
        // order_by: {quantity: desc}
        let products_order_by_argument = GqlFieldArgument {
            name: "order_by".to_string(),
            fields: vec![GqlInputField {
                name: "quantity".to_string(),
                fields: vec![],
                used: Some(OpenddObject::Field(FieldUsage {
                    name: FieldName(identifier!("quantity")),
                    opendd_type: Qualified::new(
                        "app".to_string(),
                        CustomTypeName(identifier!("Product")),
                    ),
                })),
            }],
        };

        let operation = GqlOperation::Query {
            operation_name: "MyQuery".to_string(),
            fields: vec![GqlField {
                name: "app_orders".to_string(),
                alias: "orders".to_string(),
                arguments: vec![where_argument, order_by_argument],
                used: OpenddObject::Model {
                    name: Qualified::new("app".to_string(), ModelName(identifier!("Orders"))),
                },
                fields: vec![
                    GqlField {
                        name: "date".to_string(),
                        alias: "date".to_string(),
                        used: OpenddObject::Field(FieldUsage {
                            name: FieldName(identifier!("date")),
                            opendd_type: Qualified::new(
                                "app".to_string(),
                                CustomTypeName(identifier!("Order")),
                            ),
                        }),
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
                                used: OpenddObject::Field(FieldUsage {
                                    name: FieldName(identifier!("address_line_1")),
                                    opendd_type: Qualified::new(
                                        "app".to_string(),
                                        CustomTypeName(identifier!("Address")),
                                    ),
                                }),
                            },
                            GqlField {
                                name: "address_line_2".to_string(),
                                alias: "address_line_2".to_string(),
                                arguments: vec![],
                                fields: vec![],
                                used: OpenddObject::Field(FieldUsage {
                                    name: FieldName(identifier!("address_line_2")),
                                    opendd_type: Qualified::new(
                                        "app".to_string(),
                                        CustomTypeName(identifier!("Address")),
                                    ),
                                }),
                            },
                        ],
                        used: OpenddObject::Field(FieldUsage {
                            name: FieldName(identifier!("address")),
                            opendd_type: Qualified::new(
                                "app".to_string(),
                                CustomTypeName(identifier!("Order")),
                            ),
                        }),
                    },
                    GqlField {
                        name: "product".to_string(),
                        alias: "product".to_string(),
                        arguments: vec![products_where_argument, products_order_by_argument],
                        used: product_relationship,
                        fields: vec![GqlField {
                            name: "name".to_string(),
                            alias: "name".to_string(),
                            used: OpenddObject::Field(FieldUsage {
                                name: FieldName(identifier!("name")),
                                opendd_type: Qualified::new(
                                    "app".to_string(),
                                    CustomTypeName(identifier!("Product")),
                                ),
                            }),
                            arguments: vec![],
                            fields: vec![],
                        }],
                    },
                ],
            }],
        };
        let actual = serde_json::to_string_pretty(&operation).unwrap();
        println!("{}", actual);
    }
}
