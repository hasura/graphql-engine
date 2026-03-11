use jsonpath::JSONPath as Path;
use std::collections::HashSet;

use crate::identifier::SubgraphName;
use crate::{
    Metadata, MetadataWithVersion, OpenDdSubgraphObject, OpenDdSupergraphObject, aggregates,
    boolean_expression, commands, data_connector, flags, graphql_config, models,
    order_by_expression, permissions, plugins, relationships, types, views,
};

const GLOBALS_SUBGRAPH: SubgraphName = SubgraphName::new_inline_static("__globals");
const UNKNOWN_SUBGRAPH: SubgraphName = SubgraphName::new_inline_static("__unknown_namespace");

pub struct QualifiedObject<T> {
    pub path: Path,
    pub subgraph: SubgraphName,
    pub object: T,
}

impl<T> QualifiedObject<T> {
    pub fn new(path: Path, subgraph: &SubgraphName, object: T) -> Self {
        QualifiedObject {
            path,
            subgraph: subgraph.clone(),
            object,
        }
    }
}

pub struct MetadataAccessor {
    pub subgraphs: HashSet<SubgraphName>,
    pub data_connectors: Vec<QualifiedObject<data_connector::DataConnectorLinkV1>>,
    pub object_types: Vec<QualifiedObject<types::ObjectTypeV1>>,
    pub object_boolean_expression_types: Vec<QualifiedObject<types::ObjectBooleanExpressionTypeV1>>,
    pub scalar_types: Vec<QualifiedObject<types::ScalarTypeV1>>,
    pub boolean_expression_types: Vec<QualifiedObject<boolean_expression::BooleanExpressionTypeV1>>,
    pub order_by_expressions: Vec<QualifiedObject<order_by_expression::OrderByExpressionV1>>,
    pub data_connector_scalar_representations:
        Vec<QualifiedObject<types::DataConnectorScalarRepresentationV1>>,
    pub aggregate_expressions: Vec<QualifiedObject<aggregates::AggregateExpressionV1>>,
    pub models: Vec<QualifiedObject<models::Model>>,
    pub type_permissions: Vec<QualifiedObject<permissions::TypePermissionsV2>>,
    pub model_permissions: Vec<QualifiedObject<permissions::ModelPermissionsV2>>,
    pub relationships: Vec<QualifiedObject<relationships::RelationshipV1>>,
    pub commands: Vec<QualifiedObject<commands::CommandV1>>,
    pub command_permissions: Vec<QualifiedObject<permissions::CommandPermissionsV2>>,
    pub flags: flags::OpenDdFlags,
    // `graphql_config` is a vector because we want to do some validation depending on the presence of the object
    pub graphql_config: Vec<QualifiedObject<graphql_config::GraphqlConfig>>,
    pub plugins: Vec<QualifiedObject<plugins::LifecyclePluginHookV1>>,
    pub views: Vec<QualifiedObject<views::ViewV1>>,
    pub view_permissions: Vec<QualifiedObject<permissions::ViewPermissions>>,
}

/// Counts of each metadata object variant, used for preallocating Vec capacity.
#[derive(Default)]
struct ObjectCounts {
    data_connectors: usize,
    graphql_config: usize,
    object_types: usize,
    scalar_types: usize,
    object_boolean_expression_types: usize,
    boolean_expression_types: usize,
    order_by_expressions: usize,
    data_connector_scalar_representations: usize,
    aggregate_expressions: usize,
    models: usize,
    type_permissions: usize,
    model_permissions: usize,
    relationships: usize,
    commands: usize,
    command_permissions: usize,
    plugins: usize,
    views: usize,
    view_permissions: usize,
}

impl ObjectCounts {
    fn count_objects(objects: &[OpenDdSubgraphObject]) -> Self {
        let mut counts = Self::default();
        for object in objects {
            match object {
                OpenDdSubgraphObject::DataConnectorLink(_) => counts.data_connectors += 1,
                OpenDdSubgraphObject::GraphqlConfig(_) => counts.graphql_config += 1,
                OpenDdSubgraphObject::ObjectType(_) => counts.object_types += 1,
                OpenDdSubgraphObject::ScalarType(_) => counts.scalar_types += 1,
                OpenDdSubgraphObject::ObjectBooleanExpressionType(_) => {
                    counts.object_boolean_expression_types += 1;
                }
                OpenDdSubgraphObject::BooleanExpressionType(_) => {
                    counts.boolean_expression_types += 1;
                }
                OpenDdSubgraphObject::OrderByExpression(_) => counts.order_by_expressions += 1,
                OpenDdSubgraphObject::DataConnectorScalarRepresentation(_) => {
                    counts.data_connector_scalar_representations += 1;
                }
                OpenDdSubgraphObject::AggregateExpression(_) => {
                    counts.aggregate_expressions += 1;
                }
                OpenDdSubgraphObject::Model(_) => counts.models += 1,
                OpenDdSubgraphObject::TypePermissions(_) => counts.type_permissions += 1,
                OpenDdSubgraphObject::ModelPermissions(_) => counts.model_permissions += 1,
                OpenDdSubgraphObject::Relationship(_) => counts.relationships += 1,
                OpenDdSubgraphObject::Command(_) => counts.commands += 1,
                OpenDdSubgraphObject::CommandPermissions(_) => counts.command_permissions += 1,
                OpenDdSubgraphObject::LifecyclePluginHook(_) => counts.plugins += 1,
                OpenDdSubgraphObject::View(_) => counts.views += 1,
                OpenDdSubgraphObject::ViewPermissions(_) => counts.view_permissions += 1,
            }
        }
        counts
    }

    fn add(&mut self, other: &Self) {
        self.data_connectors += other.data_connectors;
        self.graphql_config += other.graphql_config;
        self.object_types += other.object_types;
        self.scalar_types += other.scalar_types;
        self.object_boolean_expression_types += other.object_boolean_expression_types;
        self.boolean_expression_types += other.boolean_expression_types;
        self.order_by_expressions += other.order_by_expressions;
        self.data_connector_scalar_representations += other.data_connector_scalar_representations;
        self.aggregate_expressions += other.aggregate_expressions;
        self.models += other.models;
        self.type_permissions += other.type_permissions;
        self.model_permissions += other.model_permissions;
        self.relationships += other.relationships;
        self.commands += other.commands;
        self.command_permissions += other.command_permissions;
        self.plugins += other.plugins;
        self.views += other.views;
        self.view_permissions += other.view_permissions;
    }
}

fn load_metadata_objects(
    metadata_objects: Vec<OpenDdSubgraphObject>,
    subgraph: &SubgraphName,
    accessor: &mut MetadataAccessor,
) {
    accessor.subgraphs.insert(subgraph.clone());
    for object in metadata_objects {
        match object {
            OpenDdSubgraphObject::DataConnectorLink(data_connector) => {
                accessor.data_connectors.push(QualifiedObject::new(
                    data_connector.path,
                    subgraph,
                    data_connector.value.upgrade(),
                ));
            }
            OpenDdSubgraphObject::GraphqlConfig(graphql_config) => {
                accessor.graphql_config.push(QualifiedObject::new(
                    graphql_config.path,
                    subgraph,
                    graphql_config.value.clone(),
                ));
            }
            OpenDdSubgraphObject::ObjectType(object_type) => {
                accessor.object_types.push(QualifiedObject::new(
                    object_type.path,
                    subgraph,
                    object_type.value.upgrade(),
                ));
            }
            OpenDdSubgraphObject::ScalarType(scalar_type) => {
                accessor.scalar_types.push(QualifiedObject::new(
                    scalar_type.path,
                    subgraph,
                    scalar_type.value.upgrade(),
                ));
            }
            OpenDdSubgraphObject::ObjectBooleanExpressionType(object_boolean_expression_type) => {
                accessor
                    .object_boolean_expression_types
                    .push(QualifiedObject::new(
                        object_boolean_expression_type.path,
                        subgraph,
                        object_boolean_expression_type.value.upgrade(),
                    ));
            }
            OpenDdSubgraphObject::BooleanExpressionType(boolean_expression_type) => {
                accessor.boolean_expression_types.push(QualifiedObject::new(
                    boolean_expression_type.path,
                    subgraph,
                    boolean_expression_type.value.upgrade(),
                ));
            }
            OpenDdSubgraphObject::OrderByExpression(order_by_expression) => {
                accessor.order_by_expressions.push(QualifiedObject::new(
                    order_by_expression.path,
                    subgraph,
                    order_by_expression.value.upgrade(),
                ));
            }
            OpenDdSubgraphObject::DataConnectorScalarRepresentation(scalar_representation) => {
                accessor
                    .data_connector_scalar_representations
                    .push(QualifiedObject::new(
                        scalar_representation.path,
                        subgraph,
                        scalar_representation.value.upgrade(),
                    ));
            }
            OpenDdSubgraphObject::AggregateExpression(aggregate_expression) => {
                accessor.aggregate_expressions.push(QualifiedObject::new(
                    aggregate_expression.path,
                    subgraph,
                    aggregate_expression.value.upgrade(),
                ));
            }
            OpenDdSubgraphObject::Model(model) => {
                accessor.models.push(QualifiedObject::new(
                    model.path,
                    subgraph,
                    model.value.upgrade(),
                ));
            }
            OpenDdSubgraphObject::TypePermissions(permissions) => {
                accessor.type_permissions.push(QualifiedObject::new(
                    permissions.path,
                    subgraph,
                    permissions.value.upgrade(),
                ));
            }
            OpenDdSubgraphObject::ModelPermissions(permissions) => {
                accessor.model_permissions.push(QualifiedObject::new(
                    permissions.path,
                    subgraph,
                    permissions.value.upgrade(),
                ));
            }
            OpenDdSubgraphObject::Relationship(relationship) => {
                accessor.relationships.push(QualifiedObject::new(
                    relationship.path,
                    subgraph,
                    relationship.value.upgrade(),
                ));
            }
            OpenDdSubgraphObject::Command(command) => {
                accessor.commands.push(QualifiedObject::new(
                    command.path,
                    subgraph,
                    command.value.upgrade(),
                ));
            }
            OpenDdSubgraphObject::CommandPermissions(permissions) => {
                accessor.command_permissions.push(QualifiedObject::new(
                    permissions.path,
                    subgraph,
                    permissions.value.upgrade(),
                ));
            }
            OpenDdSubgraphObject::LifecyclePluginHook(plugin) => {
                accessor.plugins.push(QualifiedObject::new(
                    plugin.path,
                    subgraph,
                    plugin.value.upgrade(),
                ));
            }
            OpenDdSubgraphObject::View(view) => {
                accessor.views.push(QualifiedObject::new(
                    view.path,
                    subgraph,
                    view.value.upgrade(),
                ));
            }
            OpenDdSubgraphObject::ViewPermissions(view_permissions) => {
                accessor.view_permissions.push(QualifiedObject {
                    path: view_permissions.path.clone(),
                    subgraph: subgraph.clone(),
                    object: view_permissions.value.clone(),
                });
            }
        }
    }
}

fn load_metadata_supergraph_object(
    supergraph_object: OpenDdSupergraphObject,
    accessor: &mut MetadataAccessor,
) {
    match supergraph_object {
        OpenDdSupergraphObject::GraphqlConfig(graphql_config) => {
            accessor.graphql_config.push(QualifiedObject::new(
                Path::new(),
                &GLOBALS_SUBGRAPH,
                graphql_config,
            ));
        }
    }
}

impl MetadataAccessor {
    pub fn new(metadata: Metadata) -> MetadataAccessor {
        match metadata {
            Metadata::WithoutNamespaces(metadata) => {
                let counts = ObjectCounts::count_objects(&metadata);
                let mut accessor = MetadataAccessor::new_empty(None, &counts);
                load_metadata_objects(metadata, &UNKNOWN_SUBGRAPH, &mut accessor);
                accessor
            }
            Metadata::Versioned(MetadataWithVersion::V1(metadata)) => {
                let mut counts = ObjectCounts::default();
                for ns in &metadata.namespaces {
                    counts.add(&ObjectCounts::count_objects(&ns.objects));
                }
                let mut accessor = MetadataAccessor::new_empty(Some(metadata.flags), &counts);
                for namespaced_metadata in metadata.namespaces {
                    let subgraph = SubgraphName::new_without_validation(&namespaced_metadata.name);
                    load_metadata_objects(namespaced_metadata.objects, &subgraph, &mut accessor);
                }
                accessor
            }
            Metadata::Versioned(MetadataWithVersion::V2(metadata)) => {
                let mut counts = ObjectCounts::default();
                for subgraph in &metadata.subgraphs {
                    counts.add(&ObjectCounts::count_objects(&subgraph.objects));
                }
                let mut accessor = MetadataAccessor::new_empty(Some(metadata.flags), &counts);
                for supergraph_object in metadata.supergraph.objects {
                    load_metadata_supergraph_object(supergraph_object, &mut accessor);
                }
                for subgraph in metadata.subgraphs {
                    load_metadata_objects(subgraph.objects, &subgraph.name.into(), &mut accessor);
                }
                accessor
            }
            Metadata::Versioned(MetadataWithVersion::V3(metadata)) => {
                let mut counts = ObjectCounts::default();
                for subgraph in &metadata.subgraphs {
                    counts.add(&ObjectCounts::count_objects(&subgraph.objects));
                }
                let mut accessor = MetadataAccessor::new_empty(Some(metadata.flags), &counts);
                for subgraph in metadata.subgraphs {
                    load_metadata_objects(subgraph.objects, &subgraph.name.into(), &mut accessor);
                }
                accessor
            }
        }
    }

    fn new_empty(flags: Option<flags::OpenDdFlags>, counts: &ObjectCounts) -> MetadataAccessor {
        MetadataAccessor {
            subgraphs: HashSet::new(),
            data_connectors: Vec::with_capacity(counts.data_connectors),
            object_types: Vec::with_capacity(counts.object_types),
            scalar_types: Vec::with_capacity(counts.scalar_types),
            object_boolean_expression_types: Vec::with_capacity(
                counts.object_boolean_expression_types,
            ),
            boolean_expression_types: Vec::with_capacity(counts.boolean_expression_types),
            order_by_expressions: Vec::with_capacity(counts.order_by_expressions),
            data_connector_scalar_representations: Vec::with_capacity(
                counts.data_connector_scalar_representations,
            ),
            aggregate_expressions: Vec::with_capacity(counts.aggregate_expressions),
            models: Vec::with_capacity(counts.models),
            type_permissions: Vec::with_capacity(counts.type_permissions),
            model_permissions: Vec::with_capacity(counts.model_permissions),
            relationships: Vec::with_capacity(counts.relationships),
            commands: Vec::with_capacity(counts.commands),
            command_permissions: Vec::with_capacity(counts.command_permissions),
            flags: flags.unwrap_or_default(),
            graphql_config: Vec::with_capacity(counts.graphql_config),
            plugins: Vec::with_capacity(counts.plugins),
            views: Vec::with_capacity(counts.views),
            view_permissions: Vec::with_capacity(counts.view_permissions),
        }
    }
}
