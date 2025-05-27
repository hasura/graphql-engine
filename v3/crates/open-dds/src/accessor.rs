use jsonpath::JSONPath as Path;
use std::collections::HashSet;

use crate::identifier::SubgraphName;
use crate::{
    Metadata, MetadataWithVersion, OpenDdSubgraphObject, OpenDdSupergraphObject, aggregates,
    boolean_expression, commands, data_connector, flags, glossary, graphql_config, models,
    order_by_expression, permissions, plugins, relationships, types,
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
    pub type_permissions: Vec<QualifiedObject<permissions::TypePermissionsV1>>,
    pub model_permissions: Vec<QualifiedObject<permissions::ModelPermissionsV1>>,
    pub relationships: Vec<QualifiedObject<relationships::RelationshipV1>>,
    pub commands: Vec<QualifiedObject<commands::CommandV1>>,
    pub command_permissions: Vec<QualifiedObject<permissions::CommandPermissionsV1>>,
    pub flags: flags::OpenDdFlags,
    // `graphql_config` is a vector because we want to do some validation depending on the presence of the object
    pub graphql_config: Vec<QualifiedObject<graphql_config::GraphqlConfig>>,
    pub plugins: Vec<QualifiedObject<plugins::LifecyclePluginHookV1>>,
    pub glossaries: Vec<QualifiedObject<glossary::GlossaryV1>>,
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
            OpenDdSubgraphObject::Glossary(glossary) => {
                accessor.glossaries.push(QualifiedObject::new(
                    glossary.path,
                    subgraph,
                    glossary.value.upgrade(),
                ));
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
                let mut accessor: MetadataAccessor = MetadataAccessor::new_empty(None);
                load_metadata_objects(metadata, &UNKNOWN_SUBGRAPH, &mut accessor);
                accessor
            }
            Metadata::Versioned(MetadataWithVersion::V1(metadata)) => {
                let mut accessor: MetadataAccessor =
                    MetadataAccessor::new_empty(Some(metadata.flags));
                for namespaced_metadata in metadata.namespaces {
                    let subgraph = SubgraphName::new_without_validation(&namespaced_metadata.name);
                    load_metadata_objects(namespaced_metadata.objects, &subgraph, &mut accessor);
                }
                accessor
            }
            Metadata::Versioned(MetadataWithVersion::V2(metadata)) => {
                let mut accessor: MetadataAccessor =
                    MetadataAccessor::new_empty(Some(metadata.flags));
                for supergraph_object in metadata.supergraph.objects {
                    load_metadata_supergraph_object(supergraph_object, &mut accessor);
                }
                for subgraph in metadata.subgraphs {
                    load_metadata_objects(subgraph.objects, &subgraph.name.into(), &mut accessor);
                }
                accessor
            }
            Metadata::Versioned(MetadataWithVersion::V3(metadata)) => {
                let mut accessor: MetadataAccessor =
                    MetadataAccessor::new_empty(Some(metadata.flags));
                for subgraph in metadata.subgraphs {
                    load_metadata_objects(subgraph.objects, &subgraph.name.into(), &mut accessor);
                }
                accessor
            }
        }
    }

    fn new_empty(flags: Option<flags::OpenDdFlags>) -> MetadataAccessor {
        MetadataAccessor {
            subgraphs: HashSet::new(),
            data_connectors: vec![],
            object_types: vec![],
            scalar_types: vec![],
            object_boolean_expression_types: vec![],
            boolean_expression_types: vec![],
            order_by_expressions: vec![],
            data_connector_scalar_representations: vec![],
            aggregate_expressions: vec![],
            models: vec![],
            type_permissions: vec![],
            model_permissions: vec![],
            relationships: vec![],
            commands: vec![],
            command_permissions: vec![],
            flags: flags.unwrap_or_default(),
            graphql_config: vec![],
            plugins: vec![],
            glossaries: vec![],
        }
    }
}
