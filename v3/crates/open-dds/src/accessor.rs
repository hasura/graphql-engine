use crate::{graphql_config, MetadataWithVersion, OpenDdSupergraphObject};

use super::{
    commands, data_connector, flags, models, permissions, relationships, types, Metadata,
    OpenDdSubgraphObject,
};

pub struct QualifiedObject<T> {
    pub subgraph: String,
    pub object: T,
}

impl<T> QualifiedObject<T> {
    pub fn new(subgraph: &str, object: T) -> Self {
        QualifiedObject {
            subgraph: subgraph.to_string(),
            object,
        }
    }
}

lazy_static::lazy_static! {
    static ref DEFAULT_FLAGS: flags::Flags = flags::Flags::default();
}

pub struct MetadataAccessor {
    pub data_connectors: Vec<QualifiedObject<data_connector::DataConnectorLinkV1>>,
    pub object_types: Vec<QualifiedObject<types::ObjectTypeV1>>,
    pub object_boolean_expression_types: Vec<QualifiedObject<types::ObjectBooleanExpressionTypeV1>>,
    pub scalar_types: Vec<QualifiedObject<types::ScalarTypeV1>>,
    pub data_connector_scalar_representations:
        Vec<QualifiedObject<types::DataConnectorScalarRepresentationV1>>,
    pub models: Vec<QualifiedObject<models::ModelV1>>,
    pub type_permissions: Vec<QualifiedObject<permissions::TypePermissionsV1>>,
    pub model_permissions: Vec<QualifiedObject<permissions::ModelPermissionsV1>>,
    pub relationships: Vec<QualifiedObject<relationships::RelationshipV1>>,
    pub commands: Vec<QualifiedObject<commands::CommandV1>>,
    pub command_permissions: Vec<QualifiedObject<permissions::CommandPermissionsV1>>,
    pub flags: flags::Flags,
    // `graphql_config` is a vector because we want to do some validation depending on the presence of the object
    pub graphql_config: Vec<graphql_config::GraphqlConfig>,
}

fn load_metadata_objects(
    metadata_objects: Vec<OpenDdSubgraphObject>,
    subgraph: &str,
    accessor: &mut MetadataAccessor,
) {
    for object in metadata_objects {
        match object {
            OpenDdSubgraphObject::DataConnectorLink(data_connector) => {
                accessor
                    .data_connectors
                    .push(QualifiedObject::new(subgraph, data_connector.upgrade()));
            }
            OpenDdSubgraphObject::ObjectType(object_type) => {
                accessor
                    .object_types
                    .push(QualifiedObject::new(subgraph, object_type.upgrade()));
            }
            OpenDdSubgraphObject::ScalarType(scalar_type) => {
                accessor
                    .scalar_types
                    .push(QualifiedObject::new(subgraph, scalar_type.upgrade()));
            }
            OpenDdSubgraphObject::ObjectBooleanExpressionType(object_boolean_expression_type) => {
                accessor
                    .object_boolean_expression_types
                    .push(QualifiedObject::new(
                        subgraph,
                        object_boolean_expression_type.upgrade(),
                    ));
            }
            OpenDdSubgraphObject::DataConnectorScalarRepresentation(scalar_representation) => {
                accessor
                    .data_connector_scalar_representations
                    .push(QualifiedObject::new(
                        subgraph,
                        scalar_representation.upgrade(),
                    ));
            }
            OpenDdSubgraphObject::Model(model) => {
                accessor
                    .models
                    .push(QualifiedObject::new(subgraph, model.upgrade()));
            }
            OpenDdSubgraphObject::TypePermissions(permissions) => {
                accessor
                    .type_permissions
                    .push(QualifiedObject::new(subgraph, permissions.upgrade()));
            }
            OpenDdSubgraphObject::ModelPermissions(permissions) => {
                accessor
                    .model_permissions
                    .push(QualifiedObject::new(subgraph, permissions.upgrade()));
            }
            OpenDdSubgraphObject::Relationship(relationship) => {
                accessor
                    .relationships
                    .push(QualifiedObject::new(subgraph, relationship.upgrade()));
            }
            OpenDdSubgraphObject::Command(command) => {
                accessor
                    .commands
                    .push(QualifiedObject::new(subgraph, command.upgrade()));
            }
            OpenDdSubgraphObject::CommandPermissions(permissions) => {
                accessor
                    .command_permissions
                    .push(QualifiedObject::new(subgraph, permissions.upgrade()));
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
            accessor.graphql_config.push(graphql_config);
        }
    }
}

impl MetadataAccessor {
    pub fn new(metadata: Metadata) -> MetadataAccessor {
        match metadata {
            Metadata::WithoutNamespaces(metadata) => {
                let mut accessor: MetadataAccessor = MetadataAccessor::new_empty(None);
                load_metadata_objects(metadata, "unknown_namespace", &mut accessor);
                accessor
            }
            Metadata::Versioned(MetadataWithVersion::V1(metadata)) => {
                let mut accessor: MetadataAccessor =
                    MetadataAccessor::new_empty(Some(metadata.flags));
                for namespaced_metadata in metadata.namespaces {
                    let namespace = &namespaced_metadata.name;
                    load_metadata_objects(namespaced_metadata.objects, namespace, &mut accessor);
                }
                accessor
            }
            Metadata::Versioned(MetadataWithVersion::V2(metadata)) => {
                let mut accessor: MetadataAccessor =
                    MetadataAccessor::new_empty(Some(metadata.flags));
                for supergraph_object in metadata.supergraph.objects.into_iter() {
                    load_metadata_supergraph_object(supergraph_object, &mut accessor);
                }
                for subgraph in metadata.subgraphs {
                    load_metadata_objects(subgraph.objects, &subgraph.name, &mut accessor);
                }
                accessor
            }
        }
    }

    fn new_empty(flags: Option<flags::Flags>) -> MetadataAccessor {
        MetadataAccessor {
            data_connectors: vec![],
            object_types: vec![],
            scalar_types: vec![],
            object_boolean_expression_types: vec![],
            data_connector_scalar_representations: vec![],
            models: vec![],
            type_permissions: vec![],
            model_permissions: vec![],
            relationships: vec![],
            commands: vec![],
            command_permissions: vec![],
            flags: flags.unwrap_or_else(|| DEFAULT_FLAGS.clone()),
            graphql_config: vec![],
        }
    }
}
