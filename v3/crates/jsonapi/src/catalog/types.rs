use super::models::build_model;
use super::object_types::build_object_type;
use crate::types::{RoleWarning, Warning};
use hasura_authn_core::Role;
use indexmap::IndexMap;
use metadata_resolve::{
    deserialize_qualified_btreemap, serialize_qualified_btreemap, Qualified,
    QualifiedTypeReference, ResolvedObjectBooleanExpressionType,
};
use open_dds::{
    data_connector::DataConnectorName,
    models::ModelName,
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, FieldName},
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Deserialize, Serialize, PartialEq)]
pub struct Catalog {
    pub state_per_role: BTreeMap<Role, State>,
}

impl Catalog {
    pub fn new(metadata: &metadata_resolve::Metadata) -> (Self, Vec<Warning>) {
        let mut warnings = vec![];

        let state_per_role = metadata
            .roles
            .iter()
            .map(|role| {
                let (state, role_warnings) = State::new(metadata, role);
                warnings.extend(role_warnings.iter().map(|warning| Warning::Role {
                    role: role.clone(),
                    warning: warning.clone(),
                }));
                (role.clone(), state)
            })
            .collect();

        (Self { state_per_role }, warnings)
    }
}

#[derive(Debug, Deserialize, Serialize, PartialEq)]
pub struct State {
    pub routes: BTreeMap<String, Model>,
    #[serde(
        serialize_with = "serialize_qualified_btreemap",
        deserialize_with = "deserialize_qualified_btreemap"
    )]
    pub object_types: BTreeMap<Qualified<CustomTypeName>, ObjectType>,
}

#[derive(Debug, Deserialize, Serialize, PartialEq)]
pub struct ObjectType {
    pub type_fields: IndexMap<FieldName, Type>,
    pub type_relationships: IndexMap<RelationshipName, RelationshipTarget>,
}

#[derive(Debug, Deserialize, Serialize, PartialEq)]
pub enum RelationshipTarget {
    Model {
        object_type: Qualified<CustomTypeName>,
        relationship_type: RelationshipType,
    },
    Command {
        type_reference: QualifiedTypeReference,
    },
}

impl State {
    pub fn new(metadata: &metadata_resolve::Metadata, role: &Role) -> (Self, Vec<RoleWarning>) {
        let mut warnings = vec![];

        let routes = metadata
            .models
            .iter()
            .filter_map(|(model_name, model)| {
                match build_model(model, role, &metadata.object_types) {
                    Ok(jsonapi_model) => Some((
                        format!("/{}/{}", model_name.subgraph, model_name.name),
                        jsonapi_model,
                    )),
                    Err(warning) => {
                        warnings.push(RoleWarning::Model {
                            model_name: model_name.clone(),
                            warning,
                        });
                        None
                    }
                }
            })
            .collect::<BTreeMap<_, _>>();

        // current naive approach is to include all types that `role` has
        // access to. we could minimise required metadata by traversing the required object types
        // when building the models, and only including those.
        let object_types = metadata
            .object_types
            .iter()
            .filter_map(|(object_type_name, object_type)| {
                match build_object_type(
                    object_type,
                    role,
                    &metadata.object_types,
                    &metadata.scalar_types,
                ) {
                    Ok(jsonapi_object_type) => {
                        Some((object_type_name.clone(), jsonapi_object_type))
                    }
                    Err(warning) => {
                        warnings.push(RoleWarning::ObjectType {
                            object_type_name: object_type_name.clone(),
                            warning,
                        });
                        None
                    }
                }
            })
            .collect::<BTreeMap<_, _>>();
        (
            Self {
                routes,
                object_types,
            },
            warnings,
        )
    }
}

// we're making the assumption here of one object type that works across all data connectors.
// this is questionable - as the scalar types might be represented differently for each one
// we have a few routes out of this:
// a) we go against GraphQL implementation and dynamically create OpenAPI type names, so we can
// make a version of each type for each data connector
// b) we make the user name provided an explicit type per data connector that implements it (urgh)
// c) we smash them all together and represent these with union types in OpenAPI where the scalar
// representations differ between connectors
// d) we smash them all together but represent them as `JSON`, ie the most general type
//
// for now we'll try d) but we should check we're happy with this before general release
// of the feature
#[derive(Debug, Deserialize, Serialize, PartialEq)]
pub enum Type {
    Scalar(ndc_models::TypeRepresentation),
    ScalarForDataConnector(ScalarTypeForDataConnector),
    List(Box<Type>),
    Object(Qualified<CustomTypeName>),
}

#[derive(Debug, Deserialize, Serialize, PartialEq)]
pub struct ScalarTypeForDataConnector {
    pub type_representations: BTreeSet<ndc_models::TypeRepresentation>,
}

// only the parts of a Model we need to construct a JSONAPI
// we'll filter out fields a given role can't see
#[derive(Debug, Deserialize, Serialize, PartialEq)]
pub struct Model {
    pub name: Qualified<ModelName>,
    pub description: Option<String>,
    pub data_type: Qualified<CustomTypeName>,
    pub data_connector_name: Qualified<DataConnectorName>,
    pub filter_expression_type: Option<ResolvedObjectBooleanExpressionType>,
}
