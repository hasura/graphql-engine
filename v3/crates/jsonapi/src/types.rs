use crate::catalog::get_model_fields;
use hasura_authn_core::Role;
use indexmap::IndexMap;
use metadata_resolve::{
    ModelExpressionType, ModelWithArgumentPresets, ObjectTypeWithRelationships, Qualified,
    ScalarTypeRepresentation,
};
use open_dds::{
    data_connector::DataConnectorName,
    identifier::SubgraphName,
    models::ModelName,
    types::{CustomTypeName, FieldName},
};
use std::collections::BTreeMap;
use tracing_util::{ErrorVisibility, TraceableError};

#[derive(Debug)]
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

#[derive(Debug)]
pub struct State {
    pub routes: BTreeMap<String, Model>,
}

impl State {
    pub fn new(metadata: &metadata_resolve::Metadata, role: &Role) -> (Self, Vec<RoleWarning>) {
        let mut warnings = vec![];

        let routes = metadata
            .models
            .iter()
            .filter_map(|(model_name, model)| {
                match Model::new(model, role, &metadata.object_types, &metadata.scalar_types) {
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

        (Self { routes }, warnings)
    }
}

// feel we're going to need to think about object types for nested stuff here too
#[derive(Debug)]
pub enum FieldType {
    TypeRepresentation(ndc_models::TypeRepresentation),
    List(Box<FieldType>),
    Object(IndexMap<FieldName, FieldType>),
}

// only the parts of a Model we need to construct a JSONAPI
// we'll filter out fields a given role can't see
#[derive(Debug)]
pub struct Model {
    pub name: Qualified<ModelName>,
    pub description: Option<String>,
    pub data_type: Qualified<CustomTypeName>,
    pub type_fields: IndexMap<FieldName, FieldType>,
    /// let's consider only making this work with `BooleanExpressionType`
    /// to simplify implementation and nudge users to upgrade
    pub filter_expression_type: Option<ModelExpressionType>,
}

impl Model {
    pub fn new(
        model: &ModelWithArgumentPresets,
        role: &Role,
        object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
        scalar_types: &BTreeMap<Qualified<CustomTypeName>, ScalarTypeRepresentation>,
    ) -> Result<Model, ModelWarning> {
        let type_fields = get_model_fields(model, role, object_types, scalar_types)?;

        Ok(Model {
            name: model.model.name.clone(),
            description: model.model.raw.description.clone(),
            data_type: model.model.data_type.clone(),
            type_fields,
            filter_expression_type: model.filter_expression_type.clone(),
        })
    }

    pub fn pretty_typename(&self) -> String {
        format!("{}_{}", self.data_type.subgraph, self.data_type.name)
    }
}

#[derive(Debug, Clone)]
pub enum Warning {
    Role { role: Role, warning: RoleWarning },
}

#[derive(Debug, Clone)]
pub enum RoleWarning {
    Model {
        model_name: Qualified<ModelName>,
        warning: ModelWarning,
    },
}

// if we exclude something, let's say why
#[derive(Debug, Clone)]
#[allow(clippy::enum_variant_names)]
pub enum ModelWarning {
    NoSelectPermission,
    NoObjectTypeFound {
        object_type_name: Qualified<CustomTypeName>,
    },
    NoModelSource,
    NoTypeRepresentationFound {
        object_type_name: Qualified<CustomTypeName>,
    },
    NoTypeRepresentationFoundForDataConnector {
        data_connector_name: Qualified<DataConnectorName>,
        object_type_name: Qualified<CustomTypeName>,
    },
}

#[derive(Debug, derive_more::Display)]
pub enum RequestError {
    NotFound,
    BadRequest(String),
    InternalError(InternalError),
    PlanError(plan::PlanError),
    ExecuteError(execute::FieldError),
}

#[derive(Debug, derive_more::Display)]
pub enum InternalError {
    EmptyQuerySet,
}

impl TraceableError for RequestError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::User
    }
}

/// Model related info derived from URI path
#[allow(dead_code)]
pub struct ModelInfo {
    pub subgraph: SubgraphName,
    pub name: ModelName,
    /// value of the unique identifier of the model.
    // TODO: this won't be a string always
    pub unique_identifier: Option<String>,
    /// path to a relationship; like `["artist", "albums", "tracks"]`
    pub relationship: Vec<String>,
}

pub struct ParseError(String);

impl From<&str> for ParseError {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

impl From<ParseError> for RequestError {
    fn from(value: ParseError) -> Self {
        Self::BadRequest(format!("Parse Error: {}", value.0))
    }
}

// this is not the correct output type, we should be outputting a JSONAPI document instead
pub struct QueryResult {
    pub type_name: Qualified<CustomTypeName>,
    pub rowsets: Vec<ndc_models::RowSet>,
}
