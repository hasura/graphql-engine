use hasura_authn_core::Role;
use metadata_resolve::{
    ModelExpressionType, ModelWithArgumentPresets, ObjectTypeWithRelationships, Qualified,
};
use open_dds::{
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
                match Model::new(model, role, &metadata.object_types) {
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

// only the parts of a Model we need to construct a JSONAPI
// we'll filter out fields a given role can't see
#[derive(Debug)]
pub struct Model {
    pub name: Qualified<ModelName>,
    pub type_fields: Vec<FieldName>,
    /// let's consider only making this work with `BooleanExpressionType`
    /// to simplify implementation and nudge users to upgrade
    pub filter_expression_type: Option<ModelExpressionType>,
}

impl Model {
    pub fn new(
        model: &ModelWithArgumentPresets,
        role: &Role,
        object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    ) -> Result<Model, ModelWarning> {
        let type_fields = get_model_fields(model, role, object_types)?;
        Ok(Model {
            name: model.model.name.clone(),
            type_fields,
            filter_expression_type: model.filter_expression_type.clone(),
        })
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
pub enum ModelWarning {
    NoSelectPermission,
    NoObjectTypeFound {
        object_type_name: Qualified<CustomTypeName>,
    },
}

// look at permissions and work out which fields we're allowed to see
// this is quite limited and leans to be overcautious
fn get_model_fields(
    model: &ModelWithArgumentPresets,
    role: &Role,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
) -> Result<Vec<FieldName>, ModelWarning> {
    // if we have no select permission for the model, ignore it
    if !model.select_permissions.contains_key(role) {
        return Err(ModelWarning::NoSelectPermission);
    }
    let underlying_object_type = object_types.get(&model.model.data_type).ok_or_else(|| {
        ModelWarning::NoObjectTypeFound {
            object_type_name: model.model.data_type.clone(),
        }
    })?;
    let output_permissions_for_role = underlying_object_type
        .type_output_permissions
        .get(role)
        .unwrap();

    // otherwise return all fields (we need to check the type perms next and filter this)
    let type_fields = model
        .model
        .type_fields
        .keys()
        .filter(|field_name| {
            output_permissions_for_role
                .allowed_fields
                .contains(*field_name)
        })
        .cloned()
        .collect();
    Ok(type_fields)
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
