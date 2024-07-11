use std::collections::BTreeMap;

use open_dds::{
    models::ModelName,
    types::{CustomTypeName, FieldName},
};

use crate::types::subgraph::Qualified;

/// This isn't a particularly satisfying resolve step, as it only serves to validate
/// the output of previous steps.
/// Ideally, we could move more Apollo-based resolving into this discreet step, haven't
/// investigated this too deeply yet.
pub fn resolve(
    apollo_federation_entity_enabled_types: &BTreeMap<
        Qualified<CustomTypeName>,
        Option<Qualified<open_dds::models::ModelName>>,
    >,
) -> Result<(), ApolloError> {
    // To check if apollo federation entity keys are defined in object type but no model has
    // apollo_federation_entity_source set to true:
    //   - Throw an error if no model with apolloFederation.entitySource:true is found for the object type.
    for (object_type, model_name_list) in apollo_federation_entity_enabled_types {
        if model_name_list.is_none() {
            return Err(ApolloError::ApolloFederationEntitySourceNotDefined {
                object_type: object_type.clone(),
            });
        }
    }
    Ok(())
}

#[derive(Debug, thiserror::Error)]
pub enum ApolloError {
    #[error("empty fields in apollo federation keys defined for the object type {object_type:}")]
    EmptyFieldsInApolloFederationConfigForObject {
        object_type: Qualified<CustomTypeName>,
    },

    #[error("unknown field {field_name:} in apollo federation keys defined for the object type {object_type:}")]
    UnknownFieldInApolloFederationKey {
        field_name: FieldName,
        object_type: Qualified<CustomTypeName>,
    },
    #[error(
        "empty keys in apollo federation configuration defined for the object type {object_type:}"
    )]
    EmptyKeysInApolloFederationConfigForObject {
        object_type: Qualified<CustomTypeName>,
    },
    #[error("'apolloFederation.keys' for type {object_type:} found, but no model found with 'apolloFederation.entitySource: true' for type {object_type:}")]
    ApolloFederationEntitySourceNotDefined {
        object_type: Qualified<CustomTypeName>,
    },
    #[error(
        "model {model_name:} with arguments is unsupported as an Apollo Federation entity source"
    )]
    ModelWithArgumentsAsApolloFederationEntitySource { model_name: Qualified<ModelName> },

    #[error("Model {model_name:} is marked as an Apollo Federation entity source but there are no keys fields present in the related object type {type_name:}")]
    NoKeysFieldsPresentInEntitySource {
        type_name: Qualified<CustomTypeName>,
        model_name: ModelName,
    },
}
