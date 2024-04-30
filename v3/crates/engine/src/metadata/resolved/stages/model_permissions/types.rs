use crate::metadata::resolved::stages::models;
use crate::metadata::resolved::types::permission::ValueExpression;

use std::collections::{BTreeMap, HashMap};

use crate::metadata::resolved::types::subgraph::QualifiedTypeReference;

use crate::schema::types::output_type::relationship::PredicateRelationshipAnnotation;

use ndc_models;

use open_dds::{arguments::ArgumentName, permissions::Role, types::FieldName};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ModelWithPermissions {
    pub model: models::Model,
    pub select_permissions: Option<HashMap<Role, SelectPermission>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum FilterPermission {
    AllowAll,
    Filter(ModelPredicate),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct SelectPermission {
    pub filter: FilterPermission,
    // pub allow_aggregations: bool,
    pub argument_presets: BTreeMap<ArgumentName, (QualifiedTypeReference, ValueExpression)>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ModelPredicate {
    UnaryFieldComparison {
        field: FieldName,
        ndc_column: String,
        operator: ndc_models::UnaryComparisonOperator,
    },
    BinaryFieldComparison {
        field: FieldName,
        ndc_column: String,
        operator: String,
        argument_type: QualifiedTypeReference,
        value: ValueExpression,
    },
    Relationship {
        relationship_info: PredicateRelationshipAnnotation,
        predicate: Box<ModelPredicate>,
    },
    And(Vec<ModelPredicate>),
    Or(Vec<ModelPredicate>),
    Not(Box<ModelPredicate>),
}
