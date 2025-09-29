use indexmap::IndexMap;
use open_dds::views::ViewName;
use serde::{Deserialize, Serialize};

use crate::stages::views;
use crate::types::subgraph::Qualified;
use crate::{AllowOrDeny, ConditionHash};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ViewWithPermissions {
    pub view: views::ResolvedView,
    pub permissions: ViewPermissions,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ViewPermissions {
    pub authorization_rules: Vec<ViewAuthorizationRule>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ViewAuthorizationRule {
    Access {
        condition: Option<ConditionHash>,
        allow_or_deny: AllowOrDeny,
    },
}

impl ViewPermissions {
    pub fn new() -> Self {
        Self {
            authorization_rules: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.authorization_rules.is_empty()
    }
}

impl Default for ViewPermissions {
    fn default() -> Self {
        Self::new()
    }
}

/// The output of the view permissions stage.
pub struct ViewPermissionsOutput {
    pub permissions: IndexMap<Qualified<ViewName>, ViewWithPermissions>,
}
