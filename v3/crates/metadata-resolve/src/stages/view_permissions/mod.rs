mod types;
use hasura_authn_core::SESSION_VARIABLE_ROLE;
use indexmap::IndexMap;

use open_dds::authorization::{Comparison, Condition};
use open_dds::identifier::SubgraphName;
use open_dds::permissions::ValueExpression;
use open_dds::views::ViewName;

use crate::stages::{type_permissions::resolve_condition, views};
use crate::types::error::Error;
use crate::types::subgraph::Qualified;
use crate::{AllowOrDeny, Conditions};

pub use types::{
    ViewAuthorizationRule, ViewPermissions, ViewPermissionsOutput, ViewWithPermissions,
};

/// resolve view permissions
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    views: &IndexMap<Qualified<ViewName>, views::ResolvedView>,
    conditions: &mut Conditions,
) -> Result<ViewPermissionsOutput, Vec<Error>> {
    let mut views_with_permissions: IndexMap<Qualified<ViewName>, ViewWithPermissions> =
        IndexMap::new();

    // Initialize all views with empty permissions
    for (view_name, view) in views {
        views_with_permissions.insert(
            view_name.clone(),
            ViewWithPermissions {
                view: view.clone(),
                permissions: ViewPermissions::new(),
            },
        );
    }

    let mut results = vec![];

    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: permissions,
    } in &metadata_accessor.view_permissions
    {
        results.push(resolve_view_permission(
            subgraph,
            permissions,
            conditions,
            &mut views_with_permissions,
            &metadata_accessor.flags,
        ));
    }

    partition_eithers::collect_any_errors(results).map(|_| ViewPermissionsOutput {
        permissions: views_with_permissions,
    })
}

fn resolve_view_permission(
    subgraph: &SubgraphName,
    permissions: &open_dds::permissions::ViewPermissions,
    conditions: &mut Conditions,
    views_with_permissions: &mut IndexMap<Qualified<ViewName>, ViewWithPermissions>,
    flags: &open_dds::flags::OpenDdFlags,
) -> Result<(), Error> {
    let open_dds::permissions::ViewPermissions::V1(view_permissions_v1) = permissions;

    let qualified_view_name =
        Qualified::new(subgraph.clone(), view_permissions_v1.view_name.clone());

    let view_with_permissions = views_with_permissions
        .get_mut(&qualified_view_name)
        .ok_or_else(|| Error::UnknownView {
            view_name: qualified_view_name.clone(),
        })?;

    match &view_permissions_v1.permissions {
        open_dds::permissions::ViewPermissionOperand::RoleBased(role_permissions) => {
            // TODO: warn on duplicate roles

            for role_permission in role_permissions {
                let resolved_rule = ViewAuthorizationRule::Access {
                    condition: Some(conditions.add(resolve_condition(
                        &Condition::Equal(Comparison {
                            left: ValueExpression::SessionVariable(SESSION_VARIABLE_ROLE),
                            right: ValueExpression::Literal(serde_json::Value::String(
                                role_permission.role.to_string(),
                            )),
                        }),
                        flags,
                    ))),
                    allow_or_deny: if role_permission.allow {
                        AllowOrDeny::Allow
                    } else {
                        AllowOrDeny::Deny
                    },
                };
                view_with_permissions
                    .permissions
                    .authorization_rules
                    .push(resolved_rule);
            }
        }
        open_dds::permissions::ViewPermissionOperand::RulesBased(authorization_rules) => {
            for authorization_rule in authorization_rules {
                let resolved_rule = match authorization_rule {
                    open_dds::authorization::ViewAuthorizationRule::Allow(allow) => {
                        ViewAuthorizationRule::Access {
                            condition: allow.condition.as_ref().map(|condition| {
                                conditions.add(resolve_condition(condition, flags))
                            }),
                            allow_or_deny: AllowOrDeny::Allow,
                        }
                    }
                    open_dds::authorization::ViewAuthorizationRule::Deny(deny) => {
                        ViewAuthorizationRule::Access {
                            condition: Some(
                                conditions.add(resolve_condition(&deny.condition, flags)),
                            ),
                            allow_or_deny: AllowOrDeny::Deny,
                        }
                    }
                };
                view_with_permissions
                    .permissions
                    .authorization_rules
                    .push(resolved_rule);
            }
        }
    }

    Ok(())
}
