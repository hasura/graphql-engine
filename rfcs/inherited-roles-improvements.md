# Inherited Roles Improvements Tech Spec


Product specs: https://github.com/hasura/graphql-engine-mono/blob/rfc/inherited-roles-improvements/rfcs/inherited-roles-improvements.md

https://docs.google.com/document/d/1KoCT6DsUDYbTd3sasO_jqp-sWme7em0zgzUTA6tXHWU

Deriving permissions for inherited roles can be broken down in the following cases:

### 1. All parent roles have exactly the same permission

In this case, the permission of the inherited role will be the permission of the parent role.

### 2. Some parent roles don't have permissions defined

If the roles which have permissions are equal, then the permission of the inherited role will be the permission of the parent role (which have a permission defined)

### 3. When permissions are not equal across parent roles (future work)

There are two cases here:

1. If two permissions have different presets/check, then the permission for the inherited role is not derived
2. When the presets are the same (or doesn't exist), but other parameters are different, say the columns permitted for the user to insert data into. TODO: figure out what to do in this case

## Implementation:

Before the improvements to inherited roles, the inherited roles only worked with select permissions, the inherited roles mutations permissions are explicitly set to `Nothing` [here](https://github.com/hasura/graphql-engine-mono/blob/328e28f521f6e5fa20489aaeea2be1f1cc456d56/server/src-lib/Hasura/RQL/DDL/Schema/Cache/Permission.hs#L191). That will now be changed to a permission in appropriate cases.

For remote schema permissions, earlier remote schema permissions were only built for roles which has a remote schema permission defined, now that will be changed to building remote schemas and their permissions for inherited roles as well.

For actions, action's permissions can be thought of as boolean values (when defined it will be `true` and when not defined it will be `false`).  So, unless all the parent roles don't have a permission defined on the action, the inherited role will be allowed to access the action.

For custom functions, pre inherited roles improvements, when inferring of function permissions from the table permissions is enabled (default), the function will be accessible to the inherited role (assuming at-least one of the parent roles had a select permission defined) and when inferring of function permissions was disabled the function was not exposed to the inherited role even if its parent roles had function permissions defined for the function. In the inherited roles improvements PR, the behaviour when function permissions are inferred will remain the same and when inferring of function permissions are disabled, the permission of the inherited role will be derived like actions.

Currently, permissions **cannot** be set for an inherited role, this is done via a validation check. In the improvements PR, this validation shall be removed so that explicit permission(s) can be set for an inherited role, in case of select permissions for an inherited role where the permissions are always derived the permission set by the user will override the inherited permission.

From product spec,

> After this, there is no distinction between a role and an inherited role - a role can have zero or more parent roles. However a cycle should not be allowed. The permissions of a role are as follows:
>
> 1. If a role has no permission defined for some entity and the role does not inherit from any other roles, then the role does not have permission on that entity.
> 2. If a role has no permission defined for some entity but the role inherits from one or more roles, the permission is inferred from the parent roles. It is possible that we may not be able to infer permission from the parent roles.
> 3. If a role has a permission defined on some entity, that becomes the permission, i.e, it overrides any inferred permissions.

Since, now non-inherited roles and inherited roles are unified, the way table permissions are built for roles will be modified. There will be no changes in the way non-inherited role table permissions are built but there will be a change required for the way we build inherited role table permissions, because an inherited role may be dependent on other inherited roles which will require that to first build the permissions of the dependent role's permissions to build the permission of the current inherited role.

Let's assume there are five roles, namely:

1. role1 - non-inherited
2. role2 - non-inherited
3. inherited_role1 -> role1 + role2
4. inherited_role2 -> inherited_role1 + role2
5. inherited_role3 -> inherited_role1 + inherited_role3


If the roles were to be built in the above order, then there will be no issues since all the dependencies of an inherited role is already being built when required, but unfortunately this won't be the case in the real world because in the real world, we cannot assume the order of the permissions to be in this manner. Also, we need to detect if there are any cycles in the inherited roles.

The following function is proposed to order the inherited roles according to their dependencies i.e.
the parent roles of an inherited role will precede it in the ordering. Following the above example:

The ordered inherited roles should be ordered in this manner ``[role1, role2, inherited_role1, inherited_role2, inherited_role3]``.
The only order that's important is that the parent sdof the inherited roles should precede the inherited role.

The ordering of the roles can be done by the function (stronglyConnComp)[https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Graph.html#v:stronglyConnComp], this function
also returns if there are any cycles in the roles.

### Modelling of Permissions

```haskell
data CheckPermissions permissionType = Undefined | Defined permissionType | Inconsistent

instance (Eq permissionType) => Semigroup (CheckPermissions permissionType) where
  Undefined <> x = x
  x <> Undefined = x
  Inconsistent <> x = Inconsistent
  x <> Inconsistent = Inconsistent
  Defined e1 <> Defined e2
    | e1 == e2 = Defined e1
    | otherwise = Inconsistent

```

The above model can be used to combine mutation permissions and remote schema permissions.

For actions and function permissions, the ``Any`` monoid can be used to derive the permission of the inherited role.

## Conflicts while inheriting permissions

While inheriting a mutation or remote schema permission, a conflict may occur when there's a conflict, a conflict
maybe caused when inheriting permission from two parent roles if the relevant parts cannot be combined. For example,
if the preset of the ``check`` argument of two permissions are different then it's a conflict.

When a conflict occurs while inheriting permission for a role, the ``(role, entity)`` combination will be set as
inconsistent in the metadata. This info can be retrieved by the ``get_inconsistent_metadata`` API. This inconsistency
can be resolved by setting the permission of the conflicting ``(role, entity)`` combination.

NOTE: When the ``drop_inconsistent_metadata`` is called then these conflicts will not be dropped because there is nothing
there to drop in the metadata which will resolve this inconsistency, we can drop the inherited role itself to resolve the inconsistency,
but it's not a solution because other permissions exposed by the inherited role maybe consistent and we would not like to drop them.

## Open Questions

1. How to explicitly set permission to "no permission" when there is a conflict while deriving
   permissions for an inherited role? See "Conflicts while inheriting permissions" - product team
2. What happens when an inherited role is dropped? We'll need to track all the dependent roles of a role, the problem is that roles are an implicit part of the metadata and tracking dependencies for explicit things are easier. For example
   example: a remote relationship is dependent on its remote schema, so we know that when the remote schema is dropped, if the remote relationship exists then we need to make the metadata inconsistent, in this case the dropping of
   the remote schema being the trigger to check its dependencies but it's not the case with roles because a role will be deleted only when no permission (table/remote/function/action) uses the role.
3. Currently, the select permission of an inherited role cannot be expressed in the current select permission metadata syntax because it doesn't account for the column(s) being conditionally present depending on the row filter. TODO (future work) - product team

## Future work

1. We should expose an API about the roles metadata which should give details like
    1. How a role's permissions is inherited (if inherited) and what does the inherited permission look like?

    For non-select permissions this is straight-forward because the inherited permission when inherited successfully will be permission
    of one of the parent role, but in case of select permissions, the inherited permission may not be like one of the parent role permissions
    and currently there's no way to express it in the current metadata select permission syntax
