# Permissions roadmap in V3

We would like to improve on these known permission issues:

## 1. Reusing parent permissions

Let me explain this with an example:

```graphql
user, user_channel, channel, message
user_channel:                     { user_id: { _eq: x-hasura-user-id } }
channel:                 { users: { user_id: { _eq: x-hasura-user-id } } }
message:      { channel: { users: { user_id: { _eq: x-hasura-user-id } } } }
```

The pattern here is as follows:

> The permission on a table is derived from the permission of its parent.

To simplify this, v3 introduces this change (something equivalent to this example):

```graphql
user_channel: { user_id: { _eq: x-hasura-user-id } }
channel:      { users: { append_permission_predicate: true } }
message:      { channel: { append_permission_predicate: true } }
```

## 2. Allowing multiple roles per request

With role inheritance in v2, we introduced a way to compose permissions. You
could treat roles as entitlements and group together entitlements.

However, a major downside of this system is that this composition has to be
done in the metadata and not in your auth system. i.e, you cannot specify
'x-hasura-role' to a set of roles without combining them in v3.

We are planning to improve the system to enable support for multiple roles to
be specified in a request.

For example, if a request's authz resolves to the following roles:

```yaml
x-hasura-role: reader, editor
```

graphql-engine would combine the permissions of both reader and editor when
processing the request.

### 2.1 Challenges:

Composing permissions this way seems quite natural. Let's see how this gets
complicated.

#### 2.1.1 No overlapping permissions
If there are no overlapping permissions for a table, the merge semantics
are straightforward.

#### 2.1.2 Overlapping permissions

i.e. permission on a table is defined for two different roles:

1. For selects:

    1. If the column sets are the same, this is easy. The 'filter' becomes
   `filter(r1)` OR `filter(r2)`.

    1. If the column sets are not the same, for example:

       ```yaml
       - role: public
         table: users
         columns: id, username
         filter: {}

       - role: private
         table: users
         columns: id, username, email
         filter:
           id: x-hasura-user-id
       ```

       What should the semantics of this query be?

       ```graphql
       query {
           users {
               id
               username
               email
           }
       }
       ```

       If we were to union the columns and the rows, the users get to see
       `email` column for all users and not just theirs.

       > The expected behaviour however is that `email` is returned as `null`
       for all other users.

       This is fairly challenging to implement, v2 uses SQL `case` expressions to
       suport this and this heavily relies on the database features. It may not be
       possible to have these correct semantics for all databases.

1. For mutations, if the presets are different, we do not know how which one to
   use.

### 2.2 Merge/Conflict strategies

In v2, permission composition happens at the metadata layer, any invalid
combination of roles would result in a failed metadata update.

Once we start allowing multiple roles per request, we need to figure out the
strategy for handling these overlapping permissions:

1. Fail the request:

   If the operation being invoked has more than one permission, we fail the
   request. This is quite straightforward but may not be the desired behaviour.

2. Provide merge semantics:

   In the metadata, instead of composing a fixed set of permissions using
   inherited roles, we could for example, allow specifying the permission
   capabilities. For example,

    ```yaml
    conflict_rules:
    # admin insert permissions should be preferred over user role's insert
    # permissions on all tables
    - admin.insert > user.insert

    # manager's update permissions should be preferred over user role's select
    # update permissions on employee_details table
    - manager.update.employee_details > user.update.employee_details
    ```
3. Pick the first role in the request:

   If the request's `x-hasura-role` is `manager, employee`, we would pick
   `manager` to execute an operation when there are conflicts.

## 3. Policy Engine

Allowing multiple roles in a request is *a solution* to improve the
expressivity of Hasura's permission system. This would put Hasura's permission
in the category of an RBAC sytem with elements from ReBAC and ABAC systems.

A general solution could exist in the form of a policy engine where we don't
take a stance on a specific permission model. If we look at Hasura's query
processing engine, it is roughly as follows:

1. Read 'x-hasura-roles' from the request body.
1. Traverse the GraphQL query and derive permissions from 'roles' obtained
   above. This will involve merging and resolving conflicts using one of the
   strategies described in 2.2.
1. Execute the operation with the permissions obtained from 2.

With a policy engine, the pipeline would change as follows:
1. Read user's session, tables and columns referenced in the query.
1. Evaluate the user defined policies with user's session, tables
   and columns referenced in the query to get a set of permissions that are
   applicable for the query.
1. Execute the operation with the permissions obtained from 2.

> The key change with the policy based system is that users can define
their own merge/conflict semantics in a policy language without Hasura
having to provide a ton of switches to customize the behaviour.

We are not yet certain what this policy language could look like. Existing
policy languages such as Rego, Cedar, Biscuit, Polar derive their roots from
Datalog and/or Prolog.

For example, the following rego policy implements an RBAC system where the
conflict resolution policy is to pick the permission of a role that is
specified first:

```ruby
package rbac.authz

import future.keywords.contains
import future.keywords.if
import future.keywords.in

# Permissions defined per role
defined_permissions := {
	"public": {"product": {
		"columns": ["id", "name"],
		"filter": {},
	}},
	"analyst": {
		"product": {
			"columns": ["*"],
			"filter": {},
		},
		"product_analytics": {
			"columns": ["*"],
			"filter": {},
		},
	},
}

# Finds all the permissions defined for a table_name
find_permissions(query_table_name) := [table_permission |
	# For each role in x-hasura-roles
	some role in input.user.x_hasura_roles

	# For each table_name, table_permission of the role
	some table_name, table_permission in defined_permissions[role]

	# Check if the permission is defined on the table
	query_table_name = table_name
]

# The permissions that are decided on
resolved_permissions[query_table_name] := resolved_permission if {
	# For each table referenced in the query
	some query_table_name in input.query.tables

	# assign permission to be returned
	resolved_permission := find_permissions(query_table_name)[0]
}
```

You can try this policy out [here](https://play.openpolicyagent.org/p/BuMPLAbuL0). If you
add the input as:

```json
{
    "query": {
        "tables": [
            "product",
            "product_analytics"
        ]
    },
    "user": {
        "x_hasura_roles": [
            "public",
            "analyst"
        ]
    }
}
```

the resolved permissions would be

```json
{
    "product": {
        "columns": [
            "id",
            "name"
        ],
        "filter": {}
    },
    "product_analytics": {
        "columns": [
            "*"
        ],
        "filter": {}
    }
}
```

If you change the ordering of the roles:

```json
{
    "query": {
        "tables": [
            "product",
            "product_analytics"
        ]
    },
    "user": {
        "x_hasura_roles": [
            "analyst",
            "public"
        ]
    }
}
```

the resolved permissions would be

```json
{
    "product": {
        "columns": [
            "*"
        ],
        "filter": {}
    },
    "product_analytics": {
        "columns": [
            "*"
        ],
        "filter": {}
    }
}
```

i.e, `analyst`'s permissions are picked over `public`'s permissions

While the above demonstrates a simple example, a language such as Rego can be quite expressive. For example,
you may not even have a notion of roles and may want to define permissions in ABAC style. One such example:

```rego
package authz.admin

import future.keywords.if
import future.keywords.in

# The permissions that are decided on
resolved_permissions[query_table_name] := resolved_permission if {
	# For each table referenced in the query
	some query_table_name in input.query.tables

	# if the user is admin
	input.user.is_admin = true

	# assign permission to be returned
	resolved_permission := {
		"columns": ["*"],
		"filter": {},
	}
}
```

You can experiment with the policy
[here](https://play.openpolicyagent.org/p/DrTGxLJuFM). Try changing the
`is_admin` attribute to `false` and check the returned permissions.
