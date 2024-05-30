# Authorization Rules in V3

## Motivation

Currently, Hasura uses role based access control, where you define the entire set of permissions per-role. However, this doesn't scale well because:
- It's not always possible to capture all possible states in the authorization system as separate roles.
- It's not possible to reuse permissions (allowed fields, model predicate) across roles.
- For complicated permissions, it's hard to verify the correctness of a model's permissions predicate at a glance.

Some Github issues addressed in this RFC:
- https://github.com/hasura/graphql-engine/issues/3685
- https://github.com/hasura/graphql-engine/issues/1919
- https://github.com/hasura/graphql-engine/issues/3544

## Proposal

This RFC proposes a new rule-based mechanism for defining permissions for types, models, and commands in the metadata. The following requirements have been considered as a part of this proposal:
- Composability: The ability to define multiple simple rules that compose automatically instead of one big expression.
- Performance: The time it takes to evaluate the permissions for a particular GraphQL request should only depend on the metadata objects being queried and not on the total size of the metadata.
- Validation: We should be able to statically validate any references to the entities in the metadata (eg: fields, relationships, commands, etc.).

For a given resource (type / model / command), instead of defining permissions for a given role, you would define multiple rules, where each rule contains:
- One or more composable permission primitives corresponding to the resource.
- A condition which must evaluate to true at runtime for this rule to apply.

The final permissions for that resource are a composition of the permission primitives defined across rules whose condition evaluated to true.

### Condition

The condition that may be attached to a rule is a boolean expression built using the following primitives.

#### Boolean Operators

The condition must start at a boolean operator. These are some potential boolean operators:
- `and`: array of boolean expressions which are ANDed together.
- `or`: array of boolean expressions which are ORed together.
- `not`: boolean exrepssion which is negated.
- `equal`: Compares two value expressions are the same.
- `isNull`: whether a given value is null
- `contains`: Whether a given value is contained within a given array of values
- `greaterThan`: Compares left > right.
- `lessThan`: Compares left < right.
- `greaterThanOrEqual`: Compares left >= right
- `lessThanOrEqual`: Compares left <= right
- `regexMatch`: Whether a given value matches the given regular expression

#### Value Expressions

The left and right side of a comparison expression are value expressions which can either be:
- Literals
- Session Variable references

In the future, these may be expanded to:
- The output from an invoked command
- The result of invoking an external policy engine like Open Policy Agent (OPA).

#### Example

Here is a rule that descriebs the condition to enable an authorization rule if the user is an admin or is an unbanned user in the PRO tier.

```yaml
or:
  - and:
    - equal:
        left:
          sessionVariable: customer_tier
        right:
          literal: PRO_TIER
    - not:
        equal:
          left:
            sessionVariable: is_banned
          right:
            literal: true
  - equal:
      left:
        sessionVariable: is_admin
      right:
        literal: true
```

### Type Permissions

The following permission primitives are available when defining a permissions rule for a type.
- Allow access to certain fields
- Deny access to certain fields

If a field is both allowed and denied, it will be denied.

#### Example

```yaml
kind: TypePermissions
version: v2
definition:
  typeName: Order
  authorizationRules:
    # Rule 1: Allow all fields by default
    - allowFields: "*"
      condition:
        literal: true
    # Rule 2: Deny internal_order_notes to non-admins. Deny always takes precedence over allow.
    - denyFields:
        - internal_order_notes
      condition:
        not:
          equals:
            left: sessionVariable: x-hasura-role
            right: literal: admin
```

### Model Permissions

The following permission primitives are available when defining a permissions rule for a model.
- Allow access to certain objects or "rows" (defined by a predicate)
- Deny access to certain objects or "rows" (defined by a predicate)

If an object/row is both allowed and denied, it will be denied.

#### Example

```yaml
kind: ModelPermissions
version: v2
definition:
  modelName: Orders
  authorizationRules:
    # Rule 1: Allow users to see their own orders
    - allowObjects:
        fieldComparison:
          fieldName: user_id
          operator: _eq
          value:
            sessionVariable: x-hasura-user-id
      condition:
          equals:
            left: sessionVariable: x-hasura-role
            right: literal: user
    # Rule 2: Disallow anyone except admins to see hidden orders
    - denyObjects:
        fieldComparison:
          fieldName: is_hidden
          operator: _eq
          value: 
            literal: true
      condition:
        not:
          equals:
            left: sessionVariable: x-hasura-role
            right: literal: admin
```

### Command Permissions

The following primitives are available when defining command permissions:
- Allow / deny command execution. Deny takes precedence over allow.
- Preset a command argument to a fixed value computable at request time.
  - For boolean expression arguments, presets can be composed by using `includeBooleanExpression`, `excludeBooleanExpression` and the final preset is a merge of all applicable rules.
  - For non-boolean expression arguments, if there are multiple rules that are enabled, and both use specify presets, then the rule defined later in the order takes precedence.
- Validation of command inputs. If there are multiple rules with different validations, all validations will be enforced.

#### Example

```yaml
kind: CommandPermissions
version: v2
definition:
  # Command takes two inputs, "query" and "limit". 
  commandName: GetProductRecommendations
  authorizationRules:
    # Rule 1, all non-anonymous roles can execute the command
    - allowExecution: true
      condition:
        not:
          equals:
            left: sessionVariable: x-hasura-role
            right: literal: anonymous
    # Rule 2, maximum 5 recommendations allowed for basic_tier
    - presetArguments:
        - argumentName: limit
          value:
            literal: 5
      condition:
        # Information about whether the user has access to the basic tier is available in OPA
        # This is meant to illustrate how external systems might be brought in and calling OPA
        # is not actually being proposed in this RFC.
        equals:
          - opa:
            path: /v1/data/product_reommendations/basic_tier
            input:
              - fieldName: user_id
                value:
                  sessionVariable: x-hasura-user-id
          - literal: true
    # Rule 3, maximum 10 recommendations allowed for pro_tier
    - presetArguments:
        - argumentName: limit
          value:
            literal: 10
      condition:
        # Information whether the user has access to the pro tier is available in OPA
        # This is meant to illustrate how external systems might be brought in and calling OPA
        # is not actually being proposed in this RFC.
        equals:
          - left:
              opa:
              path: /v1/data/product_reommendations/pro_tier
              input:
                - fieldName: user_id
                  value:
                    sessionVariable: x-hasura-user-id
          - right: literal: true
    # Rule 4, run a custom command to validate the "query" argument
    - validate: 
        equals:
          left:
            # This is meant to illustrate how an external command may be invoked
            # and is not actually being proposed in this RFC.
            runCommand:
              name: is_valid_recommendation_query
              arguments:
                - name: query_to_validate
                  value:
                    # Name of the command argument to validate
                    fromInputArgument: query
          right:
            literal: true

kind: CommandPermissions
version: v2
definition:
  commandName: DeleteReviews
  authorizationRules:
    # Rule 1: Allow users to only delete their own reviews
    - presetArguments:
      - argumentName: restriction
        value:
          includeBooleanExpression:
            fieldComparison:
              fieldName: user_id
              operator: _eq
              value:
                sessionVariable: x-hasura-user-id
      condition:
          equals:
            left: sessionVariable: x-hasura-role
            right: literal: user
    # Rule 2: Do not allow anyone except admins to delete flagged reviews
    - presetArguments:
      - argumentName: restriction
        value:
          excludeBooleanExpression:
            fieldComparison:
              fieldName: is_flagged
              operator: _eq
              value:
                literal: true
      condition:
          not:
            equals:
              left: sessionVariable: x-hasura-role
              right: literal: admin
```

### Reusing Relationship Permissions

In the existing authorization system, the entire permissions predicate needs to be specified for a model's permission. For related models, however, this can result in a duplicate and very nested permission predicates.

For example, if I have models for `Channels`, `Threads`, `Replies`. Then my predicates would be as follows:
- `Channels`: `members._contains(x-hasura-user-id)`
- `Threads`: `channel.members._contains(x-hasura-user-id`
- `Replies`: `thread.channel.members._contains(x-hasura-user-id)`.

Ideally, you would be able to express the fact that an object should be selectable if the related object is also selectable.

Hence, we propose the following addition to the permissions predicate:
```yaml
allowObjects:
  relationship:
    name: 'my_relationship'
    relatedObjectAllowed: true
```

### AuthConfig

The notion of roles will be removed from AuthConfig. In particular, the `allow_role_emulation_by` key will be replaced by a `allow_session_emulation_if` key which specifies a condition which needs to evaluate to true for the request to allow emulating session variables.

No `x-hasura-` session variables will now be required in the webhook response or in JWT claims. Users can choose arbitrary names for session variables.

### Implementation

The above proposal has major implications for engine, the `lang-graphql` crate in particular. Currently, `lang-graphql` allows constructing a single graphql schema with information about all roles embedded into the schema and allows query validation / IR generation to happen in the context of a particular role only. However, with the above proposal, there is no notion of roles anymore. So, `lang-graphql` will need to move to an architecture where when trying to access a graphql field, instead of looking up an allowed set of roles, it can evaluate an arbitrary expression that evaluates to a boolean. The details of this architecture are out of scope for this RFC.

#### Milestones

- Milestone 1: Refactor the internal engine implementation to support the new permissions system without any metadata changes. Internally, the role-based definitions would be mapped to the new system.
- Milestone 2: Make the metadata changes for the new system with the following primitives:
  - Conditional activation of rules. Conditions support a basic set of operators (`equals`, `isNull`, and logical operators at least).
  - Value expressions can either be literals or session variables only.
  - Support only field permissions / model select predicates / argument presets. No input validation.
  - Composability of rules.
- New features (in no particular order):
  - Reusing relationship permissions
  - Arguments input validation
  - Calling external systems
  - Accessing sub-fields in value expressions
  - Advanced boolean operators

## FAQs

1. **Will existing metadata continue to be work?**  
    
    Yes, existing metadata (using the `v1` version of permissions objects) will continue to work and trivially maps to the new system where the condition is an equality check on the `x-hasura-role` session variable.

2. **Does this subsume the inherited roles feature in v2?**
    
    Yes, this is the intended replacement for inherited roles in v2. So, instead of defining multiple base roles and an inherited you would simply define multiple rules which can activated by arbitrary conditions.

    Note, however, that cell-level authorization will not be supported by this system, since you define type (column) permissions and model (row) permissions independently.

3. **Are any changes required to the NDC protocol?**
   
   No.

4. **Are any changes required to the user's authentication system?**

   No changes are required to existing authentication systems. However, to fully utilize the power of authorization rules you would want to inject more granular claims (instead of simply roles) during authentication.

## Before / After comparison

Imagine the following data models for an internal API targeted at a company's own developers and support agents.

- User: id, name, email, is_gov
- UserActivityRecord: id, user_id, details, is_hidden
- SupportTickets: id, assigned_agent_id

And these are the potential authorization considerations:

- Two personas: Developers and Support Agents
  - Developers can generally view all user's information
  - Support agents can only view data for users whose tickets they have been assigned
- Some user information is protected
  - PII fields (name, email) unless the API consumer has explicit access
  - Government user data is restricted unless the API consumer has explicit access

### Before

You would need to define the following roles and provision them appropriately in the authentication system.

- developer_base
- developer_with_pii_access
- developer_with_gov_access
- developer_with_pii_access_and_gov_access
- support_agent_base
- support_agent_with_pii_access
- support_agent_with_gov_access
- support_agent_with_pii_access_and_gov_access

And the permissions would be very hard to manage. Eg:

```yaml
kind: TypePermission
version: v1
definition:
  typeName: User
  roles:
    - role: developer_base
      output:
        allowedFields: [id]
    - role: developer_with_pii_access
      output:
        allowedFields: [id, name, email]
    - role: developer_with_gov_access
      output:
        allowedFields: [id]
    - role: developer_with_pii_access_and_gov_access
      output:
        allowedFields: [id, name, email]
    - role: support_agent_base
      output:
        allowedFields: [id]
    - role: support_agent_with_pii_access
      output:
        allowedFields: [id, name, email]
    - role: support_agent_with_gov_access
      output:
        allowedFields: [id]
    - role: support_agent_with_pii_access_and_gov_access
      output:
        allowedFields: [id, name, email]
```

The model permissions for User and UserActivity would be massive. For example:

```yaml
kind: ModelPermissions
version: v1
definition:
  modelName: User
  roles:
    - role: developer_base # Same for developer_with_pii_access
      select:
        filter:
          not:
            fieldComparison:
              fieldName: is_gov
              operator: _eq
              value: literal: true
    - role: developer_with_gov_access # Same for developer_with_pii_access_and_gov_access
      select:
        filter: null
    - role: support_agent_base # Same for support_agent_with_pii_access
      select:
        filter:
          and:
            - relationship:
                name: tickets
                predicate:
                  fieldComparison:
                    field: agent_id
                    operator: _eq
                    value: sessionVariable: x-hasura-agent-id
            - not:
                fieldComparison:
                  fieldName: is_gov
                  operator: _eq
                  value: literal: true
      - role: support_agent_with_gov_access # Same for support_agent_with_pii_access_and_gov_access
        select:
          filter:
            relationship:
              name: tickets
              predicate:
                fieldComparison:
                  field: agent_id
                  operator: _eq
                  value: sessionVariable: x-hasura-agent-id

kind: ModelPermissions
version: v1
definition:
  modelName: UserActivity
  roles:
    - role: developer_base # Same for developer_with_pii_access
      select:
        filter:
          relationship:
            name: user
            predicate:
              not:
                fieldComparison:
                  fieldName: is_gov
                  operator: _eq
                  value: literal: true
    - role: developer_with_gov_access # Same for developer_with_pii_access_and_gov_access
      select:
        filter: null
    - role: support_agent_base # Same for support_agent_with_pii_access
      select:
        filter:
          and:
            - relationship:
                name: tickets
                predicate:
                  fieldComparison:
                    field: agent_id
                    operator: _eq
                    value: sessionVariable: x-hasura-agent-id
            - relationship:
                name: user
                not:
                  fieldComparison:
                    fieldName: is_gov
                    operator: _eq
                    value: literal: true
      - role: support_agent_with_gov_access # Same for support_agent_with_pii_access_and_gov_access
        select:
          filter:
            relationship:
              name: tickets
              predicate:
                fieldComparison:
                  field: agent_id
                  operator: _eq
                  value: sessionVariable: x-hasura-agent-id
```

### After

The authentication system would be simple.
Two roles:
- developer
- support_agent

Some extra session variables injected based on access
- has_access_to_pii
- has_access_to_gov

```yaml
kind: TypePermissions
version: v2
definition:
  typeName: User
  authorizationRules:
    # Rule 1: Allow id field for everyone
    - allowFields: [id]
    # Rule 2: Allow name and email if pii access is allowed
    - allowFields: [name, email]
      condition:
        equals:
          left: sessionVariable: x-hasura-has-pii-access
          right: literal: true

kind: ModelPermissions
version: v2
definition:
  modelName: User
  authorizationRules:
    # Rule 1: Developers are allowed to access all users by default
    - allowObjects: "*"
      condition:
        equals:
          left: sessionVariable: x-hasura-role
          right: literal: developer
    # Rule 2: Support agents are allowed to access users they have support
    # tickets for.
    - allowObjects:
        relationship:
          name: tickets
          predicate:
             fieldComparison:
               field: agent_id
               operator: _eq
               value: sessionVariable: x-hasura-agent-id
        condition:
          equals:
            left: sessionVariable: x-hasura-role
            right: literal: support_agent
    # Rule 3: If a developer / support_agent does not have gov access,
    # don't allow them to access gov users.
    - denyObjects:
       fieldComparison:
         fieldName: is_gov
         operator: _eq
         value: literal: true
       condition:
         not:
           equals:
             left: sessionVariable: x-hasura-has-gov-access
             right: literal: true

kind: ModelPermissions
version: v2
definition:
  modelName: UserActivity
  authorizationRules:
    # Rule 1: By default allow access to user activity if someone has access to the user itself
    - allowObjects:
        relationship:
          name: user
          relatedObjectAllowed: true
    # Rule 2: Disallow activity that is explicitly hidden
    - denyObjects:
        fieldComparison:
          fieldName: is_hidden
          operator: _eq
          value: literal: true
```