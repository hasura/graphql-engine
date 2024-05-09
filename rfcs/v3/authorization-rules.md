# Authorization Rules in V3

## Motivation

Currently, Hasura uses role based access control, where you define the entire set of permissions per-role. However, this doesn't scale well because:
- It's not always possible to capture all possible states in the authorization system as separate roles.
- It's not possible to reuse permissions (allowed fields, model predicate) across roles.
- For complicated permissions, it's hard to verify the correctness of a model's permissions predicate at a glance.

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

The condition must start at a boolean operator. These are the available boolean operators:
- `and`: array of boolean expressions which are ANDed together.
- `or`: array of boolean expressions which are ORed together.
- `not`: boolean exrepssion which is negated.
- `equal`: Compares two value expressions are the same.
- `contains`: Whether a given value is contained within a given array of values
- `greaterThan`: Compares left > right.
- `lessThan`: Compares left < right.
- `greaterThanOrEqual`: Compares left >= right
- `lessThanOrEqual`: Compares left <= right

In the future, these may be expanded to more boolean operators. For example:
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
            runCommand:
              name: is_valid_recommendation_query
              arguments:
                - name: query_to_validate
                  value:
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
```
allowObjects:
  relationship:
    name: 'my_relationship'
    relatedObjectAllowed: true
```


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
