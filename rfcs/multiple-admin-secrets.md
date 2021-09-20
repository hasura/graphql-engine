### Motivation

If Hasura provides an ability to accept multiple admin secrets, then users can implement security mechanisms like admin key rotation.

### Configuration spec

1. Introduce env var and command line arg `HASURA_GRAPHQL_ADMIN_SECRETS` that can take a comma separated list of strings (instead of a single string currently).
2. If an admin secret contains a comma, then it can be _literalized_ by putting the secret in quotes (this is as per the standard CSV spec).
   For example, HASURA_GRAPHQL_ADMIN_SECRETS: "abcd,efg", "xyz"

### Implementing rotating secrets

To implement a secret rotation mechanism, the following can be done:

1. add a new secret to list of admin secrets (and perform a rolling deploy)
2. update apps/backends to use the new secret
3. remove the old secret from the list (and perform a rolling deploy)

### Backwards compatibility

Note that we already have `HASURA_GRAPHQL_ADMIN_SECRET` env var, if both `HASURA_GRAPHQL_ADMIN_SECRET` and `HASURA_GRAPHQL_ADMIN_SECRETS` 
are given then we ignore `HASURA_GRAPHQL_ADMIN_SECRETS`. 
