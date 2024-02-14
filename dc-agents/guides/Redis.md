# Hasura + Redis

Redis:

1. Super low-latency reads

Hasura + Redis:

1. Ignore models entirely.
2. Support functions that allow getting values from redis with authz on the input "key" and output "value". Output "values" can have types and will hence show up in the GraphQL schema properly.
