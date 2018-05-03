========================
GraphQL Custom Resolvers
========================

The Hasura platform allows you to deploy custom code as microservices with a git push. Your microservices can act as a graphql proxy where you can add custom resolvers and then connect to Hasura GraphQL engine.

However, proxying Hasura GraphQL with a custom microservice is not recommended due to the following reasons:

1. All the customization can be implemented within Postgres as views if the data-modelling is apt. Check :ref:`data-modelling guide <data-modelling>` for more about data-modelling.
2. You will have to rewrite the access control layer for your microservice which is already implemented in Hasura GraphQL.
3. You have to write more code when you don't need to.

In case your custom code does not connect with the Postgres database and you want to just expose an API for some third party API, then Deploying your graphql server as a microservice is the best choice available and is as easy as deploying your graphql server as a microservice on the Hasura cluster.
