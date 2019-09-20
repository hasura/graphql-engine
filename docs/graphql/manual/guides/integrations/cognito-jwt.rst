.. _cognito_jwt:

Using AWS Cognito for authentication in JWT mode
================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

You can use Cognito in both JWT mode and webhook mode.

- For JWT, take a look at this blog post: https://blog.hasura.io/hasura-authentication-explained/#cognito.
- For webhook, you’ll need to write a thin wrapper/webhook that implements the spec in https://docs.hasura.io/1.0/graphql/manual/auth/authentication/webhook.html#spec-for-the-webhook. You can  also use boilerplates from https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/auth-webhooks to write this webhook.