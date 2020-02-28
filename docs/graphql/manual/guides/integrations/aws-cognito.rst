.. meta::
   :description: Use AWS Cognito for authentication with Hasura
   :keywords: hasura, docs, guide, authentication, auth, integration

.. _aws_cognito:

Using AWS Cognito for authentication
====================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Cognito can be used in both :doc:`JWT mode <../../auth/authentication/jwt>` and :doc:`webhook mode <../../auth/authentication/webhook>`.

- For JWT, `this blog post <https://hasura.io/blog/hasura-authentication-explained/#cognito>`__ contains detailed instructions.
- For webhook, a thin wrapper/webhook that implements `this spec <https://hasura.io/blog/1.0/graphql/manual/auth/authentication/webhook.html#spec-for-the-webhook>`__ needs to be written. Boilerplates to write this webhook can be found `here <https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/auth-webhooks>`__.
