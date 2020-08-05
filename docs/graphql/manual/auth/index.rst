.. meta::
   :description: Manage GraphQL Authentication and Authorization with Hasura
   :keywords: hasura, docs, authentication, auth, authorization

.. _auth:

Authentication & Authorization
==============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

In Hasura, access control or authorization is based on **roles**. Let's take a look at how this works
when the GraphQL engine receives a request:

.. thumbnail:: /img/graphql/manual/auth/auth-high-level-overview.png
   :alt: Authentication and authorization with Hasura

As you can see from this:

- **Authentication** is handled outside of Hasura. Hasura delegates authentication and resolution of request
  headers into session variables to your authentication service *(existing or new)*.

  Your authentication service is required to pass a user's **role** information in the form of session
  variables like ``X-Hasura-Role``, etc. More often than not, you'll also need to pass user information
  for your access control use cases, like ``X-Hasura-User-Id``, to build permission rules.

- For **Authorization** or **Access Control**, Hasura helps you define granular role-based access control
  rules for every field in your GraphQL schema *(granular enough to control access to any row or
  column in your database)*.

  Hasura uses the role/user information in the session variables and the actual request itself to validate
  the request against the rules defined by you. If the request/operation is allowed, it generates an SQL
  query, which includes the row/column-level constraints from the access control rules, and sends it to
  the database to perform the required operation (*fetch the required rows for queries, insert/edit
  rows for mutations, etc.*).

**See more details about setting up authentication and access control at:**

.. toctree::
  :maxdepth: 1

  authentication/index
  authorization/index
