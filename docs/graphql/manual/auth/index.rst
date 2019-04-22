Authentication & Authorization
==============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

In Hasura, access control or authorization is based on roles. Let's take a look at how this works when GraphQL engine receives a request:

.. thumbnail:: ../../../img/graphql/manual/auth/auth-high-level-overview.png

As you can see from this:

- **Authentication** is handled outside Hasura. Hasura delegates authentication and resolution of request headers into session variables to your existing (*or new/custom*) authentication service.

- Your authentication service is required to pass a user's **role** information in the form of session variables like ``X-Hasura-Role``, etc. More often than not, you'll also need to pass user information for your use-cases.

- For **Authorization** or **Access Control**, Hasura helps you define granular role-based access control rules for every field in your GraphQL schema i.e. granular enough to control access to any row or column in your database.

- Hasura uses the role/user information in the session variables and the actual query itself to validate the query against the rules defined by you. If the query/operation is allowed, it generates a SQL query, which includes the row/column-level constraints from the access control rules, and sends it to the database to perform the required operation (*fetch the required rows for queries, insert/edit rows for mutations, etc.*).


Authentication - Overview
-------------------------
Hasura supports two modes of authentication configuration:

1) **Webhook**: Your auth server exposes a webhook that is used to authenticate all incoming requests to the Hasura GraphQL engine server and to get metadata about the request to evaluate access control rules. Here's how a GraphQL request is processed in Webhook mode:

.. thumbnail:: ../../../img/graphql/manual/auth/auth-webhook-overview.png

2) **JWT** (JSON Web Token): Your auth server issues JWTs to your client app, which, when sent as part of the request, are verified and decoded by GraphQL engine to get metadata about the request to evaluate access control rules. Here's how a GraphQL query is processed in JWT mode:

.. thumbnail:: ../../../img/graphql/manual/auth/auth-jwt-overview.png

Authorization - Overview
------------------------

Hasura supports role-based authorization where access control is done by creating rules for each role, table and operation(like *insert*, *update*, etc.). These access control rules use dynamic session variables that are passed to GraphQL engine from your authentication service (*webhook or JWT*) with every request. Role information is inferred from the ``X-Hasura-Role`` and ``X-Hasura-Allowed-Roles``. Other session variables can be passed by your auth as per your requirements.

**For example:**

.. thumbnail:: ../../../img/graphql/manual/auth/hasura-perms.png
   :width: 80 %

Getting started with Auth & Access Control in Hasura
----------------------------------------------------

.. admonition:: Trying access control out

   If you just want to see role-based access control in action, you need not set up or integrate your auth service with GraphQL Engine. You can just:
   
   * Define permission rules for a table for a role. See :doc:`access control basics<basics>`.
   
   * Use the GraphiQL interface in the console to make a request and send the session variables as request headers (*send a* ``X-Hasura-Role`` *key, with its value as the name of the role you've defined rules for*). The data in the response will be restricted as per your definition.

   While this is not a mode of authentication and is highly **NOT RECOMMENDED in production**, you can secure your GraphQL endpoint while trying Hasura out or development by :doc:`configuring an admin secret key <../deployment/securing-graphql-endpoint>`.

Here's how we would recommend you go about setting up auth and access control:

1) Check out this :doc:`overview of access control<basics>` and, if and when required, see the :doc:`reference documentation for permissions<reference-permissions>`.

2) Go through :doc:`access control examples<common-roles-auth-examples>`.

3) Read more about :doc:`roles and session variables<roles-variables>`

4) Check out the :doc:`basics of modeling roles<modeling-roles>`.

5) Configure authentication in either :doc:`webhook<webhook>` or :doc:`JWT<jwt>` mode.



**See:**

.. toctree::
   :maxdepth: 1

   basics
   reference-permissions
   common-roles-auth-examples
   modeling-roles
   roles-variables
   webhook
   webhook-examples
   jwt
   jwt-examples
   
