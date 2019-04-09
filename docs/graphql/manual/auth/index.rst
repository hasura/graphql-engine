Authentication & Authorization
==============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

The following is the life-cycle of a GraphQL query to Hasura:

<Client->Query->Authn->Authz/Permissions->Postgres->Response>

As you can see from this diagram:

- **Authentication** is handled outside Hasura and Hasura can be configured to work with your existing (*or new/custom*) Authentication system.

- For **Authorization** or **Access Control**, Hasura helps you define granular role-based access control rules for every field in your GraphQL schema i.e. every row or column in your database.


Authentication - Overview
-------------------------
Hasura supports two modes of authentication configuration:

1) **Webhook**: Your auth server exposes a webhook that is used to authenticate all incoming requests to the Hasura GraphQL engine server and to get metadata about the request to evaluate access control rules. The life-cycle of a GraphQL query, modified for **Webhook** mode looks like this:

.. thumbnail:: ../../../img/graphql/manual/auth/webhook-auth.png

2) **JWT** (JSON Web Token): Your auth server will issue JWT tokens to your client app, which, when sent as part of the request, are verified and decoded by GraphQL engine to get metadata about the request to evaluate access control rules. The life-cycle of a GraphQL query, modified for **JWT** mode looks like this:

.. thumbnail:: ../../../img/graphql/manual/auth/jwt-auth.png

Authorization - Overview
------------------------

Access control is done by creating permission rules for each role, table and operation(like *insert*, *update*, etc.). These access control rules use dynamic session variables that are passed to GraphQL engine with every request.

**For example:**

.. thumbnail:: ../../../img/graphql/manual/auth/hasura-perms.png
   :width: 80 %


.. admonition:: Getting started with access control

  1) **While developing**, you need not set up or integrate your auth service with GraphQL Engine and you can just send the session variables as request headers directly in the Graphiql interface of the console.

  2) While this is not exactly a mode of authentication and is highly **NOT RECOMMENDED** in production, you can secure your GraphQL endpoint by :doc:`configuring an admin secret key <../deployment/securing-graphql-endpoint>`.

However, **in production** when your application is deployed, your app can't send these authorization variables
directly! Your app will likely only send an authorization token or cookie provided by your app's authentication
system to Hasura. In this case, Hasura will make a request to a webhook set up by you with the request headers your
app has sent (authorization tokens, cookies, etc). The webhook should then return the variables required as context for
the access control rules. Alternatively, your app can send to Hasura JWT tokens, which can then be decoded by Hasura to
get the variables required for the access control rules.

See :doc:`webhook` or :doc:`jwt` for more details on passing dynamic session variables.

Next, let's setup some :doc:`basic access control rules <basics>`.

**See:**

.. toctree::
   :maxdepth: 1

   basics
   roles-variables
   common-roles-auth-examples
   webhook
   webhook-examples
   jwt
   jwt-examples
