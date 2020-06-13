.. meta::
   :description: Bypassing Hasura's authorization system for remote schema queries
   :keywords: hasura, docs, remote schema, authorization

.. _bypass_auth:

Bypassing Hasura's authorization system for remote schema queries
-----------------------------------------------------------------

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

It might be necessary sometimes to bypass Hasura's authorization system (calling
the configured webhook, or validating the JWT), for queries that are for a
remote GraphQL server.

**For example**, you have a remote GraphQL server which does authentication,
i.e. signup and login, and you have added it as a remote schema. In this case,
you would not want to perform Hasura's authorization when the user is making a
login/signup request.

There is no first-class option to currently do this via any configuration in
Hasura. However a similar solution can be achieved by the following workarounds:

Bypassing webhook authorization
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you have a :ref:`webhook authorization setup <auth_webhooks>`, in the normal scenario, your authorization
webhook would return ``200`` on success and ``401`` if it is either unable to authorize the current request or if
the authorization information is absent (like cookie, authorization header etc.)

To bypass the webhook auth:

- the webhook should respond with ``200`` and ``x-hasura-role: anonymous`` instead of a ``401`` when the
  authorization information is absent or if it fails to resolve the authorization information.
- when adding the remote schema, check the ``Forward all headers from client`` option so that the remote server
  will get the relevant cookie/header (from the client) and the role ``anonymous``.

Bypassing JWT authorization
^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you have a :ref:`JWT authorization setup <auth_jwt>`, to bypass the JWT auth:

- your authentication server should generate a static JWT token for ``anonymous`` i.e. unauthenticated users.
- when adding the remote schema, check the ``Forward all headers from client`` option so that the remote server
  will get the JWT (from the client).

For example, the generated JWT can be:

.. code-block:: json

  {
    "sub": "0000000000",
    "iat": 1516239022,
    "role": "anonymous",
    "https://hasura.io/jwt/claims": {
      "x-hasura-allowed-roles": ["anonymous"],
      "x-hasura-default-role": "anonymous"
    }
  }


Hasura will get this JWT and successfully validate it. When your remote server receives this JWT, it should
specifically validate the JWT and, for example, check for the ``role`` key in the JWT. If it is set to ``anonymous``,
then it should consider the request as unauthenticated.
