.. meta::
   :description: Authorization in your remote schema server with Hasura
   :keywords: hasura, docs, remote schema, authorization

.. _schema_auth:

Authorization in your remote schema server
==========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Hasura will forward the resolved ``x-hasura-*`` values as headers to your remote
schema. You can use this information to apply authorization rules in your
server. You don't have to redo authentication in your remote schema server. Native fine grained access control for remote schemas will be released soon.

You can also configure Hasura to have (as shown :ref:`here <merge_remote_schema>`):

1. static header values that are sent to the remote server
2. forward all headers from the client (like ``Authorization``, ``Cookie`` headers etc.)

In case there are multiple headers with same name, the order of precedence is:
configuration headers > resolved user (``x-hasura-*``) variables > client headers.

So for example, if the client sends an ``Authorization`` header, and the
configuration also has an ``Authorization`` header, the configuration header value
will selected.

.. note::

   The headers from the client behave similarly to the authorization system. If
   ``x-hasura-admin-secret`` is sent, then all ``x-hasura-*`` values from the
   client are respected, otherwise they are ignored.

Cookie header from your remote GraphQL servers
----------------------------------------------
``Set-Cookie`` headers from your remote schema servers are sent back to the
client over HTTP transport. **Over websocket transport there exists no means 
of sending headers after a query/mutation and hence the ``Set-Cookie`` headers are 
not sent to the client.** Use HTTP transport if your remote servers set cookies.


Bypassing Hasura's authorization system for remote schema queries
-----------------------------------------------------------------

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
