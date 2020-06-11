.. meta::
   :description: Manage remote schemas with Hasura
   :keywords: hasura, docs, remote schema

.. _remote_schemas:

Remote schemas
==============

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Hasura gives you CRUD + realtime GraphQL APIs with authorization & access control. However, in many cases, you will
need to write APIs (queries, mutations) that contain custom logic. For example, implementing a payment API, or
querying data that is not in your database.

Hasura has the ability to merge remote GraphQL schemas and provide a unified GraphQL API. Think of it
like automated schema stitching. All you need to do is build your own GraphQL service and then provide the HTTP
endpoint to Hasura. Your GraphQL service can be written in any language or framework.

This is what Hasura running with "Remote schemas" looks like:


.. thumbnail:: /img/graphql/manual/remote-schemas/remote-schemas-arch.png
   :class: no-shadow
   :width: 55%
   :alt: Architecture of Hasura with remote schemas

.. note::

  This is a new feature under active development. Please do give us feedback, bug-reports and ask us questions on
  our `discord <https://discord.gg/vBPpJkS>`__ or on `github <https://github.com/hasura/graphql-engine>`__.

Use cases
---------

- Custom business logic, like a payment API
- Querying data that is not available in your database


You can handle these use cases by writing resolvers in a custom GraphQL server
and making Hasura merge this "remote schema" with the existing auto-generated
schema. You can also add multiple remote schemas. Think of the merged schema as
a union of top-level nodes from each of the sub-schemas.

.. note::

  If you are looking for adding authorization & access control for your
  app users to the GraphQL APIs that are auto-generated via Hasura, head to
  :ref:`auth`

Adding a remote schema
----------------------

Follow the steps below to add a "remote schema" to the Hasura GraphQL engine:

Step 1: Write a custom GraphQL server
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You need to create a custom GraphQL server with a schema and corresponding resolvers that solve your use case
(*if you already have a functional GraphQL server that meets your requirements, you can skip this step*).

You can use any language/framework of your choice to author this server and deploy it anywhere. A great way to get
started is to use one of our boilerplates:

- `Boilerplates <https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/remote-schemas>`__

.. _merge_remote_schema:

Step 2: Merge remote schema
^^^^^^^^^^^^^^^^^^^^^^^^^^^

To merge your remote schema with the GraphQL engine's auto-generated schema:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    Head to the ``Remote Schemas`` tab of the console and click on the ``Add`` button.

    .. thumbnail:: /img/graphql/manual/business-logic/add-remote-schemas-interface.png
      :alt: Merge remote schema

    You need to enter the following information:

    - **Remote Schema name**: an alias for the remote schema that must be unique on an instance of the GraphQL engine.
    - **GraphQL server URL**: the endpoint at which your remote GraphQL server is available. This value can be entered
      manually or by specifying an environment variable that contains this information.

  .. tab:: Via CLI

    To add a remote schema, edit the ``remote_schemas.yaml`` file in the ``metadata`` directory as follows:

    .. code-block:: yaml
       :emphasize-lines: 1-5

        - name: my-remote-schema
          definition:
            url: https://business-logic.domain/graphql
            timeout_seconds: 60
            forward_client_headers: true

    Then apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: Via API

    You can add a remote schema by using the :ref:`add_remote_schema API <add_remote_schema>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
          "type": "add_remote_schema",
          "args": {
              "name": "my-remote-schema",
              "definition": {
                  "url": "https://business-logic.domain/graphql",
                  "headers": [
                      {
                          "name": "X-Server-Request-From",
                          "value": "Hasura"
                      }
                  ],
                  "forward_client_headers": true,
                  "timeout_seconds": 60
              }
          }
      }

  .. note::

    During **local development** with the remote schema server on localhost, ensure that the Hasura Docker container can reach
    the server endpoint on the host. A common way to do this is to use the domain ``host.docker.internal`` on Docker for Mac and Windows or the Docker bridge IP on Linux (typically ``172.17.0.1`` - the IP address of ``docker0`` interface. use ``ip addr show docker0``) instead of ``127.0.0.1``. 
    
    Similarly, if you are adding the URL using env variable, then run the Hasura docker container with the env variable added during `docker run`. Example ``-e REMOTE_SCHEMA_ENDPOINT=http://host.docker.internal:4000/mycustomgraphql``

- **Headers**: configure the headers to be sent to your custom GraphQL server:

  - Toggle forwarding all headers sent by the client (when making a GraphQL query) to your remote GraphQL server.
  - Send additional headers to your remote server - these can be static header name-value pairs; and/or pairs of
    "header name-environment variable name". You can specify the value of the header to be picked up from the environment
    variable.

    **Example**: Let's say your remote GraphQL server needs a ``X-Api-Key`` as a header. As this value contains
    sensitive data (like API key in this example), you can configure the name of an environment variable which will hold
    the value. This environment variable needs to be present when you start the GraphQL engine. When Hasura sends
    requests to your remote server, it will pick up the value from this environment variable.

.. admonition:: Using environment variables

  If you are using environment variables in the remote schema configuration - either
  for URL or headers - **the environment variables need to be present**  with valid values
  when adding the remote schema i.e. the GraphQL engine should be started with these environment variables.

Click on the ``Add Remote Schema`` button to merge the remote schema.

Step 3: Make queries to the remote server from Hasura
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    Now you can head to the ``GraphiQL`` tab and make queries to your remote server from Hasura.

  .. tab:: Via API

    You can query your remote server by making requests to the Hasura GraphQL endpoint (``/v1/graphql``).

Points to remember
------------------

Remote schema fields nomenclature
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- Top-level field names need to be unique across all merged schemas (*case-sensitive match*).
- Types with the *exact same name and structure* will be merged. But types with the *same name but different
  structure* will result in type conflicts.


Schema refreshing
^^^^^^^^^^^^^^^^^

For versions <= ``v1.0.0-beta.2``, GraphQL schema of each added remote server is refreshed every time a
metadata modifying operation like adding tables/functions, defining relationships/permissions etc. is done.

From ``v1.0.0-beta.3`` onwards, a remote server's GraphQL schema is cached and refreshed only when user
explicitly reloads remote schema.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    Click the ``Reload`` button on the console.

  .. tab:: Via API

    Make a request to the :ref:`reload_remote_schema<api_remote_schemas>` API.

Current limitations
^^^^^^^^^^^^^^^^^^^

- Nodes from different GraphQL servers cannot be used in the same query/mutation. All top-level fields have to be
  from the same GraphQL server.
- Subscriptions on remote GraphQL servers are not supported.

These limitations will be addressed in upcoming versions.

Extending the auto-generated GraphQL schema fields
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For some use cases, you may need to extend the GraphQL schema fields exposed by the Hasura GraphQL engine
(*and not merely augment as we have done above*) with a custom schema/server. To support them, you can use
community tooling to write your own client-facing GraphQL gateway that interacts with the GraphQL engine.

.. note::

  **Adding an additional layer on top of the Hasura GraphQL engine significantly impacts the performance provided by
  it out of the box** (*by as much as 4x*). If you need any help with remodelling these kinds of use cases to use the
  built-in remote schemas feature, please get in touch with us on `Discord <https://discord.gg/vBPpJkS>`__.


Authorization in your remote schema server
------------------------------------------

Hasura will forward the resolved ``x-hasura-*`` values as headers to your remote
schema. You can use this information to apply authorization rules in your
server. You don't have to redo authentication in your remote schema server.

You can also configure Hasura to have (as shown :ref:`above <merge_remote_schema>`):

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
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
