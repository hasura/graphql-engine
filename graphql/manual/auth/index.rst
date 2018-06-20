Autentication / Access control
==============================

Hasura helps you define granular access controls for every field in your GraphQL schema, basically every table or view in your postgres schema. These access control rules can use dynamic variables that come in with every request.



While developing, you can send variables as request headers directly.


However, in production, when your application is deployed your app will only send an authorization token, or cookie. In this case, when your app makes queries to Hasura, Hasura makes a request to a webhook with the request headers your app has sent (authorization tokens, cookies etc). Your webhook should then return an object of variables that will be provided as context to the access control rules.


Next, let's setup some :doc:`basic access control rules<basics>`.

- Basics
- Access control rules using relationships across tables
- Setting up your authorization webhook

Authorization
-------------
You can run Hasura's GraphQL Engine in three modes

1. No Authentication mode
^^^^^^^^^^^^^^^^^^^^^^^^^

- When ``--access-key`` and ``--auth-hook`` are not set

- It is useful when you're developing . It is not recomended to use in production but however you can have proxy gateway that will set (``X-Hasura-Access-Key``) header and other required ``X-Hasura-*`` headers.

Run server in this mode using following docker command.

.. code-block:: bash

   docker run --name hasura-graphql-engine -p 9000:9000 \
              --link hasura-postgres:postgres \
              -d hasuranightly/raven:8df5234 raven \
              --database-url \
                postgres://postgres:mysecretpassword@postgres:5432/postgres \
                serve --server-port 9000 --cors-domain "*"


2. Access key mode
^^^^^^^^^^^^^^^^^^

- When only ``--access-key`` is set. See :doc:`GraphQL Server Options <../getting-started/deploy/raven-opts>`

- Server authenticates based on ``X-Hasura-Access-Key`` header and expects all other required ``X-Hasura-*`` headers.

Run server in this mode using following docker command.

.. code-block:: bash

   docker run --name hasura-graphql-engine -p 9000:9000 \
              --link hasura-postgres:postgres \
              -d hasuranightly/raven:8df5234 raven \
              --database-url \
                postgres://postgres:mysecretpassword@postgres:5432/postgres \
                serve --server-port 9000 --access-key myAccKey \
                  --cors-domain "*"


3. Access key and Authorization webhook mode
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- When both ``--access-key`` and ``--auth-hook`` are set

- This mode is useful in production. When server founds ``X-Hasura-Access-Key`` header it ignores webhook and expects all other required ``X-Hasura*`` headers

- If ``X-Hasura-Access-key`` header not found then server authenticaters through webhook. See :doc:`Authorization Webhook <hook>`

Run server in this mode using following docker command.

.. code-block:: bash

   docker run --name hasura-graphql-engine -p 9000:9000 \
              --link hasura-postgres:postgres \
              -d hasuranightly/raven:8df5234 raven \
              --database-url \
                postgres://postgres:mysecretpassword@postgres:5432/postgres \
                serve --server-port 9000 --access-key myAccKey \
                  --auth-hook http://myAuthhook/ --cors-domain "*"


Permissions
-----------

- Role and user-id based rules

See:
----

.. toctree::
   :maxdepth: 1

   hook
