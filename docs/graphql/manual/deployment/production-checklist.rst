.. _production-checklist:

Production Checklist
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

This guide is a checklist for configuring and securing GraphQL engine for a
production deployment.

Set Admin Secret
----------------

Set an admin secret to protect the API from unauthrorized access. It is
recommended to keep this as a long string.

.. code-block:: bash

   # set env var
   HASURA_GRAPHQL_ADMIN_SECRET=averylongpasswordstring

   # or use the flag
   graphql-engine --database-url=<database-url> serve --admin-secret=<averylongpasswordstring>

More details can be found at :ref:`securing-graphql-endpoint`.

Verify Permissions
------------------

Review the summary
~~~~~~~~~~~~~~~~~~
Review the authorization/permission rules set on tables. You can make use of the
"Schema permissions summary" page to get a bird's eye view on all the
permissions set across all tables and roles. Pay extra attention to roles like
"anonymous" which allow unauthenticated access.

.. image:: https://graphql-engine-cdn.hasura.io/img/schema_permissions_summary.png
   :class: no-shadow                                                              
   :alt: Hasura console - Schema permissions summary

Limit number of rows returned
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You should :ref:`limit the number of rows <limit-rows-permissions>` that can be
accessed in one request, by setting the number in the select permission. This
will prevent someone from accidentally or otherwise querying the entire table in
one shot, thus adding load on Postgres.

Disable APIs
------------

Hasura exposes many APIs which might not be relevant for a production instance
that is only supposed to serve GraphQL. APIs can be selectively enabled using
the corresponding flag or environment variable.

In most production scenarios, you would only need GraphQL API to be enabled.

.. code-block:: bash

   # set this env var to enable only the graphql api
   HASURA_GRAPHQL_ENABLED_APIS=graphql

   # if you're using flags
   graphql-engine --database-url=<database-url> serve --enabled-apis=graphql

By setting the above flag or env var, we are disabling the ``metadata``,
``pg_dump`` and ``config`` APIs. ``health`` and ``version`` APIs are public and
cannot be disabled.

Read more at :ref:`api-reference`.

.. note::

   If you're using ``cli-migrations`` image, prior to ``v1.0.0-beta.8``, setting
   enabled APIs to only ``graphql`` can cause the migration apply step to fail.
   Please update to the latest version if you're facing this issue.
   ``graphql`` can cause the mi


Disable Console
---------------

It is recommended that you disable the console on production deployments. Also,
when you disable Metadata APIs, console will stop working.

.. code-block:: bash

   # set the env var to disable console
   HASURA_GRAPHQL_ENABLE_CONSOLE=false

   # when using flags, no --enable-console flag implies console is disabled
   graphql-engine --database-url=<database-url> serve

.. note::

   You can still use the CLI to open a console connected to this instance.
   (Provided ``metadata`` APIs are not disabled).

Set up an Allow List
--------------------

An allow-list can be set up to restrict what kind of queries can be made against
this particular instance. If your API is meant to serve a frontend client, you
can only allow those queries used by the client to pass through. Every other
query will be rejected without even getting validated.

Read more at :ref:`allow-list`.

Restrict CORS Domains
---------------------

By default, all cross-origin requests are allowed by Hasura. You should restrict
it to the domains which you trust.

.. code-block:: bash

   # set the env var, accept cross-origin requests from https://my-ui.com
   HASURA_GRAPHQL_CORS_DOMAIN=https://my-ui.com

   # using flags
   graphql-engine --database-url=<database-url> server --cors-domain="https://my-ui.com"

You can read more about this setting at :ref:`configure-cors`.

Enable HTTPS
------------

A detailed guide is available at :ref:`enable-https`.

Configure Logging
-----------------


The :ref:`guide on logs <hge_logs>` describes different log types Hasura uses.
You can configure the GraphQL engine to enable/disable certain log-types using
the the ``--enabled-log-types`` flag or the ``HASURA_GRAPHQL_ENABLED_LOG_TYPES``
env var.

If you are collecting your logs using an agent and you're interested in
capturing the request logs along with the SQL that is generated, you should
enable ``query-log`` (it is not enabled by default).

.. code-block:: bash

   # enable all log types
   HASURA_GRAPHQL_ENABLED_LOG_TYPES=startup,http-log,query-log,websocket-log,webhook-log

   # using flags
   graphql-engine --database-url=<database-url>
   serve --enabled-log-types="startup,http-log,query-log,websocket-log,webhook-log"
