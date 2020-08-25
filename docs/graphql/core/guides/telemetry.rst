.. meta::
   :description: Telemetry on Hasura
   :keywords: hasura, docs, guide, telemetry

.. _telemetry:

Telemetry Guide/FAQ
===================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

The Hasura GraphQL engine collects anonymous telemetry data that helps the
Hasura team in understanding how the product is being used and in deciding
what to focus on next.

The data collected is minimal, statistical in nature and 
**cannot be used to uniquely identify a user**. Please see the 
next section to see what data is collected and sent. Access to collected 
data is strictly limited to the Hasura team and not shared with 3rd parties.

As a growing community, we greatly appreciate the usage data users
send to us, as it is very valuable in helping us make the Hasura GraphQL 
engine a better product for everyone! However, if you are concerned about 
sending usage stats, you can choose to disable telemetry as 
described :ref:`here <telemetry_optout>`.


What data is collected?
-----------------------

Server
~~~~~~

The server periodically sends the number of tables, views, relationships,
permission rules, custom SQL functions, event triggers and remote schemas
tracked by the GraphQL engine, along with randomly generated UUIDs per database and
per instance. The name of the current continuous integration environment
(if any) and the server version is also sent.

Here is a sample row from the telemetry database:

.. code-block:: json

   {
      "id": 12,
      "timestamp": "2019-01-21T19:43:33.63838+00:00",
      "db_uid": "dddff371-dab2-450f-9969-235bca66dab1",
      "instance_uid": "6799360d-a431-40c5-9f68-24592a9f07df",
      "version": "v1.0.0-alpha36",
      "ci": "TRAVIS",
      "metrics": {
        "views": 1,
        "tables": 2,
        "functions": 1,
        "permissions": {
          "roles": 1,
          "delete": 2,
          "insert": 1,
          "select": 2,
          "update": 2
        },
        "relationships": {
          "auto": 2,
          "manual": 0
        },
        "event_triggers": 0,
        "remote_schemas": 1
      }
    }


Console
~~~~~~~

The console is a React-Redux app. Redux action names along with anonymized
route names are sent without any identifiable information or payload. The console
also records the UUID of the server/CLI that it is connected to.

Here is a sample:

.. code-block:: json

   {
     "id": 902,
     "timestamp": "2019-01-21T10:00:23.849202+00:00",
     "url": "/data/schema/SCHEMA_NAME/tables/TABLE_NAME/modify",
     "event_type": "ModifyTable/RESET",
     "console_mode": "server",
     "server_uuid": "79485a57-fca5-40f3-a31b-78c0d211314b",
     "server_version": "v1.0.0-alpha36",
     "cli_uuid": null
   }

Please note that ``TABLE_NAME`` and ``SCHEMA_NAME`` are not placeholders. 
The actual names of the tables, schemas, remote-schemas and event-triggers that
are a part of the URL are not sent.

CLI
~~~

The CLI collects each execution event, along with a randomly generated UUID.
The execution event contains the command name, timestamp and whether the
execution resulted in an error or not. **Error messages, arguments and flags
are not recorded**. The CLI also collects the server version and UUID that it
is talking to. The operating system platform and architecture is also
noted along with the CLI version.

Sample data:

.. code-block:: json

   {
     "id": 115,
     "timestamp": "2019-01-21T11:36:07.86783+00:00",
     "uuid": "e462ce20-42dd-40fd-9549-edfb92f80455",
     "execution_id": "ddfa9c33-0693-457d-9026-c7f456c43322",
     "version": "v0.4.27",
     "command": "hasura version",
     "is_error": false,
     "os_platform": "linux",
     "os_arch": "amd64",
     "server_uuid": "a4d66fb2-f88d-457b-8db1-ea7a0b57921d",
     "server_version": "v1.0.0-alpha36",
     "payload": null
   }

Where is the data sent?
-----------------------

The data is sent to Hasura's servers addressed by ``telemetry.hasura.io``.

.. _telemetry_optout:

How do I turn off telemetry (opt-out)?
--------------------------------------

Disable server telemetry
~~~~~~~~~~~~~~~~~~~~~~~~

You can turn off telemetry on **the server and on the console hosted by the server**
by setting the env variable ``HASURA_GRAPHQL_ENABLE_TELEMETRY=false`` or the flag
``--enable-telemetry=false`` on the server.

Disable CLI telemetry
~~~~~~~~~~~~~~~~~~~~~

You can turn off telemetry on **the CLI and on the console served by the CLI**
by setting the env variable ``HASURA_GRAPHQL_ENABLE_TELEMETRY=false`` on the
machine running the CLI. You can also set ``"enable_telemetry": false`` in the
JSON config file created by the CLI at ``~/.hasura/config.json`` to persist the
setting.

Privacy Policy
--------------

You can check out our privacy policy `here <https://hasura.io/legal/hasura-privacy-policy>`__.
