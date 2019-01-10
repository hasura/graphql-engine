Telemetry Guide/FAQ
===================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Since ``v1.0.0-alpha35``, the Hasura GraphQL Engine Server, CLI
and Console collects anonymous usage stats. The resulting data
plays a key role in understanding how users and using the
product and in deciding what to focus on next.

.. note::

   The data collected is strictly anonymous and does not contain
   any kind of identifiable information. The data is used solely
   for the purpose of improving the product and is not shared
   outside Hasura.

What data are collected?
------------------------

Server
~~~~~~

The server periodically sends the number of tables, views, relationships,
permission rules, event triggers and remote schemas tracked by GraphQL Engine,
along with a randomly generated UUID and the server version.
Note that only the number is sent, not anything identifiable.

Console
~~~~~~~

The console is a React-Redux UI. Redux action names along with anonymized
route names are sent without any identifiable information or payload. Console
also records the UUID of the server/CLI that it is connected to.

CLI
~~~

The CLI collects each execution event, along with a randomly generated UUID.
The execution event contains the command name, timestamp and whether the
execution resulted in an error or not. Error messages, arguments and flags
are not recorded. CLI also collects the server version and UUID that it
is talking to. The operating system platform and architecture is also
noted along with the CLI version.

Where is the data sent?
-----------------------

The data is sent to Hasura's servers addressed by ``telemetry.hasura.io``.


How do I turn off telemetry (opt-out)?
--------------------------------------

You can turn off telemetry on the server and on the console hosted by server
by setting the following environment variable on the server or by using
the flag ``--enable-telemetry=false``:

.. code-block:: bash

   HASURA_GRAPHQL_ENABLE_TELEMETRY=false

In order to turn off telemetry on CLI and on the console served by CLI,
you can set the same environment varibale on the machine running CLI.
You can also set ``"enable_telemetry": false`` in the JSON file created
by the CLI at ``~/.hasura/.config.json`` to perisist the setting.
