Telemetry Guide/FAQ
===================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The Hasura GraphQL Engine collects anonymous telemetry data that helps the Hasura team understand how the product is being used and in deciding what to focus on next.

The data collected is minimal and, since there is no *sign-in* associated with the GraphQL Engine, it **cannot be used to uniquely identify any user**. Furthermore, data collected is strictly statistical in nature and **no propriatary information is collected** (*please see the next section*).

As a growing community, we greatly appreciate the telemetry data users send to us, as it is very valuable in making GraphQL Engine a better product for everyone. If you are worried about privacy, you can choose to disable sending telemetry as described :ref:`here <optout>`.

.. note::

   Access to collected data is strictly limited to the Hasura team and not shared with 3rd parties.

What data are collected?
------------------------

Server
~~~~~~

The server periodically sends the number of tables, views, relationships,
permission rules, event triggers and remote schemas tracked by GraphQL Engine,
along with a randomly generated UUID and the server version.

.. lets add a table here. 


Console
~~~~~~~

The console is a React-Redux UI. Redux action names along with anonymized
route names are sent without any identifiable information or payload. Console
also records the UUID of the server/CLI that it is connected to.

CLI
~~~

The CLI collects each execution event, along with a randomly generated UUID.
The execution event contains the command name, timestamp and whether the
execution resulted in an error or not. **Error messages, arguments and flags
are not recorded**. CLI also collects the server version and UUID that it
is talking to. The operating system platform and architecture is also
noted along with the CLI version.

.. lets add a few lines here?

Where is the data sent?
-----------------------

The data is sent to Hasura's servers addressed by ``telemetry.hasura.io``.

.. _optout:
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

Privacy Policy
--------------

You can check out our privacy policy `here <https://hasura.io/legal/hasura-privacy-policy>`_.