.. _hub:

============================
Publish your project to /hub
============================

Any Hasura project can be published to `/hub <https://hasura.io/hub>`_.
The details of what gets published are mentioned in ``hasura.yaml``.


.. code-block:: bash

   $ #From inside your project directory
   $ hasura publish


This will create a tarball of the current directory state, and use the metadata from ``hasura.yaml`` information to publish
your project to the hub.

hasura.yaml
^^^^^^^^^^^

This file contains the name of your project and the Hasura platform version.

.. code-block:: yaml

  name: <project_name>
  platformVersion: v0.15.3

