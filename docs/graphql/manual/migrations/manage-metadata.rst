.. meta::
   :description: Manage Hasura metadata
   :keywords: hasura, docs, metadata

.. _manage_hasura_metadata:

Managing Hasura metadata
========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

If your Postgres schema is already managed with a tool like knex, TypeORM,
Django/Rails migrations, you will still need a way to export the actions you
performed on the Hasura console to apply it later on another Hasura instance.

All the actions performed on the console, like tracking tables/views/functions,
creating relationships, configuring permissions, creating event triggers and remote
schemas, etc. can be exported as a JSON/yaml metadata file which can be version
controlled. The metadata file can be later imported to another Hasura instance to get the same
configuration. You can also manually edit the metadata file to add more objects to
it and then use it to update the instance.

Exporting Hasura metadata
-------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: CLI

     Metadata can be exported with the :ref:`hasura metadata export <hasura_metadata_export>`
     command.

     This will export the metadata as yaml files in the ``/metadata`` directory

  .. tab:: Console

     1. Click on the settings (⚙) icon at the top right corner of the console screen.
     2. In the Hasura metadata actions page that opens, click on the ``Export Metadata`` button.
     3. This will prompt a file download for ``hasura_metadata_<timestamp>.json``. Save the file.

  .. tab:: CLI

     Metadata can be exported with the :ref:`hasura metadata export <hasura_metadata_export>` command via the Hasura CLI.

  .. tab:: API

     The export can be done via the :ref:`Metadata API
     <api_manage_metadata>`.
     Response will be a JSON object with the Hasura metadata. Here is an example
     using ``curl`` to save this as a file:

     .. code-block:: bash

        curl -d'{"type": "export_metadata", "args": {}}' http://localhost:8080/v1/query -o hasura_metadata.json

     This command will create a ``hasura_metadata.json`` file.
     If an admin secret is set, add ``-H 'X-Hasura-Admin-Secret: <your-admin-secret>'`` as the API is an
     admin-only API.

Applying/Importing Hasura metadata
----------------------------------

You can apply exported metadata from one Hasura GraphQL engine instance to another. You can also apply an older or
modified version of an instance's metadata onto itself.

Importing completely replaces the metadata on that instance, i.e. you lose any metadata that was already present
before.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: CLI

     Metadata can be applied with the :ref:`hasura metadata apply <hasura_metadata_apply>`
     command.

  .. tab:: Console

     1. Click on the settings (⚙) icon at the top right corner of the console screen.
     2. Click on ``Import Metadata`` button.
     3. Choose a ``hasura_metadata.json`` file that was exported earlier.
     4. A notification should appear indicating the success or error.

  .. tab:: API

     The exported JSON can be imported via the :ref:`Metadata API
     <api_manage_metadata>`.
     Here is an example using ``curl``:

     .. code-block:: bash

        curl -d'{"type":"replace_metadata", "args":'$(cat hasura_metadata.json)'}' http://localhost:8080/v1/query

     This command reads the ``hasura_metadata.json`` file and makes a POST request to
     replace the metadata.
     If an admin secret is set, add ``-H 'X-Hasura-Admin-Secret: <your-admin-secret>'`` as the API is an
     admin-only API.

.. note::

   All the dependent objects, like tables, views, functions etc. should exist on
   Postgres before importing the metadata. Otherwise, it will result in an error
   saying the object does not exist. So, apply the Postgres schema first, before
   importing the metadata.


.. _reload_metadata_manual:

Reloading Hasura metadata
-------------------------

In some cases, the metadata can be out of sync with the Postgres schema. For example,
when a new column has been added to a table via an external tool such as ``psql``.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: CLI

     Metadata can be reloaded with the :ref:`hasura metadata reload <hasura_metadata_reload>`
     command.

  .. tab:: Console

     1. Click on the settings (⚙) icon at the top right corner of the console screen.
     2. Click on ``Reload`` button.
     3. A notification should appear indicating the success.

  .. tab:: CLI

     Metadata can be reloaded with the :ref:`hasura metadata reload <hasura_metadata_reload>` command via the Hasura CLI.

  .. tab:: API

     The reload of metadata can be done via the :ref:`Metadata API
     <api_manage_metadata>`.
     Here is an example using ``curl``:

     .. code-block:: bash

        curl -d'{"type": "reload_metadata", "args": {}}' http://localhost:8080/v1/query

     If an admin secret is set, add ``-H 'X-Hasura-Admin-Secret: <your-admin-secret>'`` as the API is an
     admin-only API.

.. note::

   Reloading may result in inconsistent metadata status. You may need to resolve
   all inconsistent objects manually or delete them. After that, you need to reload
   metadata again.

Managing Hasura metadata in CI/CD
---------------------------------

In case you need an automated way of applying/importing the metadata, take a
look at the :ref:`cli-migrations <auto_apply_migrations>` Docker image, which
can start the GraphQL engine after automatically importing a mounted metadata
directory.
