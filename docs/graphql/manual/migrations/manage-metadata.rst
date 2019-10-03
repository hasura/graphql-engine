.. _manage_hasura_metadata:

Managing Hasura metadata
========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

If your Postgres schema is already managed with a tool like knex, TypeORM,
Django/Rails migrations, you will still need a way to export the actions you
performed on the Hasura console to apply it later on another Hasura instance.

All the actions performed on the console, like tracking tables/views/functions,
creating relationships, configuring permissions, creating event triggers and remote
schemas, etc. can be exported as a JSON file which can be version
controlled. The content of this JSON file is called "Hasura metadata". The
metadata file can be later imported to another Hasura instance to get the same
configuration. You can also manually edit the JSON file to add more objects to
it and then use it to update the instance.

Exporting Hasura metadata
-------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

     1. Click on the settings (⚙) icon at the top right corner of the console screen.
     2. In the Hasura metadata actions page that opens, click on the ``Export Metadata`` button.
     3. This will prompt a file download for ``metadata.json``. Save the file. 

  .. tab:: API

     The export can be done via the :doc:`Metadata API
     <../api-reference/schema-metadata-api/manage-metadata>` too.
     Response will be a JSON object with the Hasura metadata. Here is a example
     using ``curl`` to save this as a file:

     .. code-block:: bash

        curl -d'{"type": "export_metadata", "args": {}}' http://localhost:8080/v1/query -o metadata.json

     This command will create a ``metadata.json`` file. If admin secret is set,
     add ``-H 'X-Hasura-Admin-Secret: <your-admin-secret>'`` as the API is an
     admin-only API.

Importing Hasura metadata
-------------------------

You can apply exported metadata from one Hasura GraphQL engine instance to another. You can also apply an older or
modified version of an instance's metadata onto itself.

Importing completely replaces the metadata on that instance. ie: you lose any metadata that was already present

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

     1. Click on the settings (⚙) icon at the top right corner of the console screen.
     2. Click on ``Import Metadata`` button.
     3. Choose a ``metadata.json`` file that was exported earlier.
     4. A notification should appear indicating the success or error.

  .. tab:: API

     The exported JSON can be imported via the :doc:`Metadata API
     <../api-reference/schema-metadata-api/manage-metadata>` too.
     Here is a example using ``curl``:

     .. code-block:: bash

        curl -d'{"type":"replace_metadata", "args":'$(cat metadata.json)'}' http://localhost:8080/v1/query

     This command will read the ``metadata.json`` file and makes a POST request to
     replace the metadata. If an admin secret is set, add ``-H
     'X-Hasura-Admin-Secret: <your-admin-secret>'`` as the API is an admin-only
     API.

.. note::

   All the dependent objects, like tables, views, functions etc. should exist on
   Postgres before importing the metadata. Otherwise, it will result in an error
   saying the object does not exist. So, apply the Postgres schema first, before
   importing the metadata.


The ``curl`` based API calls can be easily integrated with your CI/CD workflows.
In case you need an automated way of applying/importing the metadata, take a
look at the :doc:`CLI-Migrations <auto-apply-migrations>` Docker image, which
can start the GraphQL engine after automatically importing a mounted metadata file. 
