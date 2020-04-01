.. meta::
   :description: Hasura Metadata file format reference
   :keywords: hasura, docs, metadata, file format

.. _metadata_file_format:

Metadata file format reference
==============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The CLI now supports two versions of configuration: ``v1`` and ``v2``

config v1
---------

For ``config v1``, the ``config.yaml`` of your Hasura project would look like:

.. code-block:: bash

    endpoint: http://localhost:8080

The metadata file that is exported from the server is a JSON/YAML representation
of the Hasura metadata stored in the ``hdb_catalog`` schema on the Postgres
database.

The top level keys will be the following arrays:

.. code-block:: yaml

   functions: []
   remote_schemas: []
   tables: []

Depending on the tables tracked, remote schemas and functions created, these
keys will have elements inside them.

The ``table`` will have permission rules, relationships and event triggers
defined for each table. Here is an example metadata file:

**metadata.yaml**

.. code-block:: yaml

   functions:
   - search_articles
   remote_schemas:
   - comment: null
     definition:
       forward_client_headers: false
       headers: []
       url: https://graphql-pokemon.now.sh/graphql
       url_from_env: null
     name: pokemon
   tables:
   - table: author
     array_relationships:
     - comment: null
       name: articlesByauthorId
       using:
         foreign_key_constraint_on:
           column: author_id
           table: article
     delete_permissions: []
     event_triggers: []
     insert_permissions:
     - comment: null
       permission:
         check:
           id:
             _eq: X-Hasura-User-Id
         columns:
         - name
         set: {}
       role: user
     object_relationships: []
     select_permissions:
     - comment: null
       permission:
         allow_aggregations: false
         columns:
         - id
         - name
         filter:
           id:
             _eq: X-Hasura-User-Id
       role: user
     update_permissions: []
   - table: article
     array_relationships: []
     delete_permissions: []
     event_triggers:
     - definition:
         delete:
           columns: '*'
         insert:
           columns: '*'
         update:
           columns:
           - id
           - title
           - author_id
       headers: []
       name: update_article_search_index
       retry_conf:
         interval_sec: 10
         num_retries: 0
         timeout_sec: 60
       webhook: https://my-algolia-api.com/update_index
     insert_permissions:
     - comment: null
       permission:
         check:
           author_id:
             _eq: X-Hasura-User-Id
         columns:
         - title
         set:
           author_id: x-hasura-user-id
       role: user
     object_relationships:
     - comment: null
       name: authorByauthorId
       using:
         foreign_key_constraint_on: author_id
     select_permissions:
     - comment: null
       permission:
         allow_aggregations: true
         columns:
         - author_id
         - id
         - title
         filter:
           author_id:
             _eq: X-Hasura-User-Id
       role: user
     update_permissions: []

The schema for this file will mostly correspond to the table structure of the
:ref:`metadata catalogue <hasura_metadata_schema>`.

config v2
---------

For ``config v2``, the ``config.yaml`` of your Hasura project would look like:

.. code-block:: bash

    actions:
      handler_webhook_baseurl: http://localhost:3000/api
      kind: synchronous
    endpoint: http://localhost:8080
    metadata_directory: metadata
    version: 2

With ``config v2``, the metadata that is exported from the server is a directory of multiple files. When you run ``hasura metadata export``, the following files will be generated in the ``metadata/`` directory of your project.

- ``version.yaml``: Contains the metadata version of the server
- ``tables.yaml``: Contains the metadata related to tables
- ``remote_schemas.yaml``: Contains the metadata related to :ref:`remote schemas<remote_schemas>`
- ``functions.yaml``: Contains the metadata related to :ref:`custom functions<custom_sql_functions>`
- ``allow_list.yaml``: Contains the metadata related to :ref:`allow lists<allow_list>`
- ``actions.yaml``: Contains the metadata related to :ref:`actions<actions>`
- ``actions.graphql``: Contains all the action definition and custom type definitions
