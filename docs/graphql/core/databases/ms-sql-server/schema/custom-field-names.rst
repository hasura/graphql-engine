.. meta::
   :description: Use custom field names for MS SQL Server in Hasura
   :keywords: hasura, docs, ms sql server, schema, custom field name

.. _ms_sql_server_custom_field_names:

MS SQL Server: Customise auto-generated field names
===================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Hasura auto-generates GraphQL field names based on your database table and column names. If you'd like to change the defaults,
it is possible to override the auto-generated table and column field names exposed over the GraphQL API.

..  admonition:: Supported from

  This feature is supported in versions ``v1.0.0-beta.8`` and later.

Expose columns with a different name in the GraphQL API
-------------------------------------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    Console support coming soon
  ..
      Head to the ``Data -> [table-name] -> Modify``. On the relevant field, click ``Edit`` and change the GraphQL field name to a name of your choice.

     .. thumbnail:: /img/graphql/core/schema/custom-field-name-column.png
        :alt: Customise GraphQL field name

  .. tab:: CLI

    You can customize auto-generated field names in the ``tables.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml
       :emphasize-lines: 4-6

        - table:
            schema: dbo
            name: author
          configuration:
            custom_column_names:
              addr: address

    Apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: API

    A custom field name can be set for a column via the following 2 methods:

    1. Passing a :ref:`table_config` with the :ref:`CustomColumnNames` to the :ref:`mssql_set_table_customization` API while tracking a table:

       .. code-block:: http

         POST /v1/metadata HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
           "type": "mssql_set_table_customization",
           "args": {
             "table": "authors",
             "configuration": {
               "custom_column_names": {
                 "id": "AuthorId"
               }
             }
           }
         }


    2. Customization can be done at the time of creation using :ref:`mssql_track_table` API also.
  

Expose table root fields with a different name in the GraphQL API
-----------------------------------------------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    Console support coming soon

    ..
      Head to the ``Data -> [table-name] -> Modify``. Click the ``Edit`` button in the ``Custom GraphQL Root Fields`` section and define names over which you'd like to expose the table root fields.

     .. thumbnail:: /img/graphql/core/schema/custom-field-name-root-fields.png
        :alt: Customise GraphQL root field

  .. tab:: CLI
    
    You can expose table root fields with a different name in the GraphQL API in the ``tables.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml
       :emphasize-lines: 3-6

        - table:
            schema: dbo
            name: authors
          configuration:
            custom_root_fields:
              select: authors_aggregate

    After that, apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: API

    A custom field name can be set for a table root field via the following 2 methods:

    1. Passing a :ref:`table_config` with the :ref:`custom_root_fields` to the :ref:`mssql_set_table_customization` API while tracking a table:

       .. code-block:: http

         POST /v1/metadata HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
           "type": "mssql_set_table_customization",
           "args": {
             "table": "authors",
             "configuration": {
               "custom_column_names": {
                 "id": "AuthorId"
               }
               "custom_root_fields": {
                 "select": "authors",
                 "select_aggregate": "authors_aggregate"
               }
             }
           }
         }


    2. Customization can be done at the time of creation using :ref:`mssql_track_table` API also.
    
    