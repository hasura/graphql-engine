.. meta::
   :description: Set up a Hasura GraphQL schema with an existing database
   :keywords: hasura, docs, schema, existing database

.. _schema_existing_db:

Setting up a GraphQL schema using an existing database
======================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

When you have an existing database with a schema already present, you don't need to create tables or views or run
DDL queries through the Hasura console.

All you need to do is indicate to Hasura GraphQL engine which tables and views you want to expose over GraphQL and
how they are connected to each other so that you can query them as a "graph".

Step 1: Track tables/views
--------------------------

Tracking a table or a view means telling Hasura GraphQL engine that you want to expose that table/view over GraphQL.

To track a table or a view
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

   #. Head to the ``Data -> Schema`` section of the console.
   #. Under the heading ``Untracked Tables/Views``, click on the ``Track`` button next to the table/view name.

  .. tab:: CLI

   To track the table and expose it over the GraphQL API, add it to the ``tables.yaml`` file in the ``metadata`` directory as follows:

   .. code-block:: yaml
      :emphasize-lines: 1-3

      - table:
         schema: public
         name: <table name>

   Apply the metadata by running:

   .. code-block:: bash

      hasura metadata apply

  .. tab:: API

   To track a table and expose it over the GraphQL API, use the :ref:`track_table metadata API <track_table>`:

   .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
         "type": "track_table",
         "args": {
            "schema": "public",
            "name": "<table name>"
         }
      }

To track all tables and views present in the database
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

   #. Head to the ``Data -> Schema`` section of the console.
   #. Under the heading ``Untracked Tables/Views``, click the ``Track All`` button.

  .. tab:: CLI

   To track all tables and expose them over the GraphQL API, add them to the ``tables.yaml`` file in the ``metadata`` directory as follows:

   .. code-block:: yaml
      :emphasize-lines: 1-6

      - table:
         schema: public
         name: <table-name-1>
      - table:
         schema: public
         name: <table-name-2>

   To automate this, you could add the tables in a loop through a script.

   Apply the metadata by running:

   .. code-block:: bash

      hasura metadata apply

  .. tab:: API 

   To track all tables and expose them over the GraphQL API, use the :ref:`track_table metadata API <track_table>`:

   .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "bulk",
        "args": [
          {
             "type": "track_table",
             "args": {
                "schema": "public",
                "name": "<table-name-1>"
             }
          },
          {
             "type": "track_table",
             "args": {
                "schema": "public",
                "name": "<table-name-2>"
             }
          }
        ]
      }

   To automate this, you could add the ``track_table`` requests to the ``bulk`` request in a loop through a script.

Step 2: Track foreign-keys
--------------------------

Tracking a foreign-key means creating a :ref:`relationship <table_relationships>` between the tables involved in the
foreign-key.

To track a foreign-key between two tables in the database
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Console

      #. Head to the ``Data -> Schema`` section of the console.
      #. Click on a table involved in the foreign-key and head to the ``Relationships`` tab.
      #. You should see a suggested relationship based on the foreign-key. Click ``Add``, give a name to your relationship
         (this will be the name of the :ref:`nested object <nested_object_queries>` in the GraphQL query), and
         hit ``Save`` to create the relationship.
      #. Repeat with the other table involved in the foreign-key.

   .. tab:: CLI

      To track a relationship and expose it over the GraphQL API, add it to the ``tables.yaml`` file in the ``metadata`` directory as follows:

      **Object relationship**

      .. code-block:: yaml
         :emphasize-lines: 4-7

         - table:
             schema: public
             name: <table name>
           object_relationships:
           - name: <relationship name>
             using:
               foreign_key_constraint_on: <reference column>

      **Array relationship**

      .. code-block:: yaml
         :emphasize-lines: 4-11

         - table:
               schema: public
               name: <table name>
            array_relationships:
            - name: <relationship name>
               using:
               foreign_key_constraint_on:
                  column: <reference column>
                  table:
                     schema: public
                     name: <reference table name>

      Apply the metadata by running:

      .. code-block:: bash

         hasura metadata apply

   .. tab:: API

      **Object relationship**

      You can create an object relationship by using the :ref:`create_object_relationship metadata API <create_object_relationship>`:

      .. code-block:: http

         POST /v1/query HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
            "type": "create_object_relationship",
            "args": {
               "table": "<table name>",
               "name": "<relationship name>",
               "using": {
                  "foreign_key_constraint_on": "<reference column>"
               }
            }
         }

      **Array relationship**

      You can create an array relationship by using the :ref:`create_array_relationship metadata API <create_array_relationship>`:

      .. code-block:: http

         POST /v1/query HTTP/1.1
         Content-Type: application/json
         X-Hasura-Role: admin

         {
            "type": "create_array_relationship",
            "args": {
               "table": "<table name>",
               "name": "<relationship name>",
               "using": {
                  "foreign_key_constraint_on" : {
                     "table" : "<reference table name>",
                     "column" : "<reference column>"
                  }
               }
            }
         }

To track all the foreign-keys of all tables in the database
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Console

      #. Head to the ``Data -> Schema`` section of the console.
      #. Under the heading ``Untracked foreign-key relations``, click the ``Track All`` button to automatically
         create relationships based on the foreign-keys.

   .. tab:: CLI

      To track all relationships and expose them over the GraphQL API, add them to the ``tables.yaml`` file in the ``metadata`` directory as follows:

      **Object relationship**

      .. code-block:: yaml
         :emphasize-lines: 4-7

         - table:
             schema: public
             name: <table name>
           object_relationships:
           - name: <relationship name>
             using:
               foreign_key_constraint_on: <reference column>

      **Array relationship**

      .. code-block:: yaml
         :emphasize-lines: 4-11

         - table:
             schema: public
             name: <table name>
           array_relationships:
           - name: <relationship name>
             using:
               foreign_key_constraint_on:
                 column: <reference column>
                 table:
                   schema: public
                   name: <reference table name>

      To automate this, you could add the relationships in a loop through a script.

      Apply the metadata by running:

      .. code-block:: bash

         hasura metadata apply

   .. tab:: API

      You can create multiple relationships by using the :ref:`create_object_relationship metadata API <create_object_relationship>`
      and the :ref:`create_array_relationship metadata API <create_array_relationship>`:

      .. code-block:: http

        POST /v1/query HTTP/1.1
        Content-Type: application/json
        X-Hasura-Role: admin

        {
          "type": "bulk",
          "args": [
            {
              "type": "create_object_relationship",
              "args": {
                "table": "<table name>",
                "name": "<relationship name>",
                "using": {
                  "foreign_key_constraint_on": "<reference column>"
                }
              }
            },
            {
              "type": "create_array_relationship",
              "args": {
                "table": "<table name>",
                "name": "<relationship name>",
                "using": {
                  "foreign_key_constraint_on" : {
                    "table" : "<reference table name>",
                    "column" : "<reference column>"
                  }
                }
              }
            }
          ]
        }

      To automate this, you could add the create relationships requests to the ``bulk`` request in a loop through a script.

.. admonition:: Relationship nomenclature

  In this case, Hasura GraphQL engine will **automatically generate relationship names** (the names of the
  :ref:`nested objects <nested_object_queries>` in the GraphQL query) based on the table names and the
  foreign-key names.

  The name is generated in the following format:

  - For object relationships: ``singular of foreignTableName``
  - For array relationships: ``plural of foreignTableName``

  For example, for the foreign-key ``article.author_id -> author.id``, the relationship names will be
  ``author`` for ``article`` table and ``articles`` for ``author`` table.

  In case a field with the generated name already exists, a new name will be generated of the form:
  ``camel case of (singular/plural of foreignTableName + _by_ + foreignKeyColumnName)``

  Note that, **this is just  an arbitrary naming convention** chosen by Hasura to ensure the generation of unique
  relationship names. You can choose to rename your relationships to anything you wish. You can **change the
  relationship names** with a name of your choice as shown in :ref:`rename_relationships`.

