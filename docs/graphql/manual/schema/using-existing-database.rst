.. meta::
   :description: Set up a Hasura GraphQL schema with an existing database
   :keywords: hasura, docs, schema, existing database

.. _schema_existing_db:

Setting up a GraphQL schema using an existing database
======================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

When you have an existing database with a schema already present, you don't need to create tables or views or run
DDL queries through the Hasura console.

All you need to do is indicate to Hasura GraphQL engine which tables and views you want to expose over GraphQL and
how they are connected to each other so that you can query them as a "graph".

Step 1: Track tables/views
--------------------------

Tracking a table or a view means telling Hasura GraphQL engine that you want to expose that table/view over GraphQL.

To track a table or a view:
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

   #. Head to the ``Data -> Schema`` section of the console.
   #. Under the heading ``Untracked Tables/Views``, click on the ``Track`` button next to the table/view name.

  .. tab:: Via CLI

   To track the table and expose it over the GraphQL API, add it to the ``tables.yaml`` file in the ``metadata`` directory as follows:

   .. code-block:: yaml
      :emphasize-lines: 1-3

      - table:
         schema: public
         name: <table name>

   Then apply the metadata by running:

   .. code-block:: bash

      hasura metadata apply

  .. tab:: Via API

   To track a table and expose it over the GraphQL API, make the following API call to the :ref:`track_table API <track_table>`:

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

To track all tables and views present in the database:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

   #. Head to the ``Data -> Schema`` section of the console.
   #. Under the heading ``Untracked Tables/Views``, click the ``Track All`` button.

  .. tab:: Via CLI

   To track all tables and expose them over the GraphQL API, add them to the ``tables.yaml`` file in the ``metadata`` directory as follows:

   .. code-block:: yaml
      :emphasize-lines: 1-3

      - table:
         schema: public
         name: <table name>

   To automate this, add the tables in a loop through a script.

   Then apply the metadata by running:

   .. code-block:: bash

      hasura metadata apply

  .. tab:: Via API 

   To track all tables and expose them over the GraphQL API, use the :ref:`track_table API <track_table>`:

   .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
         "type": "track_table",
         "args": {
            "schema": "public",
            "name": "article"
         }
      }

   To automate this, add the tables in a loop through a script.

Step 2: Track foreign-keys
--------------------------

Tracking a foreign-key means creating a :ref:`relationship <relationships>` between the tables involved in the
foreign-key.

To track a foreign-key between two tables in the database:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Via console

      #. Head to the ``Data -> Schema`` section of the console.
      #. Click on a table involved in the foreign-key and head to the ``Relationships`` tab.
      #. You should see a suggested relationship based on the foreign-key. Click ``Add``, give a name to your relationship
         (this will be the name of the :ref:`nested object <nested_object_queries>` in the GraphQL query), and
         hit ``Save`` to create the relationship.
      #. Repeat with the other table involved in the foreign-key.

   .. tab:: Via CLI

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
               foreign_key_constraint_on: <reference key>

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
                  column: <reference key>
                  table:
                     schema: public
                     name: <reference table name>

      Then apply the metadata by running:

      .. code-block:: bash

         hasura metadata apply

   .. tab:: Via API

      **Object relationship**

      You can create an object relationship by using the :ref:`create_object_relationship API <create_object_relationship>`:

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
                  "foreign_key_constraint_on": "<reference key>"
               }
            }
         }

      **Array relationship**

      You can create an array relationship by using the :ref:`create_array_relationship API <create_array_relationship>`:

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
                        "column" : "<reference key>"
                     }
               }
            }
         }

To track all the foreign-keys of all tables in the database:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. rst-class:: api_tabs
.. tabs::

   .. tab:: Via console

      #. Head to the ``Data -> Schema`` section of the console.
      #. Under the heading ``Untracked foreign-key relations``, click the ``Track All`` button to automatically
         create relationships based on the foreign-keys.

   .. tab:: Via CLI

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
               foreign_key_constraint_on: <reference key>

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
                  column: <reference key>
                  table:
                     schema: public
                     name: <reference table name>

      To automate this, add the relationships in a loop through a script.

      Then apply the metadata by running:

      .. code-block:: bash

         hasura metadata apply

   .. tab:: Via API

      To track all relationships and expose them over the GraphQL API, there are two APIs you can use depending on the kind of relationship.

      **Object relationship**

      You can create an object relationship by using the :ref:`create_object_relationship API <create_object_relationship>`:

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
                     "foreign_key_constraint_on": "<reference key>"
               }
            }
         }

      **Array relationship**

      You can create an array relationship by using the :ref:`create_array_relationship API <create_array_relationship>`:

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
                     "column" : "<reference key>"
                  }
               }
            }
         }

      To automate this, add the relationships in a loop through a script.

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

