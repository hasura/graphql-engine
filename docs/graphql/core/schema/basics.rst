.. meta::
   :description: Schema design basics in Hasura
   :keywords: hasura, docs, schema, basics

.. _schema_basics:

Basics & workflows
==================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

This page introduces the basics of how to manipulate and consume your GraphQL schema, and the workflows that can be followed to do so.

Workflows
---------

When developing with Hasura, there are different possible workflows that can be followed, depending on your requirements.

Console
^^^^^^^

The Hasura console is the UI that can be used to build your schema. For example, you can create tables and relationships, as well as perform different types of data validation.

.. thumbnail:: /img/graphql/manual/schema/hasura-console.png
   :alt: Hasura console

CLI
^^^

The CLI workflow is used if you'd like to work with migrations. This can be useful if you have several environments, such as development, staging and production.

Setup
*****

1. :ref:`Install the Hasura CLI <install_hasura_cli>`.

2. `Set up a project directory <https://hasura.io/docs/1.0/graphql/manual/migrations/migrations-setup.html#step-2-set-up-a-project-directory>`__.

3. If you'd like to work with the Hasura console, `use the console from the CLI <https://hasura.io/docs/1.0/graphql/manual/migrations/migrations-setup.html#step-4-use-the-console-from-the-cli>`__.

The newly created project directory contains a ``migrations`` folder and a ``metadata`` folder.

Migrations
**********

Migrations are used if the underlying Postgres database is to be manipulated. For example, migrations are created when a new table is created or altered.
The ``migrations`` directory contains one directory for each migration that has been created. 
If you use the console using the Hasura CLI, migrations get automatically added to this folder. However, you can also add a migration :ref:`manually <manual_migrations>`.

Inside the migrations directory, there are two files:

- ``up.sql``: This file contains the SQL statement used to do the required database manipulation.
- ``down.sql``: This file contains the SQL statement used to :ref:`roll back <roll_back_migrations>` the statement defined in ``up.sql``. 

.. note::

  Depending on the complexity of the statement in ``up.sql``, the ``down.sql`` file will not always be automatically filled by Hasura. You might want to add it yourself.

Learn more about the :ref:`migrations file format <migration_file_format_v2>`.

Metadata
********

Metadata in Hasura is all information and configuration of the schema that is on top of the Postgres database. 
The ``metadata`` directory contains several files related to metadata. They include ``tables.yaml`` (containing all information around tables and views, such as relationships, permissions etc.), ``functions.yaml`` (containing all tracked custom functions) and others.

When working on the Hasura console, the metadata will be updated in these files. However, it's also possible to update metadata manually in their respective files.

Learn more about the :ref:`metadata file format <metadata_file_format_v1>`.

API
^^^

The API can be used if the database or the GraphQL schema is accessed programatically i.e. through a computer program.

- The :ref:`schema & metadata APIs <metadata_apis>` are used for performing schema manipulations.
- The :ref:`GraphQL API <api_reference_graphql>` is used for querying or mutating GraphQL tables.

Examples
--------

Schema manipulation
^^^^^^^^^^^^^^^^^^^

To illustrate the three different workflows presented above, we'll look at an example of creating and tracking the following table:

.. code-block:: sql

  author (
    id SERIAL PRIMARY KEY,
    name TEXT
    rating INT
  )

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    On the Hasura console, navigate to the ``Data`` tab and then click on the button ``Create table``.

    .. thumbnail:: /img/graphql/manual/schema/console-create-table.png
      :alt: Create a table with Hasura console  

    .. note::

      When adding a table over the Hasura console, it's tracked by default.

  .. tab:: CLI

    :ref:`Create a migration manually <manual_migrations>` and add the following SQL statement to the ``up.sql`` file:

    .. code-block:: sql

      CREATE TABLE author(id serial NOT NULL, name text NOT NULL, rating integer);

    Add the following statement to the ``down.sql`` file in case you need to :ref:`roll back <roll_back_migrations>` the above statement:

    .. code-block:: sql

      DROP TABLE author;

    Apply the migration by running:

    .. code-block:: bash

      hasura migrate apply

    To track the table and expose it over the GraphQL API, edit the ``tables.yaml`` file in the ``metadata`` directory as follows:

    .. code-block:: yaml
       :emphasize-lines: 1-3

        - table:
            schema: public
            name: author

    Apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: API

    You can create a table by making an API call to the :ref:`run_sql metadata API <run_sql>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
          "sql": "CREATE TABLE author(id serial NOT NULL, name text NOT NULL, rating integer);"
        }
      }

    To track the table and expose it over the GraphQL API, make the following API call to the :ref:`track_table metadata API <track_table>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "track_table",
        "args": {
          "schema": "public",
          "name": "author"
        }
      }

GraphQL queries
^^^^^^^^^^^^^^^

You can perform GraphQL queries either from the Hasura console or over the GraphQL API.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    On the console, you can perform queries in the GraphiQL field:

    .. graphiql::
      :view_only:
      :query:
        query {
          author {
            id
            name
            rating
          }
        }
      :response:
        {
          "data": {
            "author": [
              {
                "id": 1,
                "name": "Jenny",
                "rating": 10
              },
              {
                "id": 2,
                "name": "John",
                "rating": 9
              },
              {
                "id": 3,
                "name": "Betty",
                "rating": 8
              }
            ]
          }
        }

  .. tab:: API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { author { id name rating }}"
      }
