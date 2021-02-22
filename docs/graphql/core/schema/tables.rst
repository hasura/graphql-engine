.. meta::
   :description: GraphQL tables in Hasura
   :keywords: hasura, docs, schema, tables

.. _schema_tables:

Tables basics
=============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Adding tables allows you to define the GraphQL types of your schema including their corresponding fields. 

.. _create_tables:

Creating tables
---------------

Let's say we want to create two simple tables for ``articles`` and ``author`` schema:

.. code-block:: sql

  author (
    id SERIAL PRIMARY KEY,
    name TEXT
  )

  articles (
    id SERIAL PRIMARY KEY,
    title TEXT,
    content TEXT,
    rating INT,
    author_id INT
  )

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    Open the Hasura console and head to the ``Data`` tab and click on the button on the left side bar to open up an interface to
    create tables.

    For example, here is the schema for the ``articles`` table in this interface:

    .. thumbnail:: /img/graphql/core/schema/create-table-graphql.png
      :alt: Schema for an article table

  .. tab:: CLI

    1. :ref:`Create a migration manually <manual_migrations>` and add the following SQL statement to the ``up.sql`` file:

       .. code-block:: sql

         CREATE TABLE articles(id serial NOT NULL, title text NOT NULL, content text NOT NULL, rating integer NOT NULL, author_id serial NOT NULL, PRIMARY KEY (id));

    2. Add the following statement to the ``down.sql`` file in case you need to :ref:`roll back <roll_back_migrations>` the above statement:

       .. code-block:: sql

          DROP TABLE articles;

    3. Apply the migration by running:

       .. code-block:: bash

         hasura migrate apply

  .. tab:: API

    You can create a table by making an API call to the :ref:`run_sql metadata API <run_sql>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
          "sql": "CREATE TABLE articles(id serial NOT NULL, title text NOT NULL, content text NOT NULL, rating integer NOT NULL, author_id serial NOT NULL, PRIMARY KEY (id));"
        }
      }

Tracking tables
---------------

Tables can be present in the underlying Postgres database without being exposed over the GraphQL API.
In order to expose a table over the GraphQL API, it needs to be **tracked**.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    When a table is created via the Hasura console, it gets tracked by default.

    You can track any existing tables in your database from the ``Data -> Schema`` page:

    .. thumbnail:: /img/graphql/core/schema/schema-track-tables.png
       :alt: Track table

  .. tab:: CLI

    1. To track the table and expose it over the GraphQL API, edit the ``tables.yaml`` file in the ``metadata`` directory as follows:

       .. code-block:: yaml
         :emphasize-lines: 4-6

          - table:
              schema: public
              name: authors
          - table:
              schema: public
              name: articles

    2. Apply the metadata by running:

       .. code-block:: bash

         hasura metadata apply

  .. tab:: API

    To track the table and expose it over the GraphQL API, make the following API call to the :ref:`track_table metadata API <track_table>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "track_table",
        "args": {
          "schema": "public",
          "name": "articles"
        }
      }

Generated GraphQL schema types
------------------------------

As soon as a table is created and tracked, the corresponding GraphQL schema types
and query/mutation fields will be automatically generated.

The following object type is generated for the ``articles``
table we just created and tracked:

.. code-block:: graphql

  # Object type
  type Articles {
    id: Int
    title: String
    content: String
    rating: Int
    author_id: Int
  }

Let's analyze the above type:

- ``Articles`` is the name of the type
- ``id``, ``title``, ``content``, ``rating`` and ``author_id`` are fields of the ``Articles`` type
- ``Int`` and ``String`` are types that fields can have

The following query/mutation fields are generated for the ``articles``
table we just created and tracked:

.. code-block:: graphql

  # Query field
  articles (
    where: articles_bool_exp
    limit: Int
    offset: Int
    order_by: [articles_order_by!]
  ): [articles!]!

  # insert/upsert mutation field
  insert_articles (
    objects: [articles_insert_input!]!
    on_conflict: articles_on_conflict
  ): articles_mutation_response

  # update mutation field
  update_articles (
    where: articles_bool_exp!
    _inc: articles_inc_input
    _set: articles_set_input
  ): articles_mutation_response

  # delete mutation field
  delete_articles (
    where: articles_bool_exp!
  ): articles_mutation_response

These auto-generated fields will allow you to query and mutate data
in our table.

See the :ref:`query <graphql_api_query>` and :ref:`mutation <graphql_api_mutation>`
API references for the full specifications.

Try out basic GraphQL requests
------------------------------

At this point, you should be able to try out basic GraphQL queries/mutations on
the newly created tables from the GraphiQL tab in the console. *(You may want to add some
sample data into the tables first)*

- Query all rows in the ``articles`` table:

  .. graphiql::
    :view_only:
    :query:
      query {
        articles {
          id
          title
          author_id
        }
      }
    :response:
      {
        "data": {
          "articles": [
            {
              "id": 1,
              "title": "sit amet",
              "author_id": 4
            },
            {
              "id": 2,
              "title": "a nibh",
              "author_id": 2
            },
            {
              "id": 3,
              "title": "amet justo morbi",
              "author_id": 4
            },
            {
              "id": 4,
              "title": "vestibulum ac est",
              "author_id": 5
            }
          ]
        }
      }

- Insert data in the ``author`` table:

  .. graphiql::
    :view_only:
    :query:
      mutation add_author {
        insert_author(
          objects: [
            { name: "Jane" }
          ]
        ) {
            affected_rows
            returning {
              id
              name
            }
          }
      }
    :response:
      {
        "data": {
          "insert_author": {
            "affected_rows": 1,
            "returning": [
              {
                "id": 11,
                "name": "Jane"
              }
            ]
          }
        }
      }

.. note::

  author's ``id`` does not need to be passed as an input as it is of type ``serial`` (auto incrementing integer).
