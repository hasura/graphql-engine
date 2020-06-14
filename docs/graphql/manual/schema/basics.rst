.. meta::
   :description: Schema design basics in Hasura
   :keywords: hasura, docs, schema, basics

.. _schema_basics:

Schema design basics
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The Hasura GraphQL engine creates GraphQL schema object types and corresponding query/mutation fields with resolvers
automatically as we create tables/views in the Postgres database.

Let's take a look at how to create tables using the Hasura console, a UI tool meant for doing exactly this, and the
GraphQL schema it generates.

Let's say we want to create two simple tables for an article/author schema:

.. code-block:: sql

  author (
    id SERIAL PRIMARY KEY,
    name TEXT
  )

  article (
    id SERIAL PRIMARY KEY,
    title TEXT,
    content TEXT,
    rating INT,
    author_id INT
  )

.. _create-tables:

Create tables
-------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    Open the Hasura console and head to the ``Data`` tab and click the ``Create Table`` button to open up an interface to
    create tables.

    As soon as a table is created, the corresponding GraphQL schema types and query/mutation resolvers will be
    automatically generated.

    For example, here is the schema for the ``article`` table in this interface:

    .. thumbnail:: /img/graphql/manual/schema/create-table-graphql.png
      :alt: Schema for an article table

  .. tab:: Via CLI

    You can :ref:`create a migration manually <manual_migrations>` with the following statement:

    .. code-block:: sql

      CREATE TABLE article(id serial NOT NULL, title text NOT NULL, content text NOT NULL, rating integer NOT NULL, author_id serial NOT NULL, PRIMARY KEY (id));

    Then apply the migration by running:

    .. code-block:: bash

      hasura migrate apply

    To track the table and expose it over the GraphQL API, edit the ``tables.yaml`` file in the ``metadata`` directory as follows:

    .. code-block:: yaml
       :emphasize-lines: 4-6

        - table:
            schema: public
            name: author
        - table:
            schema: public
            name: article

    Then apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: Via API

    You can create a table by making an API call to the :ref:`run_sql API <run_sql>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
          "sql": "CREATE TABLE article(id serial NOT NULL, title text NOT NULL, content text NOT NULL, rating integer NOT NULL, author_id serial NOT NULL, PRIMARY KEY (id));"
        }
      }

    To track the table and expose it over the GraphQL API, make the following API call to the :ref:`track_table API <track_table>`:

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


The following object type and query/mutation fields are generated for the ``article`` table we just created:

.. code-block:: graphql

  # Object type
  type Article {
    id: Int
    title: String
    content: String
    rating: Int
    author_id: Int
  }

  # Query field
  article (
    where: article_bool_exp
    limit: Int
    offset: Int
    order_by: [article_order_by!]
  ): [article!]!

  # insert/upsert mutation field
  insert_article (
    objects: [article_insert_input!]!
    on_conflict: article_on_conflict
  ): article_mutation_response

  # update mutation field
  update_article (
    where: article_bool_exp!
    _inc: article_inc_input
    _set: article_set_input
  ): article_mutation_response

  # delete mutation field
  delete_article (
    where: article_bool_exp!
  ): article_mutation_response

See the :ref:`query <graphql_api_query>` and :ref:`mutation <graphql_api_mutation>`
API references for the full specifications.

You can insert some sample data into the tables using the ``Insert Row`` tab of the created tables.

Try out basic GraphQL queries
-----------------------------

At this point, you should be able to try out basic GraphQL queries/mutations on the newly created tables (*you may want to add some sample data into the tables first*).

Here are a couple of examples of GraphQL requests:

- Query all rows in the ``article`` table

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console 

    .. graphiql::
      :view_only:
      :query:
        query {
          article {
            id
            title
            author_id
          }
        }
      :response:
        {
          "data": {
            "article": [
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

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { article { id title author_id } }"
      }

- Insert data in the ``author`` table

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

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

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "mutation add_author { insert_author(objects: [ { name: \"Jane\" } ]) { affected_rows returning { id name }} }"
      }
    
Note that the author's ``id`` does not need to passed as an input as it is of type ``serial`` (auto incrementing integer).
