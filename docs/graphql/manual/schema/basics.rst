Schema design basics
====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

The Hasura GraphQL engine creates GraphQL schema object types and corresponding query/mutation fields with resolvers
automatically as we create tables/views in the Postgres database.

Let's take a look at how to create tables using the Hasura console, a UI tool meant for doing exactly this, and what
GraphQL schema it generates.

Let's say we want to create two simple tables for an article/author schema:

.. code-block:: sql

  author (
    id INT PRIMARY KEY,
    name TEXT
  )

  article (
    id INT PRIMARY KEY,
    title TEXT,
    content TEXT,
    rating INT,
    author_id INT
  )

.. _create-tables:

Create tables
-------------

Open the Hasura console and head to the ``Data`` tab and click the ``Create Table`` button to open up an interface to
create tables.

As soon as a table is created, the corresponding GraphQL schema types and query/mutation resolvers will be
automatically generated.

For example, here is the schema for the ``article`` table in this interface:

.. thumbnail:: ../../../img/graphql/manual/schema/create-table-graphql.png

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

See the :doc:`query <../api-reference/graphql-api/query>` and :doc:`mutation <../api-reference/graphql-api/mutation>`
API references for the full specifications

You can insert some sample data into the tables using the ``Insert Row`` tab of the created tables.

Try basic GraphQL queries
-------------------------
At this point, you should be able to try out basic GraphQL queries/mutations on the newly created tables using the
console ``GraphiQL`` tab (*you may want to add some sample data into the tables first*).

Here are a couple of examples:

- Query all rows in the ``article`` table

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

- Insert data in the ``author`` table

.. graphiql::
  :view_only:
  :query:
    mutation add_author {
      insert_author(
        objects: [
          {id: 11, name: "Jane"}
        ]
      ) {
        affected_rows
      }
    }
  :response:
    {
      "data": {
        "insert_author": {
          "affected_rows": 1
        }
      }
    }
