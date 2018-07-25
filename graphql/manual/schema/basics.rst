Schema design basics
====================
Let's take a look at how to create tables using the Hasura console, a UI tool meant for doing exactly this. We'll use a
typical author/articles schema as a reference for all the following examples.

Open the console
----------------
Run the following command using the Hasura CLI tool. 

.. code:: bash

   hasura console

Create tables
-------------
Let's say we want to create two simple tables:

- ``author`` with columns ``id``, ``name``

- ``article`` with columns ``id``, ``title``, ``content``, ``author_id``

Head to the ``Data`` tab and click the ``Create Table`` button to open up an interface to create tables.

For example, here is the schema for the ``article`` table in this interface:

.. image:: ../../../img/graphql/manual/schema/create-table-graphql.png

As soon as a table is created, the corresponding GraphQL schema and resolvers are automatically created/updated. For
e.g. the following *query* and *mutation* fields are generated for the tables we just created:

.. code-block:: none

    article (
      where: article_bool_exp
      limit: Int
      offset: Int
      order_by: [article_order_by!]
    ): [article]

.. code-block:: none

    insert_article (
        objects: [article_input!]
        on_conflict: conflict_clause
    ): article_mutation_response


Try basic GraphQL queries
-------------------------
At this point, you should be able to try out basic GraphQL queries/mutations on the newly created tables using the API
Explorer in the console(*you may want to add some test data in the tables*).

.. note::
    
    You can either use the admin token to run them or modify the permissions for these tables to temporarily allow
    anonymous access to data in the **Permissions** tab of each table.

Here are a couple of examples:

- Query all rows in the ``article`` table

.. graphiql::
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
  :view_only: true
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
