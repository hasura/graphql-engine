Schema design basics
====================
Let's take a look at how to create tables using the Console, a UI tool meant for doing exactly this. We'll use a typical author/articles schema as a reference for all the following examples.

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

Click on the "Create Table" button in the **Data** section to open up an interface to create tables. For e.g. here's the schema for the ``author`` table in this interface:

.. image:: ../../../img/graphql/manual/schema/create-table-graphql.png

As soon as a table is created, the corresponding GraphQL schema and resolvers are automatically created/updated. For e.g. the following *query* and *mutation* fields are generated for the tables we just created:

.. code-block:: none

    author (
      where: author_bool_exp
      limit: Int
      offset: Int
      order_by: [String]
    ): [author]

.. code-block:: none

    insert_article (
        objects: [article_input!]
        on_conflict: conflict_clause
    ): article_mutation_response

.. note::
    
    If you are connecting to a database instance that already has data, you will need to explicitly *track* those tables using the console in the **Schema** section. You will be shown a list of untracked tables from which you can choose the tables to be included in the GraphQL schema.



Try basic GraphQL queries
-------------------------
At this point, you should be able to try out basic GraphQL queries/mutations on the newly created tables using the API Explorer in the console(*you may want to add some test data in the tables*). 

.. note::
    
    You can either use the admin token to run them or modify the permissions for these tables to temporarily allow anonymous access to data in the **Permissions** tab of each table.

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
            "id": 3,
            "title": "some title",
            "author_id": 28
          },
          {
            "id": 4,
            "title": "some title",
            "author_id": 5
          },
          {
            "id": 8,
            "title": "some title",
            "author_id": 6
          }
        ]
      }
    }

- Insert data in the ``author`` table

.. graphiql::
  :view_only: true
  :query:
    mutation add_author {
      insert_author (
        objects: [
          {id: 2121, name:"Paul Graham"}
        ]
      )
      {
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

You can try out the examples :doc:`here <../queries/index>` (*except nested object queries, for which you'll need to connect your tables- see the next section*).
