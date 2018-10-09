.. meta::
  :keywords: computed fields, derived data, aggregations

Derived data in queries
=======================
GraphQL’s "select" query language is designed to be simple yet powerful. But there are still certain
queries that you cannot express with a GraphQL query. For example, getting the average rating of articles by an
author.

To express complex queries for derived data like aggregations or custom joins etc., use SQL, which is designed for this
purpose. If you can express your aggregation query in SQL, define a view with it and then use the newly created
type in the GraphQL query.

For example, let’s see how to fetch the average article rating for each author in our author/article schema:

1) Create a view
----------------
Open the Hasura console and head to the ``Data -> SQL`` tab.

A view that averages the rating of articles for each author can be created using the following SQL query:

.. code-block:: SQL

  CREATE VIEW author_average_rating AS
  SELECT author.id, avg(article.rating)
  From author, article
  WHERE author.id = article.author_id
  GROUP BY author.id

2) Add a relationship
---------------------
Relationships are generally defined using foreign-key constraints. However, you cannot define foreign-key constraints
on/to views. So, in these cases, we can define a relationship without using a foreign-key as described
:doc:`here <../schema/relationships/index>`.

Create an object relationship, ``avg_rating``, by mapping ``author::id -> author_average_rating::id``.

3) Query using the relationship
-------------------------------
Now that we have the relationship between the ``author`` table and the ``author_average_rating`` view has been set
up, we can query the aggregate data in ``author_average_rating`` as with any regular nested object.

Fetch a list of authors along with their average article rating:

.. graphiql::
  :view_only:
  :query:
    query {
      author {
        id
        name
        avg_rating {
          avg
        }
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 1,
            "name": "Justin",
            "avg_rating": {
              "avg": 2.5
            }
          },
          {
            "id": 2,
            "name": "Beltran",
            "avg_rating": {
              "avg": 3
            }
          },
          {
            "id": 3,
            "name": "Sidney",
            "avg_rating": {
              "avg": 2.6666666666666665
            }
          },
          {
            "id": 4,
            "name": "Anjela",
            "avg_rating": {
              "avg": 2.5
            }
          }
        ]
      }
    }

This example can be easily extended to cover any use-case involving an SQL aggregate function that you may want to use.
