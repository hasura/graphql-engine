Aggregations in queries
=======================
GraphQL’s query language for a ``select`` query is designed to be simple yet powerful. There will still be queries that you cannot express with the ``select`` query. For example, getting the number of likes for each article. To express complex queries like aggregations (or custom joins etc.), use SQL, which is designed for this purpose. If you can express your aggregation query in SQL, define a view with it and then use the newly created type in the GraphQL query.

Let’s see an example of how to do that with our reference schema, assuming we also have a table ``article_like`` with a row for each unique like for an article (columns are id, article_id, date_liked, etc.). Our aim is to get a total number of likes per article.

Create a view
-------------
A view that sums up the number of likes for each article is to be created, using the following SQL query:

.. code-block:: SQL

    CREATE VIEW article_like_count AS
    SELECT article_id, COUNT(author_id) AS like_count
    FROM article_like
    GROUP BY article_id;

Add a relationship
------------------
Relationships are generally defined using foreign key constraints. However, you cannot define foreign key constraints on/to views. So, in these cases, we can define a relationship without using a foreign keys as described `here <https://docs.hasura.io/0.15/manual/data/relationships/create-relationships.html>_` (we create an object relationship, ``total_likes``, by mapping ``article``:``id`` -> ``article_like_count``:``article_id``).

Query using the relationship
----------------------------
Now that we have the relationship between the `article` table and the ``total_likes`` view has been set up, we can query the aggregate data in ``total_likes`` as with any regular nested object.

Example: aggregate data
^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of articles along with the total number of likes received by each one:

.. graphiql::
   :query:
        query  {
            article {
                id
                rating
                total_likes{
                    like_count
                }
            }
        }
   :response:
        {
            "data": {
                "article": [
                    {
                        "id": 3,
                        "rating": 4,
                        "total_likes": {
                        "like_count": 2
                        }
                    },
                    {
                        "id": 4,
                        "rating": 4,
                        "total_likes": {
                        "like_count": 1
                        }
                    },
                    {
                        "id": 10,
                        "rating": 5,
                        "total_likes": {
                        "like_count": 2
                        }
                    }
                ]
            }
        }

This example can be easily extended to cover any use-case involving a SQL aggregate function that you may want to use.