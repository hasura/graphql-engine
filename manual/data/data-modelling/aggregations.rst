==========================
Fetching aggregations data
==========================

You can do basic calculations on data from tables and create a view to show this calculated data.
Common examples would be counting the number of times something is present, calculating averages and
showing maximum and minimum values for something.

Let's understand how this works with an example. Consider the following schema

+----------------------------------------+----------------------------------------+
|Table                                   |Columns                                 |
+========================================+========================================+
|article                                 |id, title, content, author_id           |
+----------------------------------------+----------------------------------------+
|like                                    |user_id, article_id                     |
+----------------------------------------+----------------------------------------+

We have two tables ``article`` and ``like`` to store a list of articles and likes for these articles respectively.

Let's `create a view using SQL <https://www.postgresql.org/docs/current/static/sql-createview.html>`_ which calculates
the total number of likes for each article.

.. code-block:: SQL

  CREATE VIEW article_total_likes (article_id, total_likes) AS
    SELECT a.id, COUNT(l.user_id)
    FROM "article" a, "like" l
    WHERE a.id = l.article_id
    GROUP BY a.id;

Head to *Data > SQL* section of the :doc:`API console <../../api-console/index>` and run the above SQL command.
Ensure that you check the ``Track Table`` checkbox before running the query so that you can use Data APIs to query the view.

Next, let's :ref:`create a relationship <relationship_without_fkey>` to this view on the ``article`` table.

- Relationship Type will be ``Object Relationship``
- Relationship Name can be "total_likes"
- Configuration: ``id :: article_total_likes -> article_id``

Query the data
--------------

You can now fetch the total likes from the ``article`` table

.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

        query fetch_article {
          article {
           id
           content
           total_likes {
             count
           }
          }
        }

   .. tab:: JSON API

      .. code-block:: http

         POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
         Content-Type: application/json
         Authorization: Bearer <auth-token> # optional if cookie is set
         X-Hasura-Role: <role>  # optional. Required if request needs particular user role

         {
             "type" : "select",
             "args" : {
                 "table" : "article",
                 "columns": [
                    "id",
                    "title",
                    {
                      "name": "total_likes",
                      "columns": ["count"]
                    }
                  ]
             }
         }
