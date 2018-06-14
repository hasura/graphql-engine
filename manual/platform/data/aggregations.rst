Data aggregations
=================

The JSON/GraphQL based query language for ``select`` query is designed to be simple yet powerful. There will still be
queries that you cannot express with the ``select`` query. For example, getting the number of likes for each article.
Aggregations (like counting the number of likes or average rating value) are not supported in the ``select`` query
syntax. This is conscious decision we've made to keep the query language small.

To express complex queries like aggregations, window functions, custom joins etc, use SQL, which is designed for this
purpose. **If you can express your query in SQL, define a view with it**, you can use the data APIs on these views.

Let's see how we can get the likes of an article.

Let us `define a view in SQL <https://www.postgresql.org/docs/current/static/sql-createview.html>`_:

.. code-block:: sql

   CREATE VIEW article_like_count AS
      SELECT article_id, COUNT(author_id) AS like_count
      FROM article_like
      GROUP BY article_id;


Create the view in the database by running the above sql query as described :doc:`here <run-sql>`.

.. admonition:: Views are read only!

   Views are like read-only logical tables on the database.
   So that means that Data API requests to select will work, but you cannot
   insert/update/delete items from the view.

We've seen how we can get article`s likes using the ``data`` APIs. However, this additional information of each article
can be attached to the article table using an object relationship say, ``article_like_count``.

Relationships are generally defined using foreign key constraints. However, you cannot define foreign key
constraints on/to views. So, in these cases, we can define a relationship without using a foreign keys as described
:doc:`here <relationships/create-relationships>`.

Let's fetch articles ordered by the number of likes.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

         query fetch_articles_count {
           article (where: {is_published: {_eq: true}}, order_by: ["+article_like_count.like_count"]) {
              id
              title
              article_like_count {
                 like_count
              }
           }
         }

   .. tab:: JSON API

      .. code-block:: http

         POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
         Content-Type: application/json
         Authorization: Bearer <auth-token> # optional if cookie is set
         X-Hasura-Role: <role>  # optional. Pass if only specific user role has access

         {
           "type" : "select",
           "args" : {
             "table" : "article",
             "columns": [
               "id", "title",
                 {
                   "name" : "article_like_count",
                   "columns" : ["like_count"]
                 }
             ],
             "where": {"is_published": true},
             "order_by" : "+article_like_count.like_count"
           }
         }

