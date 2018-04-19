Data aggregations and  Views
============================

The JSON based query language in ``select`` query is designed to be simple yet powerful. There will be queries that you cannot express with the ``select`` query. For example, getting the number of likes for each article. Aggregations (like counting the number of likes) are not supported in the ``select`` query syntax. This is conscious decision we've made to keep the query language small.

To express complex queries like aggregations, window functions, custom joins etc, use SQL, which is designed for this purpose. **If you can express your query in SQL, define a view with it**, you can use the data APIs on these views.

Let's see how we can get the likes of an article.

Let us `define a view in SQL <https://www.postgresql.org/docs/current/static/sql-createview.html>`_:

.. code-block:: sql

   CREATE VIEW article_like_count AS
      SELECT article_id, COUNT(author_id) AS like_count
      FROM article_like
      GROUP BY article_id;

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: admin

   {
     "type" : "run_sql",
     "args" : {
       "sql" : "CREATE VIEW article_like_count AS...",
     }
   }

Let us then add this view, using the ``add_existing_table_or_view`` query type:

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if only specific user role has access

   {
     "type" : "add_existing_table_or_view",
     "args" : {
       "name" : "article_like_count"
     }
   }

As soon as the view is tracked by the Data API, you can use ``select`` as if ``article_like_count`` is a table.

.. admonition:: Views are read only!

   Views are like read-only logical tables on the database.
   So that means that Data API requests to select will work, but you cannot
   insert/update/delete items from the view.

We've seen how we can get article`s likes using the ``data`` APIs. However, this additional information of each article can be attached to the article table using an object relationship say, ``article_like_count``.

All the relationships that we've defined till now use foreign key constraints. However, you cannot define foreign key constraints on/to views. So, in these cases, we can manually define a relationship as follows:

.. code-block:: http
   :emphasize-lines: 11-15

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if only specific user role has access

   {
       "type": "create_object_relationship",
       "args": {
           "table": "article",
           "name": "article_like_count",
           "using": {
               "manual_configuration" : {
                   "remote_table" : "article_like_count",
                   "column_mapping" : {
                       "id" : "article_id"
                   }
               }
           }
       }
   }

Let's fetch articles ordered by the number of likes.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if only specific user role has access

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
