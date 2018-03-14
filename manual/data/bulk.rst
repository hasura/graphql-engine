Bulk Data Queries
=================

To make multiple queries as a part of the same request you can sent them as parts of one bulk query.

To select from multiple tables in the same request, make a ``bulk`` query as follows

.. code-block:: http
   :emphasize-lines: 7,22

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type": "bulk",
       "args": [
           {
               "type": "select",
               "args": {
                   "table": "article",
                   "columns": ["*"]
               }
           },
           {
               "type": "select",
               "args": {
                   "table": "author",
                   "columns": ["*"]
               }
           }
       ]
   }

The response of the above query will be an array of two elements with first one containing the result from the ``article`` table and second one containing the result from the ``author`` table

A sample response will look like below

.. code-block:: http
   :emphasize-lines: 13

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   [
       [{
           "rating": 4,
           "author_id": 12,
           "content": "Vestibulum accumsan neque. Quisque ornare tortor.",
           "id": 1,
           "title": "sem ut dolor dapibus gravida."
       }],
       [{
           "name": "Chrissie",
           "id": 1
       }]
   ]

.. note::

   In the above example we are sending two ``select`` queries in the bulk query. They can be any of the query types supported by Hasura. You can find the complete list :ref:`here <data_query_types>`.
