Bulk Query
----------------

To select from multiple tables in the same request make a ``bulk`` query as follows

.. code-block:: http
   :emphasize-lines: 13

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type": "bulk",
       "args": [{
           "type": "select",
           "args": {
               "table": "article",
               "columns": [
                   "*"
               ]
           }
       },
       {
           "type": "select",
           "args": {
               "table": "author",
               "columns": [
                   "*"
               ]
           }
       }]
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
           "content": "Vestibulum accumsan neque et nunc. Quisque ornare tortor at risus. Nunc ac sem ut dolor dapibus gravida. Aliquam tincidunt, nunc ac mattis ornare, lectus ante dictum mi, ac mattis velit justo nec ante. Maecenas mi felis, adipiscing fringilla, porttitor vulputate, posuere vulputate, lacus. Cras interdum. Nunc sollicitudin commodo ipsum. Suspendisse non leo. Vivamus nibh dolor, nonummy ac, feugiat non, lobortis quis, pede. Suspendisse dui. Fusce diam nunc, ullamcorper eu, euismod ac, fermentum vel, mauris. Integer sem elit, pharetra ut, pharetra sed, hendrerit a, arcu. Sed et libero. Proin mi. Aliquam gravida mauris ut mi. Duis risus odio, auctor vitae, aliquet nec, imperdiet nec, leo. Morbi neque tellus, imperdiet non, vestibulum nec, euismod in, dolor. Fusce feugiat. Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aliquam auctor, velit eget laoreet posuere, enim nisl",
           "id": 1,
           "title": "sem ut dolor dapibus gravida."
       }],
       [{
           "name": "Chrissie",
           "id": 1
       }]
   ]

Note we are sending two ``select`` queries in the above example. it can be any of the query type supported by Hasura you can find the complete list :ref:`here <data_query_main>`.
