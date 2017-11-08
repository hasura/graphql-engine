Inserting data
----------------

Consider Let's insert a couple of categories. The full definition of `insert` request can be found :ref:`here <data_insert>`.

.. code-block:: http
   :emphasize-lines: 13

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type":"insert",
       "args":{
           "table":"category",
           "objects":[
               {"name":"News"},
               {"name":"Movies"}
           ],
           "returning":["id"]
       }
   }

Note the ``returning`` key. We would like to get the auto incremented id for each inserted row. The ``returning`` key is optional.

