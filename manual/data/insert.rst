Inserting data
==============

The following will insert a couple of new articles to the ``article`` table.

The full syntax of an ``insert`` query can be found :ref:`here <data_insert>`.

.. code-block:: http
   :emphasize-lines: 13

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if request needs particular user role

   {
       "type":"insert",
       "args":{
           "table":"article",
           "objects":[
               {"title":"Article 1", "description": "Sample article 1"},
               {"title":"Article 2", "description": "Sample article 2"}
           ],
           "returning":["id", "title"]
       }
   }

Note the ``returning`` key. We would like to get back the auto incremented id for each inserted row along with the title in the response. The ``returning`` key is optional.

