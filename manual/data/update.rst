Updating data
=============

The request to update data consists of two parts - the new values and a ``where`` indicating what rows to update. The syntax of where clause is same as in the ``select`` query.

The full definition of an `update` query can be found :ref:`here <data_update>`.


.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json

   {
       "type" : "update",
       "args" : {
           "table" : "article",
           "$set": {"title": "Article 4", "description": "Sample article 4"},
           "where": {
               "id": 4
           }
       }
   }
