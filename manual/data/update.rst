Update data 
------------------

The request to update data consists of two parts - the new values and a ``where`` indicating what rows to update. The syntax of where clause is same as in the `select` query. For the full syntax of update request, see :ref:`here <data_update>`.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json

   {
       "type" : "update",
       "args" : {
           "table" : "article",
           "$set": {"title": "Mysterious affair at Styles"},
           "where": {
               "id": 4
           }
       }
   }
