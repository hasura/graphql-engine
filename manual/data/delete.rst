Deleting data
=============

The request to delete data takes a ``where`` clause indicating what to delete. The syntax of where clause is same as in the ``select`` query.

The full syntax of a ``delete`` query can be found :ref:`here <data_delete>`.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json

   {
       "type" : "delete",
       "args" : {
           "table" : "article",
           "where": {
              "rating": { "$lte" : 1 }
           }
       }
   }


