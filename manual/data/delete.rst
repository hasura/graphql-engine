Deleting data
=============

The request to delete data takes a ``where`` clause indicating what to delete. The syntax of where clause is same as in
the ``select`` query.

The full syntax of a ``delete`` query can be found :ref:`here <data_delete>`.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

         mutation delete_article {
           delete_article (where: {rating: {_lte: 1}}) {
               affected_rows
           }
         }

   .. tab:: JSON API

      .. code-block:: http

         POST /v1/query HTTP/1.1
         Content-Type: application/json
         Authorization: Bearer <auth-token> # optional if cookie is set
         X-Hasura-Role: <role>  # optional. Required if only specific user role has access

         {
             "type" : "delete",
             "args" : {
                 "table" : "article",
                 "where": {
                    "rating": { "$lte" : 1 }
                 }
             }
         }


