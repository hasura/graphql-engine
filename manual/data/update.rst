Updating data
=============

The request to update data consists of two parts - the new values and a ``where`` indicating what rows to update. The syntax of where clause is same as in the ``select`` query.

The full definition of an `update` query can be found :ref:`here <data_update>`.


.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

         mutation update_article {
           update_article (where: {id: {_eq: 4}}, _set: {title: "Article 4", content: "Sample article 4"}) {
               affected_rows
           }
         }

   .. tab:: JSON API

      .. code-block:: http

         POST /v1/query HTTP/1.1
         Content-Type: application/json
         Authorization: Bearer <admin-token> # optional if cookie is set
         X-HASURA-ROLE: <role>  # optional. Required if request needs particular user role

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
