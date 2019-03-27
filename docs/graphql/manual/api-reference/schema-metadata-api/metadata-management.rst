Schema/Metadata API Reference: Metadata Management
==================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

.. admonition:: Admin-only

  Following APIs are admin-only, i.e. the request can only be executed by having ``X-Hasura-Role: admin``. This can be set by passing
  ``X-Hasura-Admin-Secret`` or by setting the right role in Webhook/JWT
  authorization mode.

.. _reload_metadata:

reload_metadata
---------------

``reload_metadata`` can be used to rebuild metadata from database and update cache of Hasura GraphQL Engine server.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "reload_metadata",
       "args": {}
   }

.. _clear_metadata:

clear_metadata
--------------

``clear_metadata`` can be used to clean user defined metadata. This action is **irreversible**.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "clear_metadata",
       "args": {}
   }

.. _export_metadata:

export_metadata
---------------

``export_metadata`` can be used to export all user defined metadata objects.


.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "export_metadata",
       "args": {}
   }

Response:

.. code-block:: json

   {
       "functions": [],
       "remote_schemas": [],
       "tables": [
           {
               "table": "author",
               "object_relationships": [],
               "array_relationships": [
                   {
                       "using": {
                           "foreign_key_constraint_on": {
                               "column": "author_id",
                               "table": "article"
                           }
                       },
                       "name": "articles",
                       "comment": null
                   }
               ],
               "insert_permissions": [],
               "select_permissions": [],
               "update_permissions": [],
               "delete_permissions": [],
               "event_triggers": []
           },
           {
               "table": "article",
               "object_relationships": [
                   {
                       "using": {
                           "foreign_key_constraint_on": "author_id"
                       },
                       "name": "author",
                       "comment": null
                   }
               ],
               "array_relationships": [],
               "insert_permissions": [],
               "select_permissions": [],
               "update_permissions": [],
               "delete_permissions": [],
               "event_triggers": []
           }
       ],
       "query_templates": []
   }

.. _replace_metadata:

replace_metadata
----------------

``replace_metadata`` can be used to replace metadata objects given.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
     "type": "replace_metadata",
     "args": {
       "functions": [],
       "remote_schemas": [],
       "tables": [
         {
           "table": "author",
           "object_relationships": [],
           "array_relationships": [
             {
               "using": {
                 "foreign_key_constraint_on": {
                   "column": "author_id",
                   "table": "article"
                 }
               },
               "name": "articles",
               "comment": null
             }
           ],
           "insert_permissions": [],
           "select_permissions": [],
           "update_permissions": [],
           "delete_permissions": [],
           "event_triggers": []
         },
         {
           "table": "article",
           "object_relationships": [
             {
               "using": {
                 "foreign_key_constraint_on": "author_id"
               },
               "name": "author",
               "comment": null
             }
           ],
           "array_relationships": [],
           "insert_permissions": [],
           "select_permissions": [],
           "update_permissions": [],
           "delete_permissions": [],
           "event_triggers": []
         }
       ],
       "query_templates": []
     }
   }

.. _get_inconsistent_objects:

get_inconsistent_objects
------------------------

``get_inconsistent_objects`` can be used to fetch all inconsistent metadata objects.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "get_inconsistent_objects",
       "args": {}
   }

Response:-

.. code-block:: json

   [
       {
           "definition": {
               "using": {
                   "foreign_key_constraint_on": {
                       "column": "author_id",
                       "table": "article"
                   }
               },
               "name": "articles",
               "comment": null,
               "table": "author"
           },
           "reason": "table \"article\" does not exist",
           "type": "array_relation"
       },
       {
           "definition": {
               "using": {
                   "foreign_key_constraint_on": "author_id"
               },
               "name": "author",
               "comment": null,
               "table": "article"
           },
           "reason": "table \"article\" does not exist",
           "type": "object_relation"
       },
       {
           "definition": "article",
           "reason": "no such table/view exists in postgres : \"article\"",
           "type": "table"
       }
   ]

.. _drop_inconsistent_objects:

drop_inconsistent_objects
-------------------------

``drop_inconsistent_objects`` can be used to purge all inconsistent objects from metadata.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "drop_inconsistent_objects",
       "args": {}
   }
