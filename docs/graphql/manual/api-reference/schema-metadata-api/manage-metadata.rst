Schema/Metadata API Reference: Manage metadata
==============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

APIs to manage Hasura metadata which is stored in ``hdb_catalog`` schema.

.. _export_metadata:

export_metadata
---------------

``export_metadata`` is used to export the current metadata from server as a JSON
file. Response JSON will be the metadata object.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "export_metadata",
       "args": {}
   }

.. _replace_metadata:

replace_metadata
----------------

``replace_metadata`` is used to replace/import metadata into Hasura. Existing
metadata will be replaced with the new one.

.. code-block:: none

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "replace_metadata",
       "args": <metadata-as-json-object>
   }

.. _replace_metadata_syntax:

Args syntax
^^^^^^^^^^^

Args should be the JSON object which is same as the output of
:ref:`export_metadata`.

.. _reload_metadata:

reload_metadata
---------------

``reload_metadata`` should be used when there is change in underlying Postgres
database that Hasura should be aware of. Example: a new column is added to a
table using ``psql`` and this column should now be added to the GraphQL schema.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "reload_metadata",
       "args": {}
   }

.. _clear_metadata:

clear_metadata
--------------

``clear_metadata`` can be used to reset the state of Hasura -- clean the current
state by forgetting the tables tracked, relationships, permissions, event
triggers etc.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "clear_metadata",
       "args": {}
   }

.. _get_inconsistent_metadata:

get_inconsistent_metadata
-------------------------

``get_inconsistent_metadata`` can be used to fetch all inconsistent metadata objects.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "get_inconsistent_metadata",
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

.. _drop_inconsistent_metadata:

drop_inconsistent_metadata
--------------------------

``drop_inconsistent_metadata`` can be used to purge all inconsistent objects from metadata.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "drop_inconsistent_metadata",
       "args": {}
   }
