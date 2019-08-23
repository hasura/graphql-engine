Schema/Metadata API Reference: Remote schemas
=============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Add/Remove a remote GraphQL server as remote schema in Hasura GraphQL engine.

.. _add_remote_schema:

add_remote_schema
-----------------

``add_remote_schema`` is used to add a remote GraphQL server as remote schema. GraphQL engine stitches it's schema with existing.

An example request as follows:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "add_remote_schema",
       "args": {
           "name": "my remote schema",
           "definition": {
               "url": "https://remote-server.com/graphql",
               "headers": [{"name": "X-Server-Request-From", "value": "Hasura"}],
               "forward_client_headers": false
           },
           "comment": "some optional comment"
       }
   }


.. _add_remote_schema_syntax:

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`RemoteSchemaName`
     - Name of the remote schema
   * - definition
     - true
     - :ref:`RemoteSchemaDef`
     - Definition for the remote schema
   * - comment
     - false
     - Text
     - comment

.. _remove_remote_schema:

remove_remote_schema
--------------------

``remove_remote_schema`` is used to delete a remote schema. GraphQL engine de-stitches it's schema.

An example request as follows:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "remove_remote_schema",
       "args": {
           "name": "my remote schema"
       }
   }

.. _remove_remote_schema_syntax:

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`RemoteSchemaName`
     - Name of the remote schema

.. _reload_remote_schema:

reload_remote_schema
--------------------

``reload_remote_schema`` is used to refresh schema of the remote server. GraphQL engine refetches schema from server and stitches.

An example request as follows:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "reload_remote_schema",
       "args": {
           "name": "my remote schema"
       }
   }

.. _reload_remote_schema_syntax:

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`RemoteSchemaName`
     - Name of the remote schema
