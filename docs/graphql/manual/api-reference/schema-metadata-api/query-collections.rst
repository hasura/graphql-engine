Schema/Metadata API Reference: Query collections
================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Group queries using Query collections.

Create/Drop query collections and Add/Drop a query to a collection using following query types.

.. _create_query_collection:

create_query_collection
-----------------------

``create_query_collection`` is used to define a collection.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "create_query_collection",
       "args": {
            "name": "my_collection",
            "comment": "an optional comment",
            "definition": {
                "queries": [
                    {"name": "query_1", "query": "query { test {id name}}"}
                 ]
            }
        }
   }

.. _create_query_collection_syntax:

Args Syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`CollectionName`
     - Name of the query collection
   * - definition
     - true
     - :ref:`CollectionQuery` array
     - List of queries
   * - comment
     - false
     - text
     - Optional comment


.. _drop_query_collection:

drop_query_collection
---------------------

``drop_query_collection`` is used to drop a collection

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "drop_query_collection",
       "args": {
            "name": "my_collection",
        }
   }

.. _drop_query_collection_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`CollectionName`
     - Name of the query collection

.. _add_query_to_collection:

add_query_to_collection
-----------------------

``add_query_to_collection`` is used to add a query to given collection

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "add_query_to_collection",
       "args": {
            "collection_name": "my_collection",
            "query_name": "query_2",
            "query": "query {test {name}}"
        }
   }

.. _add_query_to_collection_syntax:

Args Syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - collection_name
     - true
     - :ref:`CollectionName`
     - Name of the query collection
   * - query_name
     - true
     - :ref:`QueryName`
     - Name of the query
   * - query
     - true
     - text
     - The GraphQL query text

.. _drop_query_from_collection:

drop_query_from_collection
--------------------------

``drop_query_from_collection`` is used to remove a query from given collection

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "drop_query_from_collection",
       "args": {
            "collection_name": "my_collection",
            "query_name": "query_2"
        }
   }

.. _drop_query_from_collection_syntax:

Args Syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - collection_name
     - true
     - :ref:`CollectionName`
     - Name of the query collection
   * - query_name
     - true
     - :ref:`QueryName`
     - Name of the query

.. _add_collections_to_allowlist:

add_collections_to_allowlist
----------------------------

``add_collections_to_allowlist`` is used to add collections to allow-list

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "add_collections_to_allowlist",
       "args": {
            "collections": ["my_collection_1", "my_collection_2"]
        }
   }

.. _add_collections_to_allowlist_syntax:

Args Syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - collections
     - true
     - :ref:`CollectionName` array
     - Names of query collections

.. _drop_collections_from_allowlist:

drop_collections_from_allowlist
-------------------------------

``drop_collections_from_allowlist`` is used to remove collections from allow-list

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "drop_collections_from_allowlist",
       "args": {
            "collections": ["my_collection_1", "my_collection_2"]
        }
   }

.. _drop_collections_from_allowlist_syntax:

Args Syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - collections
     - true
     - :ref:`CollectionName` array
     - Names of query collections
