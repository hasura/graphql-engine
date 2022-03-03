.. meta::
   :description: Manage query collections with the Hasura metadata API
   :keywords: hasura, docs, metadata API, API reference, query collection

.. _metadata_api_query_collections:

Metadata API Reference: Query collections
=========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Group queries using query collections.

Create/drop query collections and add/drop a query to a collection using the following query types.

.. admonition:: Supported from

  The metadata API is supported for versions ``v2.0.0`` and above and replaces the older
  :ref:`schema/metadata API <schema_metadata_apis>`.

.. _metadata_create_query_collection:

create_query_collection
-----------------------

``create_query_collection`` is used to define a collection.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "create_query_collection",
       "args": {
           "name": "my_collection",
           "comment": "an optional comment",
           "definition": {
               "queries": [
                   {
                       "name": "query_1",
                       "query": "query { test { id name } }"
                   }
               ]
           }
       }
   }

.. _metadata_create_query_collection_syntax:

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


.. _metadata_drop_query_collection:

drop_query_collection
---------------------

``drop_query_collection`` is used to drop a collection

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "drop_query_collection",
       "args": {
           "collection": "my_collection",
           "cascade": false
       }
   }

.. _metadata_drop_query_collection_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - collection
     - true
     - :ref:`CollectionName`
     - Name of the query collection
   * - cascade
     - true
     - boolean
     - When set to ``true``, the collection (if present) is removed from the allowlist

.. _metadata_add_query_to_collection:

add_query_to_collection
-----------------------

``add_query_to_collection`` is used to add a query to a given collection.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
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

.. _metadata_add_query_to_collection_syntax:

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

.. _metadata_drop_query_from_collection:

drop_query_from_collection
--------------------------

``drop_query_from_collection`` is used to remove a query from a given collection.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "drop_query_from_collection",
       "args": {
           "collection_name": "my_collection",
           "query_name": "query_2"
       }
   }

.. _metadata_drop_query_from_collection_syntax:

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

.. _metadata_add_collection_to_allowlist:

add_collection_to_allowlist
---------------------------

``add_collection_to_allowlist`` is used to add a collection to the
allow-list. It is possible to specify a scope, defaulting to global.

If the given collection already exists in the allowlist regardless
of scope, ``add_collection_to_allowlist`` is a no-op. To change the
scope, use :ref:`metadata_update_scope_of_collection_in_allowlist`.

If the scope is global, all roles will be able to access the queries
present in the query collection:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "add_collection_to_allowlist",
       "args": {
           "collection": "my_collection",
           "scope": {
               "global": true
           }
       }
   }

If the scope is not global, only the listed roles are allowed to
to access the queries:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "add_collection_to_allowlist",
       "args": {
           "collection": "role_based_query_collection",
           "scope": {
               "global": false,
               "roles": [
                  "user",
                  "editor"
               ]
           }
       }
   }

If a query occurs in multiple collections, a role will be allowed
to access the query if it is listed for any of the collections.

.. _metadata_add_collection_to_allowlist_syntax:

Args Syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - collection
     - true
     - :ref:`CollectionName`
     - Name of a query collection to be added to the allow-list
   * - scope
     - false
     - :ref:`AllowlistScope`
     - Scope of the collection in the allowlist. (default: ``{global: true}``)
       When the scope is global, the query collection's queries will be accessible
       to all roles.
       When the scope is non-global, the query collection's queries will be accessible
       to all of the roles listed in the scope.
       *(non-global scope supported only in cloud/enterprise versions)*

.. _metadata_update_scope_of_collection_in_allowlist:

update_scope_of_collection_in_allowlist
---------------------------------------

``update_scope_of_collection_in_allowlist`` is used to add change the
scope of a collection in the allowlist. Its effect is the same as
first dropping the collection from the allowlist using
:ref:`metadata_drop_collection_from_allowlist`, and then adding it with the
given scope using :ref:`metadata_add_collection_to_allowlist`.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "update_scope_of_collection_in_allowlist",
       "args": {
           "collection": "previously_global_query_collection",
           "scope": {
               "global": false,
               "roles": [
                  "user",
                  "editor"
               ]
           }
       }
   }

.. _metadata_update_scope_of_collection_in_allowlist_syntax:

Args Syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - collection
     - true
     - :ref:`CollectionName`
     - Name of a query collection to be added to the allow-list
   * - scope
     - true
     - :ref:`AllowlistScope`
     - Scope of the collection in the allowlist.
       When the scope is global, the query collection's queries will be accessible
       to all roles.
       When the scope is non-global, the query collection's queries will be accessible
       to all of the roles listed in the scope.
       *(non-global scope supported only in cloud/enterprise versions)*

.. _metadata_drop_collection_from_allowlist:

drop_collection_from_allowlist
------------------------------

``drop_collection_from_allowlist`` is used to remove a collection from the allow-list.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "drop_collection_from_allowlist",
       "args": {
           "collection": "my_collection_1"
       }
   }

.. _metadata_drop_collection_from_allowlist_syntax:

Args Syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - collection
     - true
     - :ref:`CollectionName`
     - Name of a query collection to be removed from the allow-list
