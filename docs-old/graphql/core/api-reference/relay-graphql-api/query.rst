.. meta::
   :description: Hasura Relay GraphQL API queries and subscriptions API reference
   :keywords: hasura, docs, GraphQL API, API reference, query, subscription, relay

.. _relay_graphql_api_query:

Relay API Reference - Query / Subscription
==========================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 3
  :local:

**query** / **subscription** syntax
-----------------------------------

.. code-block:: none

    query|subscription [<op-name>] {
      connection-object [([argument])]{
        connection-object-fields
      }
    }

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - op-name
     - false
     - Value
     - Name query/subscription for observability
   * - connection-object
     - true
     - ConnectionObject_
     - Name of the table connection
   * - argument
     - false
     - Argument_
     - One or more filter criteria, instructions for sorting order or pagination

**Example: Relay Query**

.. code-block:: graphql

     query {
       author_connection(first: 2){
         pageInfo {
           hasNextPage
           endCursor
         }
         edges {
           cursor
           node {
             id
             name
             username
           }
         }
       }
     }

**Example: Relay Subscription**

.. code-block:: graphql

     subscription {
       author_connection(first: 2){
         pageInfo {
           hasNextPage
           endCursor
         }
         edges {
           cursor
           node {
             id
             name
             username
           }
         }
       }
     }q

.. note::

    For details of usage, please see :ref:`this page <pg_relay_schema>`.

Syntax definitions
------------------

.. _ConnectionObject:

Connection Object
^^^^^^^^^^^^^^^^^

.. code-block:: none

   connection-object {
     pageInfo: {
       hasNextPage
       hasPreviousPage
       startCursor
       endCursor
     }
     edges: {
       cursor
       node: {
         id
         field1
         field2
         json_field[(path: String)]
         ..
         nested object1
         nested object2
         aggregate nested object1
         ..
       }
     }
   }


.. list-table::
   :header-rows: 1

   * - Field
     - Type
   * - pageInfo
     - PageInfo_!
   * - edges
     - [Edge_!]!

.. note::

  For more details on the Relay ``connection`` object type, refer to the `Relay docs <https://relay.dev/graphql/connections.htm#sec-Connection-Types>`__.

.. _PageInfo:

PageInfo
^^^^^^^^

Information useful for pagination. 

.. code-block:: graphql

   type PageInfo {
     hasNextPage: Boolean!
     hasPreviousPage: Boolean!
     startCursor: String!
     endCursor: String!
   }

.. note::

  For more details on the Relay ``PageInfo`` object type, refer to the `Relay docs <https://relay.dev/graphql/connections.htm#sec-undefined.PageInfo>`__.

.. _Edge:

Edge
^^^^

Edge is an object type that consists of a :ref:`cursor <Cursor>` and ``node`` fields.
``node`` is a table object type which implements the Relay ``Node`` interface.

.. code-block:: graphql

   type tableEdge {
     cursor: String!
     node: table!
   }

.. _Cursor:

Cursor
^^^^^^

The cursor field returns a unique non-null String value which is useful for :ref:`pagination <RelayPaginationExp>`.

.. note::

  For more details on the Relay ``cursor``, refer to the `Relay docs <https://relay.dev/graphql/connections.htm#sec-Cursor>`__.

Argument
^^^^^^^^

.. parsed-literal::

   :ref:`DistinctOnExp <RelayDistinctOnExp>` | :ref:`WhereExp <RelayWhereExp>` | :ref:`OrderByExp <RelayOrderByExp>` | :ref:`PaginationExp <RelayPaginationExp>`

.. _RelayDistinctOnExp:

DistinctOnExp
*************

Same as the generic :ref:`DistinctOnExp`

.. _RelayWhereExp:

WhereExp
********

Same as the generic :ref:`WhereExp`

.. _RelayOrderByExp:

OrderByExp
**********

Same as the generic :ref:`OrderByExp`

.. _RelayPaginationExp:

PaginationExp
*************

**Forward Pagination:**

.. parsed-literal::

   first: Integer
   [after: Cursor_]

.. code-block:: graphql

    query {
      article_connection(
        first: 2
        after: "eyJpZCIgOiAzfQ=="
      ){
        pageInfo {
          startCursor
          endCursor
          hasPreviousPage
          hasNextPage
        }
        edges {
          cursor
          node {
            title
            content
            author_id
          }
        }
      }
    }


**Backward Pagination:**

.. parsed-literal::

   [last: Integer]
   [before: Cursor_]

.. code-block:: graphql

    query {
      article_connection(
        last: 2
        before: "eyJpZCIgOiA0fQ=="
      ){
        pageInfo {
          startCursor
          endCursor
          hasPreviousPage
          hasNextPage
        }
        edges {
          cursor
          node{
            title
            content
            author_id
          }
        }
      }
    }
