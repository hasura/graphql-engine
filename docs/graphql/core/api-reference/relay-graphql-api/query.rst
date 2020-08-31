.. meta::
   :description: Hasura Relay GraphQL API queries and subscriptions API reference
   :keywords: hasura, docs, GraphQL API, API reference, query, subscription, relay

.. _relay_graphql_api_query:

Relay API Reference - Query / Subscription
====================================

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
     - One or more of filter criteria, instructions for sort order or pagination

**Example: Relay Query**

.. code-block:: graphql

     query {
       author_connection(first: 2){
         pageInfo{
           hasNextPage
           endCursor
         }
         edges{
           cursor
           node{
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
         pageInfo{
           hasNextPage
           endCursor
         }
         edges{
           cursor
           node{
             id
             name
             username
           }
         }
       }
     }

.. note::

    For more examples and details of usage, please see :ref:`this <relay_queries>`.

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

For more details on relay connection object type, refer to the `Relay docs <https://relay.dev/graphql/connections.htm#sec-Connection-Types>`__.

.. _PageInfo:

PageInfo
^^^^^^^^

Information useful for pagination. For more details on relay PageInfo object type,
refer to the `Relay docs <https://relay.dev/graphql/connections.htm#sec-undefined.PageInfo>`__.

.. code-block:: graphql

   type PageInfo {
     hasNextPage: Boolean!
     hasPreviousPage: Boolean!
     startCursor: String!
     endCursor: String!
   }


.. _Edge:

Edge
^^^^

Edge is an object type consists of :ref:`cursor <Cursor>` and ``node`` fields.
The ``node`` field is a table object type which implements the relay ``Node`` interface.

.. code-block:: graphql

   type tableEdge {
     cursor: String!
     node: table!
   }

.. _Cursor:

Cursor
^^^^^^

The cursor field returns a unique non-null String value which is useful in :ref:`pagination <PaginationExp>`.
For more details on relay cursor, refer to the `Relay docs <https://relay.dev/graphql/connections.htm#sec-Cursor>`__.

Argument
^^^^^^^^

.. parsed-literal::

   :ref:`DistinctOnExp <distinct_on_exp>` | :ref:`WhereExp <where_exp>` | :ref:`OrderByExp <order_by_exp>` | PaginationExp_

.. _PaginationExp:

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
        pageInfo{
          startCursor
          endCursor
          hasPreviousPage
          hasNextPage
        }
        edges{
          cursor
          node{
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
        pageInfo{
          startCursor
          endCursor
          hasPreviousPage
          hasNextPage
        }
        edges{
          cursor
          node{
            title
            content
            author_id
          }
        }
      }
    }
