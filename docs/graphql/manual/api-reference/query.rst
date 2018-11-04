.. title:: API Reference - Query/Subscription

API Reference - Query/Subscription
==================================

Query/Subscription syntax
-------------------------

.. code-block:: none

    query|subscription [<op-name>] {
      object [([argument])]{
        object-fields
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
   * - object
     - true
     - Object_
     - Name of the table/object
   * - argument
     - false
     - Argument_
     - One or more of filter criteria, instructions for sort order or pagination

**E.g. QUERY**:

.. code-block:: graphql

    query {
      author(where: {articles: {rating: {_gte: 4}}} order_by: {name: asc}) {
        id
        name
      }
    }

**E.g. SUBSCRIPTION**:

.. code-block:: graphql

    subscription {
      author(where: {articles: rating: {_gte: 4}}} order_by: {name: asc}) {
        id
        name
      }
    }

.. note::

    For more examples and details of usage, please see :doc:`this <../queries/index>`.

Syntax definitions
------------------

.. _Object:

Object
^^^^^^

.. code-block:: none

  object-name {
    field1
    field2
    ..
    nested object1
    nested object2
    ..
  }

E.g.

.. code-block:: graphql

   author {
      id # scalar field
      name # scalar field
      article { # nested object
        title
      }
   }

.. _Argument:

Argument
^^^^^^^^

.. parsed-literal::

   WhereExp_ | OrderByExp_ | PaginationExp_

.. _WhereExp:

WhereExp
********

.. parsed-literal::

   where: BoolExp_

.. _BoolExp:

BoolExp
"""""""

.. parsed-literal::

   AndExp_ | OrExp_ | NotExp_ | ColumnExp_

.. _AndExp:

AndExp
######

.. parsed-literal::

    {
      _and: [BoolExp_]
    }

.. _OrExp:

OrExp
#####

.. parsed-literal::

    {
      _or: [BoolExp_]
    }

.. _NotExp:

NotExp
######

.. parsed-literal::

    {
      _not: BoolExp_
    }

ColumnExp
#########

.. parsed-literal::

    {
      field-name : {Operator_: Value }
    }

.. _Operator:

Operator
########
Generic operators (all column types except json, jsonb) :

- ``_eq``
- ``_neq``
- ``_in``
- ``_nin``
- ``_gt``
- ``_lt``
- ``_gte``
- ``_lte``

JSONB operators:

.. list-table::
   :header-rows: 1

   * - Operator
     - PostgreSQL equivalent
   * - ``_contains``
     - ``@>``
   * - ``_contained_in``
     - ``<@``
   * - ``_has_key``
     - ``?``
   * - ``_has_keys_any``
     - ``?|``
   * - ``_has_keys_all``
     - ``?&``

(For more details on what these operators do, refer to `Postgres docs <https://www.postgresql.org/docs/current/static/functions-json.html#FUNCTIONS-JSONB-OP-TABLE>`_.)

Text related operators :

- ``_like``
- ``_nlike``
- ``_ilike``
- ``_nilike``
- ``_similar``
- ``_nsimilar``

Checking for ``null`` values :

- ``_is_null`` (takes true/false as values)


.. _OrderByExp:

OrderByExp
**********

.. parsed-literal::

   order_by: (TableOrderBy_ | [ TableOrderBy_ ])

E.g.

.. parsed-literal::

   order_by: {id: desc}

or

.. parsed-literal::

   order_by: [{id: desc}, {author: {id: asc}}]


.. _TableOrderBy:

TableOrderBy
***********

For columns:

.. parsed-literal::

   {column: OrderByEnum_}

For object relations:

.. parsed-literal::
   {relation-name: TableOrderBy_}

E.g.

Order by type for "article" table:

.. code-block:: graphql

   input article_order_by {
     id: order_by
     title: order_by
     content: order_by
     author_id: order_by
     #order by using "author" object relationship columns
     author: author_order_by
   }

.. _OrderByEnum:

OrderByEnum
***********

.. code-block:: graphql

   #the order_by enum type
   enum order_by {
     #in the ascending order
     asc
     #in the descending order
     desc
     #in the ascending order, nulls first
     asc_nulls_first
     #in the descending order, nulls first
     desc_nulls_first
   }


.. _PaginationExp:

PaginationExp
*************

.. parsed-literal::

   limit: Integer
   [offset: Integer]
