.. title:: API Reference - Query

API Reference - Query
=====================

Query syntax
------------

.. parsed-literal::
   :class: haskell-pre

    object [(arguments)]{
        object-fields
    }

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - object
     - true
     - GraphQLObject_
     - Name of the table/object
   * - arguments
     - false
     - ArgExp_
     - Return these columns of the selected rows

**E.g.**:

.. parsed-literal::
   :class: haskell-pre

    query {
      author( where: {articles: {id: {_gt: 10}}}) {
        id
        name
      }
    }

.. note::
    
    For more examples and details of usage, please see :doc:`this <../queries/index>`.

Syntax definitions
------------------

.. _GraphQLObject:

Object
^^^^^^

.. parsed-literal::

  object-name {
    field1
    field2
    ..
    nested object1
    nested object2
    ..
  }

E.g.

.. parsed-literal::
   :class: haskell-pre

   author {
      id #scalar field
      name #scalar field
      article { #nested object
        title
      }
   }

.. _ArgExp:

Argument
^^^^^^^^

.. parsed-literal::
   :class: haskell-pre

   ( where BoolExp_ | OrderByExp_ | PaginationExp_ )

.. _BoolExp:

BoolExp
*******

.. parsed-literal::
   :class: haskell-pre

   AndExp_ | OrExp_ | NotExp_ | ColumnExp_

AndExp
######

.. parsed-literal::
   :class: haskell-pre

   {
        _and: [ColumnExp_]
   }


OrExp
#####

.. parsed-literal::
   :class: haskell-pre

   {
        _or: [ColumnExp_]
   }

NotExp
######

.. parsed-literal::
   :class: haskell-pre

   {
        _not: [ColumnExp_]
   }

ColumnExp
#########

.. parsed-literal::
   :class: haskell-pre

   {
       field-name : { Operator_ : Value }
   }

Operator
########
Generic operators (all column types except json, jsonb) :

- ``_eq``
- ``_ne``
- ``_in``
- ``_nin``
- ``_gt``
- ``_lt``
- ``_gte``
- ``_lte``

Operators for comparing columns (all column types except json, jsonb):

- ``_ceq``
- ``_cneq``
- ``_cgt``
- ``_clt``
- ``_cgte``
- ``_cnlte``

Text related operators :

- ``_like``
- ``_nlike``
- ``_ilike``
- ``_nilike``
- ``_similar``
- ``_nsimilar``

.. _OrderByExp:

OrderByExp
**********

.. parsed-literal::
   :class: haskell-pre

   order_by: Integer

.. _PaginationExp:

PaginationExp
*************

.. parsed-literal::
   :class: haskell-pre

   limit: Integer [offset: Integer]

