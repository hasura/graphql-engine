.. title:: API Reference - Query/Subscription

API Reference - Query/Subscription
==================================

Query/Subscription syntax
-------------------------

.. parsed-literal::
   :class: haskell-pre

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
     - one or more of filter criteria, instructions for sort order or pagination

**E.g. QUERY**:

.. parsed-literal::
   :class: haskell-pre

    query {
      author(where: {articles: {rating: {_gte: 4}}} order_by: name_asc) {
        id
        name
      }
    }

**E.g. SUBSCRIPTION**:

.. parsed-literal::
   :class: haskell-pre

    subscription {
      author(where: {articles: rating: {_gte: 4}}} order_by: name_asc) {
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
   :class: haskell-pre

   WhereExp_ | OrderByExp_ | PaginationExp_

.. _WhereExp:

WhereExp
********

.. parsed-literal::
  :class: haskell-pre

   where: BoolExp_

.. _BoolExp_:

BoolExp
"""""""

.. parsed-literal::
   :class: haskell-pre

   AndExp_ | OrExp_ | NotExp_ | ColumnExp_

AndExp
######

.. parsed-literal::
   :class: haskell-pre

    {
      _and: [BoolExp_]
    }


OrExp
#####

.. parsed-literal::
   :class: haskell-pre

    {
      _or: [BoolExp_]
    }

NotExp
######

.. parsed-literal::
   :class: haskell-pre

    {
      _not: BoolExp_
    }

ColumnExp
#########

.. parsed-literal::
   :class: haskell-pre

    {
      field-name : {Operator_: Value }
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

Checking for ``null`` values :

- ``_is_null`` (takes true/false as values)

.. _OrderByExp:

OrderByExp
**********

.. parsed-literal::
   :class: haskell-pre

   order_by: (object-field + OrderByOperator_ | [object-field + OrderByOperator_])

E.g.

.. parsed-literal::
  :class: haskell-pre

   order_by: name_asc

or

.. parsed-literal::
  :class: haskell-pre

   order_by: [name_asc, id_desc]


.. _OrderByOperator:

OrderByOperator
"""""""""""""""

- ``_asc``
- ``_desc``
- ``_asc_nulls_first``
- ``_desc_nulls_first``

.. _PaginationExp:

PaginationExp
*************

.. parsed-literal::
   :class: haskell-pre

   limit: Integer [offset: Integer]

