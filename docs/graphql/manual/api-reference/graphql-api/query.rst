API Reference - Query / Subscription
====================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 3
  :local:

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

    For more examples and details of usage, please see :doc:`this <../../queries/index>`.

Syntax definitions
------------------

Object
^^^^^^

.. parsed-literal::

   SimpleObject_ | AggregateObject_

.. _SimpleObject:

Simple Object
*************

.. code-block:: none

  object-name {
    field1
    field2
    json_field[(path: String)]
    ..
    nested object1
    nested object2
    aggregate nested object1
    ..
  }

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - path
     - false
     - Value
     - ``path`` argument of ``json``/``jsonb`` follows simple `JSONPath specification <https://github.com/json-path/JsonPath>`_. However, prefix symbol ``$.`` is optional.

E.g.

.. code-block:: graphql

   author {
      id  # scalar integer field

      name  # scalar text field

      address(path: "$.city") # scalar JSON field -> property
      address(path: "city") # scalar JSON field -> property; '$.' prefix is optional
      contacts(path: "[0]") # scalar JSON field -> array_item
      contacts(path: "[0].phone") # scalar JSON field -> array_item_property

      article {  # nested object
        title
      }

      article_aggregate {  # aggregate nested object
        aggregate {
          count
        }
        nodes {
          title
        }
      }
   }

.. _AggregateObject:

Aggregate Object
****************

.. code-block:: none

  object-name_aggregate {
    aggregate {
      count
      sum {
        field
        ..
      }
      avg {
        field
        ..
      }
      stddev {
        field
        ..
      }
      stddev_samp {
        field
        ..
      }
      stddev_pop {
        field
        ..
      }
      variance {
        field
        ..
      }
      var_samp {
        field
        ..
      }
      var_pop {
        field
        ..
      }
      max {
        field
        ..
      }
      min {
        field
        ..
      }
    nodes {
      field1
      field2
      ..
      nested object1
      nested object2
      aggregate nested object1
      ..
    }
  }

(For more details on aggregate functions, refer to `Postgres docs <https://www.postgresql.org/docs/current/functions-aggregate.html#FUNCTIONS-AGGREGATE-STATISTICS-TABLE>`__.)

E.g.

.. code-block:: graphql

   author_aggregate {
     aggregate {
       count  # total count
       sum {
         id  # sum aggregate on id
       }
       avg {
         id  # avg aggregate on id
       }
       stddev {
         id  # stddev aggregate on id
       }
       stddev_samp {
         id  # stddev_samp aggregate on id
       }
       stddev_pop {
         id  # stddev_pop aggregate on id
       }
       variance {
         id  # variance aggregate on id
       }
       var_samp {
         id  # var_samp aggregate on id
       }
       var_pop {
         id  # var_pop aggregate on id
       }
       max {
         id  # max aggregate on id
       }
       min {
         id  # min aggregate on id
       }
     }

     nodes {  # objects
       id  # scalar field
       name  # scalar field

       article {  # nested object
         title
       }

       article_aggregate {  # aggregate nested object
         aggregate {
           count
         }
         nodes {
           title
         }
       }
     }
   }

Argument
^^^^^^^^

.. parsed-literal::

   DistinctOnExp_ | WhereExp_ | OrderByExp_ | PaginationExp_


.. _DistinctOnExp:

DistinctOnExp
*************

.. parsed-literal::

   distinct_on: [ TableSelectColumnEnum_ ]

TableSelectColumnEnum
"""""""""""""""""""""

.. code-block:: graphql

   #example table_select_column enum for "article" table
   enum article_select_column {
     id
     title
     content
     author_id
     is_published
   }


.. _WhereExp:

WhereExp
********

.. parsed-literal::

   where: BoolExp_

BoolExp
"""""""

.. parsed-literal::

   AndExp_ | OrExp_ | NotExp_ | TrueExp_ | ColumnExp_

AndExp
######

.. parsed-literal::

    {
      _and: [BoolExp_]
    }

OrExp
#####

.. parsed-literal::

    {
      _or: [BoolExp_]
    }

NotExp
######

.. parsed-literal::

    {
      _not: BoolExp_
    }

TrueExp
#######

.. parsed-literal::

    {}

ColumnExp
#########

.. parsed-literal::

    {
      field-name : {Operator_: Value }
    }

.. _Operator:

Operator
########
**Generic operators (all column types except json, jsonb):**

- ``_eq``
- ``_neq``
- ``_in``
- ``_nin``
- ``_gt``
- ``_lt``
- ``_gte``
- ``_lte``

**Text related operators:**

- ``_like``
- ``_nlike``
- ``_ilike``
- ``_nilike``
- ``_similar``
- ``_nsimilar``

**Checking for NULL values:**

- ``_is_null`` (takes true/false as values)

**Type casting:**

- ``_cast`` (takes a CastExp_ as a value)

**JSONB operators:**

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

(For more details on what these operators do, refer to `Postgres docs <https://www.postgresql.org/docs/current/static/functions-json.html#FUNCTIONS-JSONB-OP-TABLE>`__.)

**PostGIS related operators on GEOMETRY columns:**

.. list-table::
   :header-rows: 1

   * - Operator
     - PostGIS equivalent
   * - ``_st_contains``
     - ``ST_Contains``
   * - ``_st_crosses``
     - ``ST_Crosses``
   * - ``_st_equals``
     - ``ST_Equals``
   * - ``_st_intersects``
     - ``ST_Intersects``
   * - ``_st_overlaps``
     - ``ST_Overlaps``
   * - ``_st_touches``
     - ``ST_Touches``
   * - ``_st_within``
     - ``ST_Within``
   * - ``_st_d_within``
     - ``ST_DWithin``

(For more details on what these operators do, refer to `PostGIS docs <http://postgis.net/workshops/postgis-intro/spatial_relationships.html>`__.)

.. note::

   - All operators take a JSON representation of ``geometry/geography`` values as input value.
   - Input value for ``_st_d_within`` operator is an object:

     .. parsed-literal::

       {
         field-name : {_st_d_within: {distance: Float, from: Value} }
       }

.. _CastExp:

CastExp
#######

.. parsed-literal ::

    {type-name: {Operator_: Value}}

.. note::

   Currently, only casting between ``geometry`` and ``geography`` types is allowed.

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

or

.. parsed-literal::

   order_by: {articles_aggregate: {count: asc}}


TableOrderBy
""""""""""""

For columns:

.. parsed-literal::

   {column: OrderByEnum_}

For object relations:

.. parsed-literal::
   {relation-name: TableOrderBy_}

For array relations aggregate:

.. parsed-literal::
   {relation-name_aggregate: AggregateOrderBy_}

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
     #order by using "likes" array relationship aggregates
     likes_aggregate: likes_aggregate_order_by
   }

OrderByEnum
###########

.. code-block:: graphql

   #the order_by enum type
   enum order_by {
     #in the ascending order, nulls last
     asc
     #in the ascending order, nulls last
     asc_nulls_last
     #in the ascending order, nulls first
     asc_nulls_first
     #in the descending order, nulls first
     desc
     #in the descending order, nulls first
     desc_nulls_first
     #in the descending order, nulls last
     desc_nulls_last
   }

AggregateOrderBy
################

Count aggregate

.. parsed-literal::
   {count: OrderByEnum_}

Operation aggregate

.. parsed-literal::
   {op_name: TableAggOpOrderBy_}

Available operations are ``sum``, ``avg``, ``max``, ``min``, ``stddev``, ``stddev_samp``,
``stddev_pop``, ``variance``, ``var_samp`` and ``var_pop``

TableAggOpOrderBy
&&&&&&&&&&&&&&&&&

.. parsed-literal::
   {column: OrderByEnum_}

.. _PaginationExp:

PaginationExp
*************

.. parsed-literal::

   limit: Integer
   [offset: Integer]
