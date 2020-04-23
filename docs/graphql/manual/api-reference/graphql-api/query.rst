.. meta::
   :description: Hasura GraphQL API queries and subscriptions API reference
   :keywords: hasura, docs, GraphQL API, API reference, query, subscription

.. _graphql_api_query:

API Reference - Query / Subscription
====================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 3
  :local:

Query / subscription syntax
---------------------------

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

    For more examples and details of usage, please see :ref:`this <queries>`.

Syntax definitions
------------------

Object
^^^^^^

.. parsed-literal::

   SimpleObject_ | AggregateObject_

.. _SimpleObject:

Simple object
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
      address(path: "$.city.altitude") # scalar JSON field -> property -> property
      address(path: "city") # scalar JSON field -> property; '$.' prefix is optional
      contacts(path: "[0]") # scalar JSON field -> array_item
      contacts(path: "[0].phone") # scalar JSON field -> array_item_property
      contacts(path: "['Hello world!']") # scalar JSON field -> property; used for special characters key
      contacts(path: "[\"Hello world!\"]") # same as above; the syntax is ugly, but still works

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

Aggregate object
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

(For more details on aggregate functions, refer to the `Postgres docs <https://www.postgresql.org/docs/current/functions-aggregate.html#FUNCTIONS-AGGREGATE-STATISTICS-TABLE>`__).

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

.. _AndExp:

AndExp
######

.. parsed-literal::

    {
      _and: [BoolExp_]
    }

.. admonition:: Syntactic sugar

  You can simplify an ``_and`` expression by passing the sub-expressions separated by a ``,``.

  **For example:**

  .. code-block:: graphql

    {
      _and: [
        { rating: { _gte: 4 } },
        { published_on: { _gte: "2018-01-01" } }
      ]
    }

    # can be simplified to:

    {
      rating: { _gte: 4 },
      published_on: { _gte: "2018-01-01" }
    }

.. _OrExp:

OrExp
#####

.. parsed-literal::

    {
      _or: [BoolExp_]
    }

.. note::

  The ``_or`` operator expects an array of expressions as input. Passing an object to it will result in the
  behaviour of the ``_and`` operator due to the way `GraphQL list input coercion <https://graphql.github.io/graphql-spec/June2018/#sec-Type-System.List>`_
  behaves.

  **For example:**

  .. code-block:: graphql

    {
      _or: {
       rating: { _gte: 4 },
       published_on: { _gte: "2018-01-01" }
      }
    }

    # will be coerced to:

    {
      _or: [
        {
          rating: { _gte: 4 },
          published_on: { _gte: "2018-01-01" }
        }
      ]
    }

    # which is equivalent to:

    {
      _or: [
        _and: [
          { rating: { _gte: 4 } },
          { published_on: { _gte: "2018-01-01" } }
        ]
      ]
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

.. _generic_operators:

**Generic operators (all column types except json, jsonb):**

.. list-table::
   :header-rows: 1

   * - Operator
     - PostgreSQL equivalent
   * - ``_eq``
     - ``=``
   * - ``_neq``
     - ``<>``
   * - ``_in``
     - ``IN``
   * - ``_nin``
     - ``NOT IN`` 
   * - ``_gt``
     - ``>``
   * - ``_lt``
     - ``<`` 
   * - ``_gte``
     - ``>=``
   * - ``_lte``
     - ``<=``   

(For more details on these operators, refer to the `Postgres docs <https://www.postgresql.org/docs/9.0/functions-comparison.html>`__.)

.. _text_operators:

**Text related operators:**

.. list-table::
   :header-rows: 1

   * - Operator
     - PostgreSQL equivalent
   * - ``_like``
     - ``LIKE``
   * - ``_nlike``
     - ``NOT LIKE``
   * - ``_ilike``
     - ``ILIKE``
   * - ``_nilike``
     - ``NOT ILIKE``
   * - ``_similar``
     - ``SIMILAR TO``
   * - ``_nsimilar``
     - ``NOT SIMILAR TO``

(For more details on text related operators, refer to the `Postgres docs <https://www.postgresql.org/docs/current/functions-matching.html>`__.)

.. _null_expression:

**Checking for NULL values:**

.. list-table::
   :header-rows: 1

   * - Operator
     - PostgreSQL equivalent
   * - ``_is_null`` (takes true/false as values)
     - ``IS NULL``

(For more details on the ``IS NULL`` expression, refer to the `Postgres docs <https://www.postgresql.org/docs/8.3/functions-comparison.html>`__.)

.. _type_casting:

**Type casting:**

.. list-table::
   :header-rows: 1

   * - Operator
     - PostgreSQL equivalent
   * - ``_cast`` (takes a CastExp_ as a value)
     - ``::``

(For more details on type casting, refer to the `Postgres docs <https://www.postgresql.org/docs/9.2/sql-createcast.html>`__.)

.. _jsonb_operators:

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
     - ``?!``
   * - ``_has_keys_all``
     - ``?&``

(For more details on JSONB operators, refer to the `Postgres docs <https://www.postgresql.org/docs/current/static/functions-json.html#FUNCTIONS-JSONB-OP-TABLE>`__.)

.. _geometry_operators:

**PostGIS related operators on GEOMETRY columns:**

.. list-table::
   :header-rows: 1

   * - Operator
     - PostGIS equivalent
   * - ``_st_contains``
     - ``ST_Contains(column, input)``
   * - ``_st_crosses``
     - ``ST_Crosses(column, input)``
   * - ``_st_equals``
     - ``ST_Equals(column, input)``
   * - ``_st_intersects``
     - ``ST_Intersect(column, input)``
   * - ``_st_overlaps``
     - ``ST_Overlaps(column, input)``
   * - ``_st_touches``
     - ``ST_Touches(column, input)``
   * - ``_st_within``
     -  ``ST_Within(column, input)``
   * - ``_st_d_within``
     - ``ST_DWithin(column, input)``

(For more details on spatial relationship operators, refer to the `PostGIS docs <http://postgis.net/workshops/postgis-intro/spatial_relationships.html>`__.)

.. note::

   - All operators take a JSON representation of ``geometry/geography`` values as input value.
   - The input value for ``_st_d_within`` operator is an object:

     .. parsed-literal::

       {
         field-name : {_st_d_within: {distance: Float, from: Value} }
       }

.. _intersect_operators:

**Intersect Operators on RASTER columns:**

.. list-table::
   :header-rows: 1

   * - Operator
     - PostgreSQL equivalent
     - Input object
   * - ``_st_intersects_rast``
     - ``ST_Intersects(column, value)``
     - ``{ _st_intersects_rast: raster }``
   * - ``_st_intersects_nband_geom``
     - ``ST_Intersects(column, nband, geommin)``
     - ``{ _st_intersects_nband_geom: {nband: Integer! geommin: geometry!}``
   * - ``_st_intersects_geom_nband``
     - ``ST_Intersects(column, geommin, nband)``
     - ``{ _st_intersects_geom_nband: {geommin: geometry! nband: Integer }``

(For more details on intersect operators on ``raster`` columns refer to the `PostGIS docs <https://postgis.net/docs/RT_ST_Intersects.html>`__.)

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
``stddev_pop``, ``variance``, ``var_samp`` and ``var_pop``.

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
