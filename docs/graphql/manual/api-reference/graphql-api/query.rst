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

**query** / **subscription** syntax
-----------------------------------

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

**Example: Query**

.. code-block:: graphql

    query {
      author(where: {articles: {rating: {_gte: 4}}} order_by: {name: asc}) {
        id
        name
      }
    }

**Example: Subscription**

.. code-block:: graphql

    subscription {
      author(where: {articles: rating: {_gte: 4}}} order_by: {name: asc}) {
        id
        name
      }
    }

.. note::

    For more examples and details of usage, please see :ref:`this <queries>`.

**query_by_pk** / **subscription_by_pk** syntax
-----------------------------------------------

.. code-block:: none

    query|subscription [<op-name>] {
      <query-field-name> (
        column1: value1
        column2: value2
      )
      <object-fields>
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
   * - query-field-name
     - true
     - Value
     - Name of the auto-generated query field, e.g. *article_by_pk*

**Example: Query by PK**

.. code-block:: graphql

    query {
      article_by_pk(id: 1) {
        id
        title
      }
    }

**Example: Subscription by PK**

.. code-block:: graphql

    subscription {
      article_by_pk(id: 1) {
        id
        title
      }
    }

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

*Example*

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

*Example*

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

*Example*

.. code-block:: graphql

  query {
    article(distinct_on: title) {
      title
      content
    }
  }

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

*Example*

.. code-block:: graphql

  query {
    author(where: {rating: {_gt: 4}}) {
      name
      articles {
        title
      }
    }
  }

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
    
*Example*

.. code-block:: graphql

  query {
    article(where: {_and: [{rating: {_gt: 4}}, {published_on: {_gt: "2018-01-01"}}]}) {
      title
      content
    }
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

*Example*

.. code-block:: graphql

  query {
    article(where: {_or: [{rating: {_gt: 4}}, {is_published: {_eq: true}}]}) {
      title
      content
    }
  }

.. note::

  The ``_or`` operator expects an array of expressions as input. Passing an object to it will result in the
  behaviour of the ``_and`` operator due to the way `GraphQL list input coercion <https://graphql.github.io/graphql-spec/June2018/#sec-Type-System.List>`_
  behaves.

  *Example:*

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

*Example*

.. code-block:: graphql

  query {
    article(where: {_not: {title: {_eq: ""}}} ) {
      title
      content
    }
  }

TrueExp
#######

.. parsed-literal::

    {}

*Example*

.. code-block:: graphql

  query {
    article(where: {is_published: {}}) {
      title
      content
    }
  }

ColumnExp
#########

.. parsed-literal::

    {
      field-name : {Operator_: Value }
    }

*Example*

.. code-block:: graphql

  query {
    article(where: {title: {_eq: "GraphQL Tutorial"}}) {
      title
      content
    }
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
   * - ``_gt``
     - ``>``
   * - ``_lt``
     - ``<`` 
   * - ``_gte``
     - ``>=``
   * - ``_lte``
     - ``<=``  
   * - ``_in``
     - ``IN``
   * - ``_nin``
     - ``NOT IN``  

(For more details, refer to the Postgres docs for `comparison operators <https://www.postgresql.org/docs/current/functions-comparison.html>`__ and `list based search operators <https://www.postgresql.org/docs/current/functions-comparisons.html>`_.)

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

(For more details on the ``IS NULL`` expression, refer to the `Postgres docs <https://www.postgresql.org/docs/current/functions-comparison.html>`__.)

.. _type_casting:

**Type casting:**

.. list-table::
   :header-rows: 1

   * - Operator
     - PostgreSQL equivalent
   * - ``_cast`` (takes a CastExp_ as a value)
     - ``::``

(For more details on type casting, refer to the `Postgres docs <https://www.postgresql.org/docs/current/sql-createcast.html>`__.)

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
     - ``ST_Intersects(column, input)``
   * - ``_st_overlaps``
     - ``ST_Overlaps(column, input)``
   * - ``_st_touches``
     - ``ST_Touches(column, input)``
   * - ``_st_within``
     - ``ST_Within(column, input)``
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

*Example*

.. code-block:: graphql

  query MyQuery($coordinate: geography!) {
    postgis_test_table(
      where: {
        geometry_column: {
          _cast: {
            geography: { _st_d_within: { distance: 1000000, from: $coordinate } }
          }
        }
      }
    ) {
      id
    }
  }
  
  Variables:
  {
    "coordinate": {
      "type": "Point",
      "coordinates": [ 2.5559, 49.0083 ]
    }
  }

.. note::

   Currently, only casting between ``geometry`` and ``geography`` types is allowed.

.. _OrderByExp:

OrderByExp
**********

.. parsed-literal::

   order_by: (TableOrderBy_ | [ TableOrderBy_ ])

*Example 1*

.. code-block:: graphql

  query {
    author(order_by: {rating: desc}) {
      name
      rating
    }
  }

*Example 2*

.. code-block:: graphql

  query {
    article(order_by: [{id: desc}, {author: {id: asc}}]) {
      title
      rating
    }
  }

*Example 3*

.. code-block:: graphql

  query {
    article(order_by: [{id: desc}, {author: {id: asc}}]) {
      title
      rating
    }
  }


TableOrderBy
""""""""""""

**For columns**

.. parsed-literal::

   {column: OrderByEnum_}

*Example*

.. code-block:: graphql

  query {
    article(order_by: {rating: asc}) {
      title
      content
    }
  }

**For object relations**

.. parsed-literal::
   {relation-name: TableOrderBy_}

*Example*

.. code-block:: graphql

  query {
    article(order_by: {author: {rating: desc}}) {
      title
      content
    }
  }

**For array relations aggregate**

.. parsed-literal::
   {relation-name_aggregate: AggregateOrderBy_}

*Example*

.. code-block:: graphql

  query {
    author(order_by: {articles_aggregate: {max: {rating: asc}}}) {
      name
    }
  }

Order by type for ``article`` table:

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

.. _OrderByEnum:

OrderByEnum

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

**Count aggregate**

.. parsed-literal::
   {count: OrderByEnum_}

*Example*

.. code-block:: graphql

  query {
    author(order_by: {articles_aggregate: {count: desc}}) {
      name
    }
  }

**Operation aggregate**

.. parsed-literal::
   {op_name: TableAggOpOrderBy_}

*Example*

.. code-block:: graphql

  query {
    author (order_by: {articles_aggregate: {sum: {id: desc}}}) {
      id
    }
  }

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

*Example*

.. code-block:: graphql

  query {
    article(limit: 6, offset: 2) {
      title
      content
    }
  }
