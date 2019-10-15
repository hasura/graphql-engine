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

    For more examples and details of usage, please see :doc:`this <../../queries/index>`.

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
Fetch all authors with the name Sidney:

.. graphiql::
  :view_only:
  :query:
    query {
      author(
        where: {
          name: {_eq: "Sidney"}
        }
      ) {
        id
        name
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 3,
            "name": "Sidney"
          }
        ]
      }
    }

- ``_neq``
Fetch all authors that aren't named Sidney:

.. graphiql::
  :view_only:
  :query:
    query {
      author(
        where: {
          name: {_neq: "Sidney"}
        }
      ) {
        id
        name
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 1,
            "name": "Ash"
          }
        ]
      }
    }

- ``_in``
Fetch all authors which names are present in a list:

.. graphiql::
  :view_only:
  :query:
    query {
      author(
        where: {
          name: {_in: ["Sidney", "Ash"]}
        }
      ) {
        id
        name
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 1,
            "name": "Ash"
          },
          {
            "id": 3,
            "name": "Sidney"
          }
        ]
      }
    }

- ``_nin``
Fetch all authors which names aren't in a list:

.. graphiql::
  :view_only:
  :query:
    query {
      author(
        where: {
          name: {_nin: ["Sidney", "Bob"]}
        }
      ) {
        id
        name
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 1,
            "name": "Ash"
          }
        ]
      }
    }

- ``_gt``
Fetch all articles with rating greater than 3:

.. graphiql::
  :view_only:
  :query:
    query {
      article(
        where: {
          rating: {_gt: 3}
        }
      ) {
        id
        title
        rating
      }
    }
  :response:
    {
      "data": {
        "article": [
          {
            "id": 3,
            "title": "amet justo morbi",
            "rating": 4
          },
          {
            "id": 7,
            "title": "nisl duis ac",
            "rating": 4
          },
          {
            "id": 17,
            "title": "montes nascetur ridiculus",
            "rating": 5
          }
        ]
      }
    }

- ``_lt``
Fetch all articles with rating less than 3:

.. graphiql::
  :view_only:
  :query:
    query {
      article(
        where: {
          rating: {_lt: 3}
        }
      ) {
        id
        title
        rating
      }
    }
  :response:
    {
      "data": {
        "article": [
          {
            "id": 1,
            "title": "lorem ipsum",
            "rating": 2
          },
          {
            "id": 11,
            "title": "consectetur",
            "rating": 1
          }
        ]
      }
    }

- ``_gte``
Fetch all articles with rating greater than and equal to 3:

.. graphiql::
  :view_only:
  :query:
    query {
      article(
        where: {
          rating: {_gte: 3}
        }
      ) {
        id
        title
        rating
      }
    }
  :response:
    {
      "data": {
        "article": [
          {
            "id": 3,
            "title": "amet justo morbi",
            "rating": 4
          },
          {
            "id": 5,
            "title": "commodo odio",
            "rating": 3
          },
          {
            "id": 7,
            "title": "nisl duis ac",
            "rating": 4
          },
          {
            "id": 17,
            "title": "montes nascetur ridiculus",
            "rating": 5
          }
        ]
      }
    }

- ``_lte``
Fetch all articles with rating less than and equal to 3:

.. graphiql::
  :view_only:
  :query:
    query {
      article(
        where: {
          rating: {_lte: 3}
        }
      ) {
        id
        title
        rating
      }
    }
  :response:
    {
      "data": {
        "article": [
          {
            "id": 1,
            "title": "lorem ipsum",
            "rating": 2
          },
          {
            "id": 5,
            "title": "commodo odio",
            "rating": 3
          },
          {
            "id": 11,
            "title": "consectetur",
            "rating": 1
          }
        ]
      }
    }

**Text related operators:**

- ``_like``
Fetch all articles whose titles contain the word "amet" (case sensitive):

.. graphiql::
  :view_only:
  :query:
    query {
      article(
        where: {
          title: {_like: "%amet%"}
        }
      ) {
        id
        title
      }
    }
  :response:
    {
      "data": {
        "article": [
          {
            "id": 1,
            "title": "sit amet"
          },
          {
            "id": 3,
            "title": "amet justo morbi"
          },
          {
            "id": 9,
            "title": "sit amet"
          }
        ]
      }
    }

- ``_nlike``
Fetch all articles whose titles do not contain the word "amet" (case sensitive):

.. graphiql::
  :view_only:
  :query:
    query {
      article(
        where: {
          title: {_nlike: "%amet%"}
        }
      ) {
        id
        title
      }
    }
  :response:
    {
      "data": {
        "article": [
          {
            "id": 5,
            "title": "ut blandit"
          },
          {
            "id": 8,
            "title": "donec semper sapien"
          },
          {
            "id": 10,
            "title": "dui proin leo"
          },
          {
            "id": 14,
            "title": "congue etiam justo"
          },
          {
            "id": 22,
            "title": "sit Amet"
        ]
      }
    }

- ``_ilike``
Fetch all articles whose titles contain the word "amet" (case insensitive):

.. graphiql::
  :view_only:
  :query:
    query {
      article(
        where: {
          title: {_ilike: "%amet%"}
        }
      ) {
        id
        title
      }
    }
  :response:
    {
      "data": {
        "article": [
          {
            "id": 1,
            "title": "sit amet"
          },
          {
            "id": 3,
            "title": "amet justo morbi"
          },
          {
            "id": 9,
            "title": "sit amet"
          },
          {
            "id": 22,
            "title": "sit Amet"
          }
        ]
      }
    }

- ``_nilike``
Fetch all articles whose titles do not contain the word "amet" (case insensitive)

.. graphiql::
  :view_only:
  :query:
    query {
      article(
        where: {
          title: {_nilike: "%amet%"}
        }
      ) {
        id
        title
      }
    }
  :response:
    {
      "data": {
        "article": [
          {
            "id": 5,
            "title": "ut blandit"
          },
          {
            "id": 8,
            "title": "donec semper sapien"
          },
          {
            "id": 10,
            "title": "dui proin leo"
          },
          {
            "id": 14,
            "title": "congue etiam justo"
          }
        ]
      }
    }

- ``_similar``
Fetch all authors whose names begin with A or C:

.. graphiql::
  :view_only:
  :query:
    query {
      author(
        where: {
          name: {_similar: "(A|C)%"}
        }
      ) {
        id
        name
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 4,
            "name": "Anjela"
          },
          {
            "id": 5,
            "name": "Amii"
          },
          {
            "id": 6,
            "name": "Corny"
          },
          {
            "id": 8,
            "name": "April"
          }
        ]
      }
    }

- ``_nsimilar``
Fetch all authors whose names don't begin with A or C:

.. graphiql::
  :view_only:
  :query:
    query {
      author(
        where: {
          name: {_nsimilar: "(A|C)%"}
        }
      ) {
        id
        name
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 3,
            "name": "Sidney"
          }
        ]
      }
    }

**Checking for NULL values:**

- ``_is_null`` (takes true/false as values)
Fetch all articles which the published date isn't NULL:

.. graphiql::
  :view_only:
  :query:
    query {
      article(
        where: {
          published_on: {_is_null: false}
        }
      ) {
        id
        title
        published_on
      }
    }
  :response:
    {
      "data": {
        "article": [
          {
            "id": 1,
            "title": "sit amet",
            "published_on": "2017-08-09"
          },
          {
            "id": 2,
            "title": "a nibh",
            "published_on": "2018-06-10"
          },
          {
            "id": 3,
            "title": "amet justo morbi",
            "published_on": "2017-05-26"
          },
          {
            "id": 4,
            "title": "vestibulum ac est",
            "published_on": "2017-03-05"
          }
        ]
      }
    }

**Type casting:**

- ``_cast`` (takes a CastExp_ as a value)
The ``_cast`` operator can be used to cast a field to a different type, which allows type-specific
operators to be used on fields that otherwise would not support them. Currently, only casting
between PostGIS ``geometry`` and ``geography`` types is supported.

Casting using ``_cast`` corresponds directly to
`SQL type casts <https://www.postgresql.org/docs/current/sql-expressions.html#SQL-SYNTAX-TYPE-CASTS>`__.

Example: cast ``geometry`` to ``geography``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Filtering using ``_st_d_within`` over large distances can be inaccurate for location data stored in
``geometry`` columns. For accurate queries, cast the field to ``geography`` before comparing:

.. graphiql::
  :view_only:
  :query:
    query cities_near($point: geography!, $distance: Float!) {
      cities(
        where: {location: {
          _cast: {geography: {
            _st_d_within: {from: $point, distance: $distance}
          }}
        }}
      ) {
        name
      }
    }
  :response:
    {
      "data": {
        "cities": [
          {
            "name": "London"
          },
          {
            "name": "Paris"
          }
        ]
      }
    }
    :variables:
    {
      "point": {
        "type": "Point",
        "coordinates": [1, 50]
      },
      "distance": 1000000
    }

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

(For more details on what these operators do, refer to the `Postgres docs <https://www.postgresql.org/docs/current/static/functions-json.html#FUNCTIONS-JSONB-OP-TABLE>`__).

Example: _contains
^^^^^^^^^^^^^^^^^^
Fetch all authors living within a particular pincode (present in ``address`` JSONB column):

.. graphiql::
  :view_only:
  :query:
    query {
      author(
        where: {
          address: {_contains: 560095 }
        }
      ) {
        id
        name
        address
      }
    }
  :response:
    {
      "data": {
        "author": [
          {
            "id": 1,
            "name": "Ash",
            "address": {
              "street_address": "161, 19th Main Road, Koramangala 6th Block",
              "city": "Bengaluru",
              "state": "Karnataka",
              "pincode": 560095,
              "phone": "9090909090",
            }
          }
        ]
      }
    }

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

(For more details on what these operators do, refer to the `PostGIS docs <http://postgis.net/workshops/postgis-intro/spatial_relationships.html>`__).

.. note::

   - All operators take a JSON representation of ``geometry/geography`` values as input value.
   - The input value for ``_st_d_within`` operator is an object:

     .. parsed-literal::

       {
         field-name : {_st_d_within: {distance: Float, from: Value} }
       }

Example: _st_within
^^^^^^^^^^^^^^^^^^^
Fetch a list of geometry values which are within the given ``polygon`` value:

.. graphiql::
  :view_only:
  :query:
    query {
      geom_table(
        where: {geom_col: {_st_within: $polygon}}
      ){
        id
        geom_col
      }
    }
  :response:
    {
      "data": {
        "geom_table": [
          {
            "id": 1,
            "geom_col": {
              "type": "Point",
              "coordinates": [
                1,
                2
              ]
            }
          }
        ]
      }
    }
  :variables:
    {
      "polygon": {
        "type": "Polygon",
        "coordinates": [
          [
            [ 0, 0 ],
            [ 0, 2 ],
            [ 2, 2 ],
            [ 2, 0 ],
            [ 0, 0 ]
          ]
        ]
      }
    }

**Intersect Operators on RASTER columns:**

- ``_st_intersects_rast``

Executes ``boolean ST_Intersects( raster <raster-column> , raster <input-raster> )``

.. parsed-literal ::

   { _st_intersects_rast: raster }

Example: _st_intersects_rast
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Filter the raster values which intersect the input raster value.

Executes the following SQL function:

.. code-block:: sql

   boolean ST_Intersects( raster <raster-col> , raster <raster-value> );


.. graphiql::
  :view_only:
  :query:
   query getIntersectingValues ($rast: raster){
     dummy_rast(where: {rast: {_st_intersects_rast: $rast}}){
       rid
       rast
     }
   }
  :response:
   {
     "data": {
       "dummy_rast": [
         {
           "rid": 1,
           "rast": "01000001009A9999999999E93F9A9999999999E9BF000000000000F0BF000000000000104000000000000000000000000000000000E610000005000500440000010101000101010101010101010101010101010001010100"
         },
         {
           "rid": 2,
           "rast": "0100000100166C8E335B91F13FE2385B00285EF6BF360EE40064EBFFBF8D033900D9FA134000000000000000000000000000000000E610000005000500440000000101010001010101010101010101010101000101010000"
         }
       ]
     }
   }
  :variables:
   {
     "rast": "0100000100000000000000004000000000000000C00000000000000000000000000000084000000000000000000000000000000000E610000001000100440001"
   }

- ``_st_intersects_nband_geom``

Executes ``boolean ST_Intersects( raster <raster-column> , integer nband , geometry geommin )``

This accepts ``st_intersects_nband_geom_input`` input object

.. parsed-literal ::

   { _st_intersects_nband_geom: {nband: Integer! geommin: geometry!}

Example: _st_intersects_nband_geom
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Filter the raster values (with specified band number) which intersect the input geometry value.

Executes the following SQL function:

.. code-block:: sql

   boolean ST_Intersects( raster <raster-col> , integer nband , geometry geommin );


.. graphiql::
  :view_only:
  :query:
    query getIntersectingValues ($point: geometry!){
      dummy_rast(where: {rast: {_st_intersects_nband_geom: {nband: 5 geommin: $point}}}){
        rid
        rast
      }
    }
  :response:
   {
     "data": {
       "dummy_rast": [
         {
           "rid": 1,
           "rast": "01000001009A9999999999E93F9A9999999999E9BF000000000000F0BF000000000000104000000000000000000000000000000000E610000005000500440000010101000101010101010101010101010101010001010100"
         },
         {
           "rid": 2,
           "rast": "0100000100166C8E335B91F13FE2385B00285EF6BF360EE40064EBFFBF8D033900D9FA134000000000000000000000000000000000E610000005000500440000000101010001010101010101010101010101000101010000"
         }
       ]
     }
   }
  :variables:
   {
     "point": {
       "type": "Point",
       "coordinates": [
         1,
         2
       ],
       "crs": {
         "type": "name",
         "properties": {
           "name": "urn:ogc:def:crs:EPSG::4326"
         }
       }
     }
   }

- ``_st_intersects_geom_nband``

Executes ``boolean ST_Intersects( raster <raster-column> , geometry geommin , integer nband = NULL )``

This accepts ``st_intersects_geom_nband_input`` input object

.. parsed-literal ::

   { _st_intersects_geom_nband: {geommin: geometry! nband: Integer }

Example: _st_intersects_geom_nband
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Filter the raster values which intersect the input geometry value and optional band number.

Executes the following SQL function:

.. code-block:: sql

   boolean ST_Intersects( raster <raster-col> , geometry geommin , integer nband=NULL );


.. graphiql::
  :view_only:
  :query:
    query getIntersectingValues ($point: geometry!){
      dummy_rast(where: {rast: {_st_intersects_geom_nband: {geommin: $point}}}){
        rid
        rast
      }
    }
  :response:
   {
     "data": {
       "dummy_rast": [
         {
           "rid": 1,
           "rast": "01000001009A9999999999E93F9A9999999999E9BF000000000000F0BF000000000000104000000000000000000000000000000000E610000005000500440000010101000101010101010101010101010101010001010100"
         },
         {
           "rid": 2,
           "rast": "0100000100166C8E335B91F13FE2385B00285EF6BF360EE40064EBFFBF8D033900D9FA134000000000000000000000000000000000E610000005000500440000000101010001010101010101010101010101000101010000"
         }
       ]
     }
   }
  :variables:
   {
     "point": {
       "type": "Point",
       "coordinates": [
         1,
         2
       ],
       "crs": {
         "type": "name",
         "properties": {
           "name": "urn:ogc:def:crs:EPSG::4326"
         }
       }
     }
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
