.. meta::
   :description: Filter query results and search queries in Hasura
   :keywords: hasura, docs, query, filter, search

.. _filter_queries:

Filter query results / search queries
=====================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

The **where** argument
----------------------

You can use the ``where`` argument in your queries to filter results based on some field’s values (even
nested objects' fields). You can even use multiple filters in the same ``where`` clause using the ``_and`` or the
``_or`` operators.

For example, to fetch data for an author whose name is "Sidney":

.. code-block:: graphql
   :emphasize-lines: 3

    query {
      author(
        where: {name: {_eq: "Sidney"}}
      ) {
        id
        name
      }
    }

You can also use nested objects` fields to filter rows from a table and also filter the nested objects as well.

For example, to fetch a list of authors who have articles with a rating greater than 4 along with those articles:

.. code-block:: graphql
   :emphasize-lines: 2,5

    query {
      author (where: {articles: {rating: {_gt: 4}}}) {
        id
        name
        articles (where: {rating: {_gt: 4}}) {
          id
          title
          rating
        }
      }
    }

Here ``_eq`` and ``_gt`` are examples of comparison operators that can be used in the ``where``
argument to filter on equality.

You can see the complete specification of the ``where`` argument in the :ref:`API reference <WhereExp>`.

Comparision operators
---------------------

Let’s take a look at different comparision operators that can be used to filter results.

Equality operators (_eq, _neq)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``_eq`` (equal to) or the ``_neq`` (not equal to) operators are compatible with any Postgres type other than
``json`` or ``jsonB`` (like ``Integer``, ``Float``, ``Double``, ``Text``, ``Boolean``,
``Date``/``Time``/``Timestamp``, etc.).

For more details on equality operators and Postgres equivalents, refer to the :ref:`API reference <generic_operators>`.

The following are examples of using the equality operators on different types.

**Example: Integer (works with Double, Float, Numeric, etc.)**

Fetch data about author whose ``id`` *(an integer field)* is equal to 3:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query {
          author(
            where: {id: {_eq: 3}}
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

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { author( where: {id: {_eq: 3}}) { id name}}"
      }


**Example: String or Text**

Fetch a list of authors with ``name`` *(a text field)* as "Sidney":

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query {
          author(
            where: {name: {_eq: "Sidney"}}
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

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { author( where: {name: {_eq: \"Sidney\"}}) { id name }}"
      }


**Example: Boolean**

Fetch a list of articles that have not been published (``is_published`` is a boolean field):

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query {
          article(
            where: {is_published: {_eq: false}}
          ) {
            id
            title
            is_published
          }
        }
      :response:
        {
          "data": {
            "article": [
              {
                "id": 5,
                "title": "ut blandit",
                "is_published": false
              },
              {
                "id": 8,
                "title": "donec semper sapien",
                "is_published": false
              },
              {
                "id": 10,
                "title": "dui proin leo",
                "is_published": false
              },
              {
                "id": 14,
                "title": "congue etiam justo",
                "is_published": false
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { article( where: {is_published: {_eq: false}}) { id title is_published }}"
      }


**Example: Date (works with Time, Timezone, etc.)**

Fetch a list of articles that were published on a certain date (``published_on`` is a Date field):

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query {
          article(
            where: {published_on: {_eq: "2017-05-26"}}
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
                "id": 3,
                "title": "amet justo morbi",
                "published_on": "2017-05-26"
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { article(where: {published_on: {_eq: \"2017-05-26\"}}) { id title published_on }}"
      }
        

Greater than or less than operators (_gt, _lt, _gte, _lte)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``_gt`` (greater than), ``_lt`` (less than), ``_gte`` (greater than or equal to),
``_lte`` (less than or equal to) operators are compatible with any Postgres type other than ``json`` or ``jsonB``
(like ``Integer``, ``Float``, ``Double``, ``Text``, ``Boolean``, ``Date``/``Time``/``Timestamp``, etc.).

For more details on greater than or less than operators and Postgres equivalents, refer to the :ref:`API reference <generic_operators>`.

The following are examples of using these operators on different types:


**Example: Integer (works with Double, Float, etc.)**

Fetch a list of articles rated 4 or more (``rating`` is an integer field):

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query {
          article(
            where: {rating: {_gte: 4}}
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

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { article(where: {rating: {_gte: 4}}) { id title rating }}"
      }

**Example: String or Text**

Fetch a list of authors whose names begin with M or any letter that follows M *(essentially, a filter based on a
dictionary sort)*:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query {
          author(
            where: {name: {_gt: "M"}}
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
              },
              {
                "id": 9,
                "name": "Ninnetta"
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { author(where: {name: {_gt: \"M\"}}) { id name }}"
      }

**Example: Date (works with Time, Timezone, etc.)**

Fetch a list of articles that were published on or after date "01/01/2018":

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query {
          article(
            where: {published_on: {_gte: "2018-01-01"}}
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
                "id": 2,
                "title": "a nibh",
                "published_on": "2018-06-10"
              },
              {
                "id": 6,
                "title": "sapien ut",
                "published_on": "2018-01-08"
              },
              {
                "id": 13,
                "title": "vulputate elementum",
                "published_on": "2018-03-10"
              },
              {
                "id": 15,
                "title": "vel dapibus at",
                "published_on": "2018-01-02"
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { article(where: {published_on: {_gte: \"2018-01-01\"}}) { id title published_on }}"
      }

List based search operators (_in, _nin)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``_in`` (in a list) and ``_nin`` (not in list) operators are used to compare field values to a list of values.
They are compatible with any Postgres type other than ``json`` or ``jsonB`` (like ``Integer``, ``Float``, ``Double``,
``Text``, ``Boolean``, ``Date``/``Time``/``Timestamp``, etc.).

For more details on list based search operators and Postgres equivalents, refer to the :ref:`API reference <generic_operators>`.

The following are examples of using these operators on different types:

**Example: Integer (works with Double, Float, etc.)**

Fetch a list of articles rated 1, 3 or 5:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query {
          article(
            where: {rating: {_in: [1,3,5]}}
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
                "title": "sit amet",
                "rating": 1
              },
              {
                "id": 2,
                "title": "a nibh",
                "rating": 3
              },
              {
                "id": 6,
                "title": "sapien ut",
                "rating": 1
              },
              {
                "id": 17,
                "title": "montes nascetur ridiculus",
                "rating": 5
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { article(where: {rating: {_in: [1,3,5]}}) { id title rating }}"
      }

**Example: String or Text**

Fetch a list of those authors whose names are NOT part of a list:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query {
          author(
            where: {name: {_nin: ["Justin","Sidney","April"]}}
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
                "id": 2,
                "name": "Beltran"
              },
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
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { author(where: {name: {_nin: [\"Justin\",\"Sidney\",\"April\"]}}) { id name }}"
      }

Text search or pattern matching operators (_like, _similar, etc.)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``_like``, ``_nlike``, ``_ilike``, ``_nilike``, ``_similar``, ``_nsimilar`` operators are used for
pattern matching on string/text fields.

For more details on text search operators and Postgres equivalents, refer to the :ref:`API reference <text_operators>`.

**Example: _like**

Fetch a list of articles whose titles contain the word “amet”:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query {
          article(
            where: {title: {_like: "%amet%"}}
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

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { article(where: {title: {_like: \"%amet%\"}}) { id title }}"
      }

.. note::

  ``_like`` is case-sensitive. Use ``_ilike`` for case-insensitive search.


**Example: _similar**

Fetch a list of authors whose names begin with A or C:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query {
          author(
            where: {name: {_similar: "(A|C)%"}}
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

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { author(where: {name: {_similar: \"(A|C)%\"}}) {id name }}"
      }

.. note::

  ``_similar`` is case-sensitive

JSONB operators (_contains, _has_key, etc.)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``_contains``, ``_contained_in``, ``_has_key``, ``_has_keys_any`` and ``_has_keys_all`` operators are used to filter
based on ``JSONB`` columns.

For more details on JSONB operators and Postgres equivalents, refer to the :ref:`API reference <jsonb_operators>`.

**Example: _contains**

Fetch all authors living within a particular pincode (present in ``address`` JSONB column):

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query get_authors_in_pincode ($jsonFilter: jsonb){
          author(
            where: {
              address: {_contains: $jsonFilter }
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
      :variables:
        {
          "jsonFilter": {
            "pincode": 560095
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query get_authors_in_pincode ($jsonFilter: jsonb){ author(where: { address: {_contains: $jsonFilter }}) { id name address }}",
        "variables": {
          "jsonFilter": {
            "pincode": 560095
          }
        }
      }

**Example: _has_key**

Fetch authors if the ``phone`` key is present in their JSONB ``address`` column:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query get_authors_if_phone {
          author(
            where: {
              address: {_has_key: "phone" }
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
                  "phone": "9090909090"
                }
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query get_authors_if_phone { author(where: { address: {_has_key: \"phone\" }}) { id name address }}"
      }


PostGIS spatial relationship operators (_st_contains, _st_crosses, etc.)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``_st_contains``, ``_st_crosses``, ``_st_equals``, ``_st_intersects``, ``_st_overlaps``, ``_st_touches``,
``_st_within`` and ``_st_d_within`` operators are used to filter based on ``geometry`` like columns.

``_st_d_within`` and ``_st_intersects`` can be used on ``geography`` columns also.

For more details on spatial relationship operators and Postgres equivalents, refer to the :ref:`API reference <geometry_operators>`.

Use JSON representation (see `GeoJSON <https://tools.ietf.org/html/rfc7946>`_) of ``geometry`` and ``geography`` values in
``variables`` as shown in the following examples:


**Example: _st_within**

Fetch a list of geometry values which are within the given ``polygon`` value:


.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query geom_table($polygon: geometry){
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

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query geom_table($polygon: geometry){ geom_table(where: {geom_col: {_st_within: $polygon}}){ id geom_col }}",
        "variables": {
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
      }

**Example: _st_d_within**

Fetch a list of ``geometry`` values which are 3 units from given ``point`` value:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query geom_table($point: geometry){
          geom_table(
            where: {geom_col: {_st_d_within: {distance: 3, from: $point}}}
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
              },
              {
                "id": 2,
                "geom_col": {
                  "type": "Point",
                  "coordinates": [
                    3,
                    0
                  ]
                }
              }
            ]
          }
        }
      :variables:
        {
          "point": {
            "type": "Point",
            "coordinates": [ 0, 0 ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query geom_table($point: geometry){ geom_table(where: {geom_col: {_st_d_within: {distance: 3, from: $point}}}){ id geom_col }}",
        "variables": {
          "point": {
            "type": "Point",
            "coordinates": [
              0,
              0
            ]
          }
        }
      }

Filter or check for null values (_is_null)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Checking for null values can be achieved using the ``_is_null`` operator.

For more details on the ``_is_null`` operator and Postgres equivalent, refer to the :ref:`API reference <null_expression>`.

**Example: Filter null values in a field**

Fetch a list of articles that have a value in the ``published_on`` field:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query {
          article(
            where: {published_on: {_is_null: false}}
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

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { article(where: {published_on: {_is_null: false}}) { id title published_on }}"
      }

Intersect operators on RASTER columns (_st_intersects_rast, etc)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Intersect operators on columns with ``raster`` type are supported.
Please submit a feature request via `GitHub <https://github.com/hasura/graphql-engine>`__ if you want support for more functions.

For more details on intersect operators on raster columns and Postgres equivalents, refer to the :ref:`API reference <intersect_operators>`.

**Example: _st_intersects_rast**


Filter the raster values which intersect the input raster value.

Executes the following SQL function:

.. code-block:: sql

   boolean ST_Intersects( raster <raster-col> , raster <raster-value> );

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

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

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query getIntersectingValues ($rast: raster){ dummy_rast(where: {rast: {_st_intersects_rast: $rast}}){ rid rast }}",
        "variables": {
          "rast": "0100000100000000000000004000000000000000C00000000000000000000000000000084000000000000000000000000000000000E610000001000100440001"
        }
      }

**Example: _st_intersects_geom_nband**

Filter the raster values which intersect the input geometry value and optional band number.

Executes the following SQL function:

.. code-block:: sql

   boolean ST_Intersects( raster <raster-col> , geometry geommin , integer nband=NULL );

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

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

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query getIntersectingValues ($point: geometry!){ dummy_rast(where: {rast: {_st_intersects_geom_nband: {geommin: $point}}}){ rid rast }}",
        "variables": {
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
      }

**Example: _st_intersects_nband_geom**

Filter the raster values (with specified band number) which intersect the input geometry value.

Executes the following SQL function:

.. code-block:: sql

   boolean ST_Intersects( raster <raster-col> , integer nband , geometry geommin );

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

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

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query getIntersectingValues ($point: geometry!){ dummy_rast(where: {rast: {_st_intersects_nband_geom: {nband: 5 geommin: $point}}}){ rid rast }}",
        "variables": {
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
      }

Filter based on failure of some criteria (_not)
-----------------------------------------------

The ``_not`` operator can be used to fetch results for which some condition does not hold true. i.e. to invert the
filter set for a condition.

**Example: _not**

Fetch all authors who don't have any published articles:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        {
          author(
            where: {
              _not: {
                articles: { is_published: {_eq: true} }
              }
            }) {
            id
            name
            articles {
              title
              is_published
            }
          }
        }
      :response:
        {
          "data": {
            "author": [
              {
                "id": 7,
                "name": "Berti",
                "articles": [
                  {
                    "title": "ipsum primis in",
                    "is_published": false
                  }
                ]
              },
              {
                "id": 9,
                "name": "Ninnetta",
                "articles": []
              },
              {
                "id": 10,
                "name": "Lyndsay",
                "articles": [
                  {
                    "title": "dui proin leo",
                    "is_published": false
                  }
                ]
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "{ author(where: { _not: { articles: { is_published: {_eq: true} }}}) { id name articles { title is_published }}}"
      }

Using multiple filters in the same query (_and, _or)
----------------------------------------------------

You can group multiple parameters in the same ``where`` argument using the ``_and`` or the ``_or`` operators to filter
results based on more than one criteria.

.. note::
  You can use the ``_or`` and ``_and`` operators along with the ``_not`` operator to create arbitrarily complex boolean
  expressions involving multiple filtering criteria.

**Example:  _and**

Fetch a list of articles published in a specific time-frame (for example: in year 2017):

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query {
          article (
            where: {
              _and: [
                { published_on: {_gte: "2017-01-01"}},
                { published_on: {_lte: "2017-12-31"}}
              ]
            }
          )
          {
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
                "id": 3,
                "title": "amet justo morbi",
                "published_on": "2017-05-26"
              },
              {
                "id": 4,
                "title": "vestibulum ac est",
                "published_on": "2017-03-05"
              },
              {
                "id": 9,
                "title": "sit amet",
                "published_on": "2017-05-16"
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { article (where: { _and: [{ published_on: {_gte: \"2017-01-01\"}}, { published_on: {_lte: \"2017-12-31\"}}]}) { id title published_on }}"
      }

.. note::

  Certain ``_and`` expressions can be expressed in a simpler format using some syntactic sugar. See the
  :ref:`API reference <AndExp>` for more details.

**Example:  _or**

Fetch a list of articles rated more than 4 or published after "01/01/2018":

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query {
          article (
            where: {
              _or: [
                {rating: {_gte: 4}},
                {published_on: {_gte: "2018-01-01"}}
              ]
            }
          )
          {
            id
            title
            rating
            published_on
          }
        }
      :response:
        {
          "data": {
            "article": [
              {
                "id": 2,
                "title": "a nibh",
                "rating": 3,
                "published_on": "2018-06-10"
              },
              {
                "id": 3,
                "title": "amet justo morbi",
                "rating": 4,
                "published_on": "2017-05-26"
              },
              {
                "id": 6,
                "title": "sapien ut",
                "rating": 1,
                "published_on": "2018-01-08"
              },
              {
                "id": 7,
                "title": "nisl duis ac",
                "rating": 4,
                "published_on": "2016-07-09"
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { article (where: { _or: [ {rating: {_gte: 4}}, {published_on: {_gte: \"2018-01-01\"}}]}) { id title rating published_on }}"
      }

.. note::

  The ``_or`` operator expects an array of expressions as input. If an object is passed as input it will behave like
  the ``_and`` operator as explained in the :ref:`API reference <OrExp>`

.. _nested_filter:

Filter nested objects
---------------------

The ``where`` argument can be used in **array relationships** as well to filter the nested objects.
**Object relationships** have only one nested object and hence they do not expose the ``where`` argument.

**Example:**

Fetch all authors with only their 5 rated articles:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        {
          author {
            id
            name
            articles(where: {rating: {_eq: 5}}) {
              title
              rating
            }
          }
        }
      :response:
        {
          "data": {
            "author": [
              {
                "id": 1,
                "name": "Justin",
                "articles": []
              },
              {
                "id": 2,
                "name": "Beltran",
                "articles": []
              },
              {
                "id": 5,
                "name": "Amii",
                "articles": [
                  {
                    "title": "montes nascetur ridiculus",
                    "rating": 5
                  }
                ]
              },
              {
                "id": 6,
                "name": "Corny",
                "articles": []
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "{ author { id name articles(where: {rating: {_eq: 5}}) { title rating }}}"
      }


Filter based on nested objects' fields
--------------------------------------

You can use the fields of nested objects as well to filter your query results.

For example:

.. code-block:: graphql
   :emphasize-lines: 2

      query {
        article (where: {author: {name: {_eq: "Sidney"}}}) {
          id
          title
        }
      }

The behaviour of the comparision operators depends on whether the nested objects are a single object related via an
object relationship or an array of objects related via an array relationship.

- In case of an **object relationship**, a row will be returned if the single nested object satisfies the defined
  condition.
- In case of an **array relationship**, a row will be returned if **any of the nested objects** satisfy the defined
  condition.

Let's look at a few use cases based on the above:

Fetch if the single nested object defined via an object relationship satisfies a condition
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Example:**

Fetch all articles whose author's name starts with "A":

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        {
          article (
            where: {
              author: {
                name: { _similar: "A%"}
              }
            }
          ) {
            id
            title
            author {
              name
            }
          }
        }
      :response:
        {
          "data": {
            "article": [
              {
                "id": 1,
                "title": "sit amet",
                "author": {
                  "name": "Anjela"
                }
              },
              {
                "id": 3,
                "title": "amet justo morbi",
                "author": {
                  "name": "Anjela"
                }
              },
              {
                "id": 4,
                "title": "vestibulum ac est",
                "author": {
                  "name": "Amii"
                }
              },
              {
                "id": 12,
                "title": "volutpat quam pede",
                "author": {
                  "name": "Amii"
                }
              },
              {
                "id": 13,
                "title": "vulputate elementum",
                "author": {
                  "name": "April"
                }
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "{ article (where: { author: { name: { _similar: \"A%\"}}}) { id title author { name }}}"
      }


Fetch if **any** of the nested objects defined via an array relationship satisfy a condition
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Example:**

Fetch all authors which have written at least one article which is rated 1:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        {
          author(
            where: {
              articles: {rating: {_eq: 1}}
            }
          ) {
            id
            name
            articles {
              title
              rating
            }
          }
        }
      :response:
        {
          "data": {
            "author": [
              {
                "id": 1,
                "name": "Justin",
                "articles": [
                  {
                    "title": "sem duis aliquam",
                    "rating": 1
                  },
                  {
                    "title": "vel dapibus at",
                    "rating": 4
                  }
                ]
              },
              {
                "id": 4,
                "name": "Anjela",
                "articles": [
                  {
                    "title": "sit amet",
                    "rating": 1
                  },
                  {
                    "title": "amet justo morbi",
                    "rating": 4
                  }
                ]
              },
              {
                "id": 3,
                "name": "Sidney",
                "articles": [
                  {
                    "title": "sapien ut",
                    "rating": 1
                  },
                  {
                    "title": "turpis eget",
                    "rating": 3
                  },
                  {
                    "title": "congue etiam justo",
                    "rating": 4
                  }
                ]
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "{ author(where: { articles: {rating: {_eq: 1}}}) { id name articles { title rating }}}"
      }

Fetch if **all** of the nested objects defined via an array relationship satisfy a condition
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default a row is returned if any of the nested objects satisfy a condition. To achieve the above, we need to frame
the ``where`` expression as ``{_not: {inverse-of-condition}}``. This reads as: fetch if not (any of the nested objects
satisfy the inverted condition) i.e. all of the nested objects satisfy the condition.

For example:

+---------------------------------------+-----------------------------------------------+
| condition                             | where expression                              |
+=======================================+===============================================+
| ``{object: {field: {_eq: "value"}}}`` | ``{_not: {object: {field: {_neq: "value"}}}`` |
+---------------------------------------+-----------------------------------------------+
| ``{object: {field: {_gt: "value"}}}`` | ``{_not: {object: {field: {_lte: "value"}}}`` |
+---------------------------------------+-----------------------------------------------+

**Example:**

Fetch all authors which have all of their articles published i.e. have ``{is_published {_eq: true}``.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        {
          author (
            where: {
              _not: {
                articles: {is_published: {_neq: true}}
              }
            }
          ) {
            id
            name
            articles {
              title
              is_published
            }
          }
        }
      :response:
        {
          "data": {
            "author": [
              {
                "id": 1,
                "name": "Justin",
                "articles": [
                  {
                    "title": "vel dapibus at",
                    "is_published": true
                  },
                  {
                    "title": "sem duis aliquam",
                    "is_published": true
                  }
                ]
              },
              {
                "id": 2,
                "name": "Beltran",
                "articles": [
                  {
                    "title": "a nibh",
                    "is_published": true
                  },
                  {
                    "title": "sit amet",
                    "is_published": true
                  }
                ]
              },
              {
                "id": 4,
                "name": "Anjela",
                "articles": [
                  {
                    "title": "sit amet",
                    "is_published": true
                  }
                ]
              },
              {
                "id": 8,
                "name": "April",
                "articles": [
                  {
                    "title": "vulputate elementum",
                    "is_published": true
                  },
                  {
                    "title": "eu nibh",
                    "is_published": true
                  }
                ]
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "{ author (where: { _not: { articles: {is_published: {_neq: true}}}}) { id name articles { title is_published }}}"
      }


Fetch if **none** of the nested objects defined via an array relationship satisfy a condition
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default a row is returned if any of the nested objects satisfy a condition. To achieve the above, we need to frame
the ``where`` expression as ``{_not: {condition}}``. This reads as: fetch if not (any of the nested objects
satisfy the condition) i.e. none of the nested objects satisy the condition.

For example,

+---------------------------------------+----------------------------------------------+
| condition                             | where expression                             |
+=======================================+==============================================+
| ``{object: {field: {_eq: "value"}}}`` | ``{_not: {object: {field: {_eq: "value"}}}`` |
+---------------------------------------+----------------------------------------------+
| ``{object: {field: {_gt: "value"}}}`` | ``{_not: {object: {field: {_gt: "value"}}}`` |
+---------------------------------------+----------------------------------------------+

**Example:**

Fetch all authors which have none of their articles published i.e. have ``{is_published {_eq: true}``:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        {
          author (
            where: {
              _not: {
                articles: {is_published: {_eq: true}}
              }
            }
          ) {
            id
            name
            articles {
              title
              is_published
            }
          }
        }
      :response:
        {
          "data": {
            "author": [
              {
                "id": 7,
                "name": "Berti",
                "articles": [
                  {
                    "title": "ipsum primis in",
                    "is_published": false
                  }
                ]
              },
              {
                "id": 10,
                "name": "Lyndsay",
                "articles": [
                  {
                    "title": "dui proin leo",
                    "is_published": false
                  }
                ]
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "{ author (where: { _not: { articles: {is_published: {_eq: true}}}}) { id name articles { title is_published }}}"
      }

Fetch if nested object(s) exist/do not exist
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can filter results based on if they have nested objects by checking if any nested objects exist. This can be
achieved by using the expression ``{}`` which evaluates to ``true`` if any object exists.


**Example where nested object(s) exist:**

Fetch all authors which have at least one article written by them:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        {
          author (
            where: {
              articles: {}
            }
          ) {
            id
            name
            articles_aggregate {
              aggregate {
                count
              }
            }
          }
        }
      :response:
        {
          "data": {
            "author": [
              {
                "id": 1,
                "name": "Justin",
                "articles_aggregate": {
                  "aggregate": {
                    "count": 2
                  }
                }
              },
              {
                "id": 4,
                "name": "Anjela",
                "articles_aggregate": {
                  "aggregate": {
                    "count": 1
                  }
                }
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "{ author (where: { articles: {}}) { id name articles_aggregate { aggregate { count }}}}"
      }

**Example where nested object(s) do not exist:**

Fetch all authors which have not written any articles:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        {
          author (
            where: {
              _not: {
                articles: {}
              }
            }
          ) {
            id
            name
            articles_aggregate {
              aggregate {
                count
              }
            }
          }
        }
      :response:
        {
          "data": {
            "author": [
              {
                "id": 2,
                "name": "Beltran",
                "articles_aggregate": {
                  "aggregate": {
                    "count": 0
                  }
                }
              },
              {
                "id": 3,
                "name": "Sidney",
                "articles_aggregate": {
                  "aggregate": {
                    "count": 0
                  }
                }
              }
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "{ author (where: { _not: { articles: {}}}) { id name articles_aggregate { aggregate { count }}}}"
      }

Cast a field to a different type before filtering (_cast)
---------------------------------------------------------

The ``_cast`` operator can be used to cast a field to a different type, which allows type-specific
operators to be used on fields that otherwise would not support them. Currently, only casting
between PostGIS ``geometry`` and ``geography`` types is supported.

Casting using ``_cast`` corresponds directly to
`SQL type casts <https://www.postgresql.org/docs/current/sql-expressions.html#SQL-SYNTAX-TYPE-CASTS>`__.

**Example: cast ``geometry`` to ``geography``**

Filtering using ``_st_d_within`` over large distances can be inaccurate for location data stored in
``geometry`` columns. For accurate queries, cast the field to ``geography`` before comparing:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

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

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query cities_near($point: geography!, $distance: Float!) { cities(where: {location: { _cast: {geography: { _st_d_within: {from: $point, distance: $distance}}}}}) { name }}",
        "variables": {
          "point": {
            "type": "Point",
            "coordinates": [
              1,
              50
            ]
          },
          "distance": 1000000
        }
      }

**Example: cast ``geography`` to ``geometry``**

Columns of type ``geography`` are more accurate, but they don’t support as many operations as
``geometry``. Cast to ``geometry`` to use those operations in a filter:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. graphiql::
      :view_only:
      :query:
        query cities_inside($polygon: geometry) {
          cities(
            where: {location: {
              _cast: {geometry: {
                _st_within: $polygon
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
                "name": "New York"
              }
            ]
          }
        }
      :variables:
        {
          "polygon": {
            "type": "Polygon",
            "crs": {
              "type": "name",
              "properties": { "name": "EPSG:4326" }
            },
            "coordinates": [
              [
                [-75, 40],
                [-74, 40],
                [-74, 41],
                [-75, 41],
                [-75, 40]
              ]
            ]
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query cities_inside($polygon: geometry) { cities(where: {location: { _cast: {geometry: { _st_within: $polygon }}}}) { name }}",
        "variables": {
          "polygon": {
            "type": "Polygon",
            "crs": {
                "type": "name",
                "properties": {
                    "name": "EPSG:4326"
                }
            },
            "coordinates": [
              [
                [
                  -75,
                  40
                ],
                [
                  -74,
                  40
                ],
                [
                  -74,
                  41
                ],
                [
                  -75,
                  41
                ],
                [
                  -75,
                  40
                ]
              ]
            ]
          }
        }
      }

.. note::

  For performant queries that filter on casted fields, create an
  `expression index <https://www.postgresql.org/docs/current/indexes-expressional.html>`__
  on the casted column. For example, if you frequently perform queries on a field ``location`` of
  type ``geometry`` casted to type ``geography``, you should create an index like the following:

  .. code-block:: sql

    CREATE INDEX cities_location_geography ON cities USING GIST ((location::geography));

.. _true_expression:

The TRUE expression ( **{ }** )
-------------------------------

The expression ``{}`` evaluates to ``true`` for all objects.

**For example**:

- any query with the condition ``{ where: {} }`` will return all objects without
  applying any filter.

- any query with the condition ``{ where: { nested_object: {} } }`` will return all
  objects for which atleast one ``nested_object`` exists.

.. _null_value_evaluation:

Evaluation of **null** values in comparision expressions
--------------------------------------------------------

If in any comparision expression a ``null`` (or ``undefined``) value is passed, the expression currently gets
reduced to ``{}`` (:ref:`TRUE expression <true_expression>`)

**For example**, the expression ``{ where: { _eq: null } }`` will be reduced to ``{ where: {} }``
