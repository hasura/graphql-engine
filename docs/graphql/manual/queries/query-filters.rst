Filter query results / search queries
=====================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

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

Let’s take a look at different comparision operators that can be used to filter results and other advanced use cases:

Equality operators (_eq, _neq)
------------------------------

The ``_eq`` (equal to) or the ``_neq`` (not equal to) operators are compatible with any Postgres type other than
``json`` or ``jsonB`` (like ``Integer``, ``Float``, ``Double``, ``Text``, ``Boolean``,
``Date``/``Time``/``Timestamp``, etc.).

The following are examples of using the equality operators on different types.

Example: Integer (works with Double, Float, Numeric, etc.)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch data about author whose ``id`` *(an integer field)* is equal to 3:

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

Example: String or Text
^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of authors with ``name`` *(a text field)* as "Sidney":

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

Example: Boolean
^^^^^^^^^^^^^^^^
Fetch a list of articles that have not been published (``is_published`` is a boolean field):

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


Example: Date (works with Time, Timezone, etc.)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of articles that were published on a certain date (``published_on`` is a Date field):

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

Greater than or less than operators (_gt, _lt, _gte, _lte)
----------------------------------------------------------

The ``_gt`` (greater than), ``_lt`` (less than), ``_gte`` (greater than or equal to),
``_lte`` (less than or equal to) operators are compatible with any Postgres type other than ``json`` or ``jsonB``
(like ``Integer``, ``Float``, ``Double``, ``Text``, ``Boolean``, ``Date``/``Time``/``Timestamp``, etc.).

The following are examples of using these operators on different types:


Example: Integer (works with Double, Float, etc.)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of articles rated 4 or more (``rating`` is an integer field):

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

Example: String or Text
^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of authors whose names begin with M or any letter that follows M *(essentially, a filter based on a
dictionary sort)*:

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

Example: Date (works with Time, Timezone, etc.)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of articles that were published on or after date "01/01/2018":

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

List based search operators (_in, _nin)
---------------------------------------

The ``_in`` (in a list) and ``_nin`` (not in list) operators are used to comparing field values to a list of values.
They are compatible with any Postgres type other than ``json`` or ``jsonB`` (like ``Integer``, ``Float``, ``Double``,
``Text``, ``Boolean``, ``Date``/``Time``/``Timestamp``, etc.).

The following are examples of using these operators on different types:

Example: Integer (works with Double, Float, etc.)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetches a list of articles rated 1, 3 or 5:

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

Example: String or Text
^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of those authors whose names are NOT part of a list: 

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

Text search or pattern matching operators (_like, _similar, etc.)
-----------------------------------------------------------------

The ``_like``, ``_nlike``, ``_ilike``, ``_nilike``, ``_similar``, ``_nsimilar`` operators are used for
pattern matching on string/text fields.

These operators behave exactly like their `SQL counterparts <https://www.postgresql.org/docs/current/static/functions-matching.html>`__

Example: _like
^^^^^^^^^^^^^^
Fetch a list of articles whose titles contain the word “amet”:

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

.. note::

  ``_like`` is case-sensitive. Use ``_ilike`` for case-insensitive search.


Example: _similar
^^^^^^^^^^^^^^^^^
Fetch a list of authors whose names begin with A or C:

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

.. note::

  ``_similar`` is case-sensitive

JSONB operators (_contains, _has_key, etc.)
-------------------------------------------

The ``_contains``, ``_contained_in``, ``_has_key``, ``_has_key_any`` and ``_has_key_all`` operators are used to filter
based on ``JSONB`` columns.

For more details on what these operators do, refer to `Postgres docs <https://www.postgresql.org/docs/current/static/functions-json.html#FUNCTIONS-JSONB-OP-TABLE>`__.

Example: _contains
^^^^^^^^^^^^^^^^^^
Fetch all authors living within a particular pincode (present in ``address`` JSONB column):

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

Example: _has_key
^^^^^^^^^^^^^^^^^
Fetch authors if the ``phone`` key is present in their JSONB ``address`` column:

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


PostGIS spatial relationship operators (_st_contains, _st_crosses, etc.)
------------------------------------------------------------------------

The ``_st_contains``, ``_st_crosses``, ``_st_equals``, ``_st_intersects``, ``_st_overlaps``, ``_st_touches``,
``_st_within`` and ``_st_d_within`` operators are used to filter based on ``geometry`` like columns.

``_st_d_within`` and ``_st_intersects`` can be used on ``geography`` columns also.

For more details on what these operators do, refer to
`PostGIS spatial relationship docs <http://postgis.net/workshops/postgis-intro/spatial_relationships.html>`_.

Use JSON representation (see `GeoJSON <https://tools.ietf.org/html/rfc7946>`_) of ``geometry`` and ``geography`` values in
``variables`` as shown in the following examples:


Example: _st_within
^^^^^^^^^^^^^^^^^^^
Fetch a list of geometry values which are within the given ``polygon`` value:

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

Example: _st_d_within
^^^^^^^^^^^^^^^^^^^^^
Fetch a list of ``geometry`` values which are 3 units from given ``point`` value:

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

Filter or check for null values (_is_null)
------------------------------------------

Checking for null values can be achieved using the ``_is_null`` operator.

Example: Filter null values in a field
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of articles that have a value in the ``published_on`` field:

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

Filter based on failure of some criteria (_not)
-----------------------------------------------

The ``_not`` operator can be used to fetch results for which some condition does not hold true. i.e. to invert the
filter set for a condition

Example: _not
^^^^^^^^^^^^^
Fetch all authors who don't have any published articles:

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

Using multiple filters in the same query (_and, _or)
----------------------------------------------------

You can group multiple parameters in the same ``where`` argument using the ``_and`` or the ``_or`` operators to filter
results based on more than one criteria.


.. note::
  You can use the ``_or`` and ``_and`` operators along with the ``_not`` operator to create arbitrarily complex boolean
  expressions involving multiple filtering criteria.

Example:  _and
^^^^^^^^^^^^^^
Fetch a list of articles published in a specific time-frame (for example: in year 2017):

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

Example:  _or
^^^^^^^^^^^^^
Fetch a list of articles rated more than 4 or published after "01/01/2018":

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

.. _nested_filter:

Filter nested objects
---------------------

The ``where`` argument can be used in **array relationships** as well to filter the nested objects.
**Object relationships** have only one nested object and hence they do not expose the ``where`` argument.

Example:
^^^^^^^^
Fetch all authors with only their 5 rated articles:

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


Filter based on nested objects' fields
--------------------------------------

You can use the fields of nested objects as well to filter your query results.

For example,

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

Example:
~~~~~~~~
Fetch all articles whose author's name starts with "A":

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


Fetch if **any** of the nested objects defined via an array relationship satisfy a condition
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Example:
~~~~~~~~
Fetch all authors which have written at least one article which is rated 1

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

Fetch if **all** of the nested objects defined via an array relationship satisfy a condition
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As by default a row is returned if any of the nested objects satisfy a condition, to achieve the above we need to frame
the ``where`` expression as ``{_not: {inverse-of-condition}}``. This reads as: fetch if not (any of the nested objects
satisfy the inverted condition) i.e. all of the nested objects satisfy the condition.

For example,

+---------------------------------------+-----------------------------------------------+
| condition                             | where expression                              |
+=======================================+===============================================+
| ``{object: {field: {_eq: "value"}}}`` | ``{_not: {object: {field: {_neq: "value"}}}`` |
+---------------------------------------+-----------------------------------------------+
| ``{object: {field: {_gt: "value"}}}`` | ``{_not: {object: {field: {_lte: "value"}}}`` |
+---------------------------------------+-----------------------------------------------+

Example:
~~~~~~~~
Fetch all authors which have all of their articles published i.e. have ``{is_published {_eq: true}``.

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


Fetch if **none** of the nested objects defined via an array relationship satisfy a condition
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As by default a row is returned if any of the nested objects satisfy a condition, to achieve the above we need to frame
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

Example:
~~~~~~~~
Fetch all authors which have none of their articles published i.e. have ``{is_published {_eq: true}``.

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

Filter based on existence of nested objects
-------------------------------------------

You can filter results based on if they have nested objects by checking if any nested objects exist. This can be
achieved by using the expression ``{}`` which evaluates to ``true`` if any object exists.


Example:
^^^^^^^^
Fetch all authors which have at least one article written by them:

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
            "id": 2,
            "name": "Beltran",
            "articles_aggregate": {
              "aggregate": {
                "count": 2
              }
            }
          },
          {
            "id": 3,
            "name": "Sidney",
            "articles_aggregate": {
              "aggregate": {
                "count": 3
              }
            }
          },
          {
            "id": 4,
            "name": "Anjela",
            "articles_aggregate": {
              "aggregate": {
                "count": 2
              }
            }
          }
        ]
      }
    }



