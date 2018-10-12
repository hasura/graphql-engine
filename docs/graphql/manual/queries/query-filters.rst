Filter query results / search queries
=====================================

You can use the ``where`` argument in your queries to filter the results based on a field’s values (including in a
nested object’s fields). You can even use multiple filters in the same ``where`` clause using the ``_and`` or the
``_or`` operators.

For example, fetch data for an author whose name is "Sidney":

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

You can also use the nested ``articles`` object to filter rows from the ``author`` table. This query fetches a list of
authors who have articles with a rating greater than 4:

.. code-block:: graphql
   :emphasize-lines: 3

    query {
      author(
        where: {articles: {rating: {_gt: 4}}}
      ) {
        id
        name
      }
    }

``_eq`` and ``_gt`` are examples of comparison operators that can be used in the ``where`` argument to filter on
equality. Let’s take a look at different operators that can be used to filter results and the field types these
operators are compatible with.

Equality operators (_eq and _neq)
---------------------------------
The ``_eq`` (equal to) or the ``_neq`` (not equal to) operators are compatible with any Postgres type other than
json or jsonB (like Integer, Float, Double, Text, Boolean, Date/Time/Timestamp, etc.). The following are examples of
using the equality operators on different types.

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
``_lte`` (less than or equal to) operators are compatible with any Postgres type other than json or jsonB
(like Integer, Float, Double, Text, Boolean, Date/Time/Timestamp, etc.). The following are examples of using these
operators on different types:


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
They are compatible with any Postgres type other than json or jsonB (like Integer, Float, Double, Text, Boolean,
Date/Time/Timestamp, etc.). The following are examples of using these operators on different types:

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

Text search / filter or pattern matching operators
--------------------------------------------------
The ``_like``, ``_nlike``, ``_ilike``, ``_nilike``, ``_similar``, ``_nsimilar`` operators behave exactly like
their `SQL counterparts <https://www.postgresql.org/docs/10/static/functions-matching.html>`_  and are used for
pattern matching on string/Text fields.

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


Example: _similar
^^^^^^^^^^^^^^^^^
Fetch a list of authors whose names begin with A or C (``similar`` is case-sensitive):

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

Filter or check for null values
-------------------------------
Checking for null values is pretty straightforward using the ``_is_null`` operator.

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

Using multiple filters in the same query
----------------------------------------
You can group multiple parameters in the same ``where`` argument using the ``_and`` or the ``_or`` operators to filter
results based on more than one criteria.

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
