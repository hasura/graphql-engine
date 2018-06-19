Query filters or search queries
===============================

You can use the ``where`` argument in your queries to filter the results based on a fields’s values (including in a nested object’s fields). You can even use multiple filters in the same ``where`` *clause* using the ``_and`` or the ``_or`` operators.

For e.g. Fetch data for an author whose name is “Mallorie”:

.. code-block:: JSON

    query {
        author(where: {name: {_eq: "Mallorie"}}) {
        id
        name
      }
    }

You can also use the ``_gt`` comparison operator with the nested ``articles`` object to filter rows from the ``author`` table. This query fetches a list of authors whose articles have ids that are greater than 10:

.. code-block:: none

    query {
      author(where: {articles: {id: {_gt: 10}}}) {
        id
        name
      }
    }

``_eq`` and ``_gt`` are examples of comparison operators that can be used in the ``where`` argument to filter on equality. Let’s take a look at different operators that can be used to filter results and the field types these operators are compatible with.

Equality operators (_eq and _neq)
---------------------------------
The ``_eq`` (equal to) or the ``_neq`` (not equal to) operators are compatible with any Postgres type other than json or jsonB (like Integer, Float, Double, Text, Boolean, Date/Time/Timestamp, etc.). The following are examples of using the equality operators on different types.

Example: Integer (works with Double, Float, Numeric, etc.)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetches data about exactly one author whose ``id`` (*an integer field*) is equal to 3:

.. graphiql::
   :query:
        query {
            author(where: {id: {_eq: 3}}) {
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
                    "name": "Mallorie"
                }
            ]
        }
    }

Example: String or Text
^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of authors who have written articles with the title “GraphQL examples” (``title`` is a TEXT field):

.. graphiql::
   :query:
        query {
            author(where: {articles: {title: {_eq: "GraphQL examples"}}}) {
                id
                name
            }
        }
   :response:
        {
            "data": {
                "author": []
            }
        }

Example: Boolean
^^^^^^^^^^^^^^^^
Fetch a list of articles that have been published (``is_published`` is a boolean field):

.. graphiql::
   :query:
        query {
            article (where: {is_published: {_eq: true}}) {
                id
                title
                content
            }
        }
   :response:
        {
            "data": {
                "article": [
                {
                    "id": 6,
                    "title": "some title",
                    "content": "some content"
                },
                {
                    "id": 2,
                    "title": "a some title",
                    "content": "some content"
                }
                ]
            }
        }


Example: Date (works with Time, Timezone, etc.)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of articles that were published on a certain date(``published_on`` is Date field):

.. graphiql::
   :query:
        query {
            article (where: {published_on: {_eq: "2018-06-14"}}) {
                id
                title
                content
            }
        }
   :response:
        {
            "data": {
                "article": [
                    {
                        "id": 2,
                        "title": "a some title",
                        "content": "some content"
                    }
                ]
            }
        }

Greater than or less than operators (_gt, _lt, _gte, _lte)
----------------------------------------------------------
The ``_gt`` (greater than), ``_lt`` (less than), ``_gte`` (greater than or equal to), ``_lte`` (less than or equal to) operators are compatible with any Postgres type other than json or jsonB (like Integer, Float, Double, Text, Boolean, Date/Time/Timestamp, etc.). The following are examples of using these operators on different types:


Example: Integer (works with Double, Float, etc.)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetches a list of articles rated 3.5 or more:

.. graphiql::
   :query:
        query {
            article (where: {rating: {_gt: 3.5}}) {
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
                    "title": "some title",
                    "rating": 4
                },
                {
                    "id": 4,
                    "title": "some title",
                    "rating": 4
                },
                {
                    "id": 8,
                    "title": "some title",
                    "rating": 4
                },
                {
                    "id": 10,
                    "title": "some title",
                    "rating": 5
                }
                ]
            }
        }

Example: String or Text
^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of authors whose names begin with S or any letter that follows S (*essentially, a filter based on a dictionary sort*):

.. graphiql::
   :query:
        query {
            author(where: {name: {_gt: "S"}}) {
                id
                name
            }
        }
   :response:
        {
            "data": {
                "author": [
                    {
                        "id": 20,
                        "name": "Saunderson"
                    },
                    {
                        "id": 21,
                        "name": "Sophey"
                    },
                    {
                        "id": 26,
                        "name": "Wenda"
                    }
                ]
            }
        }

Example: Date (works with Time, Timezone, etc.)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of articles that were published on or after a certain date:

.. graphiql::
   :query:
        query {
            article (where: {published_on: {_gte: "2018-06-14"}}) {
                id
                title
                content
            }
        }
   :response:
        {
            "data": {
                "article": [
                    {
                        "id": 2,
                        "title": "a some title",
                        "content": "some content"
                    }
                ]
            }
        }

List based search operators (_in, _nin)
---------------------------------------
The ``_in`` (in a list) and ``_nin`` (not in list) operators are used to comparing field values to a list of values. They are compatible with any Postgres type other than json or jsonB (like Integer, Float, Double, Text, Boolean, Date/Time/Timestamp, etc.). The following are examples of using these operators on different types:

Example: Integer (works with Double, Float, etc.)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetches a list of articles rated 1, 3 or 5:

.. graphiql::
   :query:
        query {
            article (where: {rating: {_in: [1,3,5]}}) {
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
                        "id": 5,
                        "title": "some title",
                        "rating": 3
                    },
                    {
                        "id": 9,
                        "title": "some title",
                        "rating": 1
                    },
                    {
                        "id": 10,
                        "title": "some title",
                        "rating": 5
                    }
                ]
            }
        }

Example: String or Text
^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of those authors whose names are NOT part of a list: 

.. graphiql::
   :query:
        query {
            author (where: {name: {_nin: ["Axel","Quintus","Niki"]}}) {
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
                        "name": "Chrissie"
                    },
                    {
                        "id": 2,
                        "name": "Aubrey"
                    },
                    {
                        "id": 3,
                        "name": "Mallorie"
                    },
                    {
                        "id": 5,
                        "name": "Dreddy"
                    },
                    {
                        "id": 6,
                        "name": "Bernhard"
                    },
                    {
                        "id": 7,
                        "name": "Eleonore"
                    },
                    {
                        "id": 8,
                        "name": "Khalil"
                    },
                    {
                        "id": 9,
                        "name": "Dorris"
                    },
                    {
                        "id": 10,
                        "name": "Obie"
                    },
                    {
                        "id": 11,
                        "name": "Rubi"
                    },
                    {
                        "id": 12,
                        "name": "Ricoriki"
                    },
                    {
                        "id": 14,
                        "name": "Chrotoem"
                    },
                    {
                        "id": 15,
                        "name": "Ericka"
                    },
                    {
                        "id": 16,
                        "name": "Catherin"
                    },
                    {
                        "id": 17,
                        "name": "Lin"
                    },
                    {
                        "id": 18,
                        "name": "Marten"
                    },
                    {
                        "id": 19,
                        "name": "Lida"
                    },
                    {
                        "id": 20,
                        "name": "Saunderson"
                    },
                    {
                        "id": 21,
                        "name": "Sophey"
                    },
                    {
                        "id": 22,
                        "name": "Conny"
                    },
                    {
                        "id": 23,
                        "name": "Edithe"
                    },
                    {
                        "id": 24,
                        "name": "Jeri"
                    },
                    {
                        "id": 26,
                        "name": "Wenda"
                    },
                    {
                        "id": 27,
                        "name": "Ashby"
                    },
                    {
                        "id": 28,
                        "name": "Derril"
                    },
                    {
                        "id": 29,
                        "name": "Carmella"
                    }
                ]
            }
        }

Text search / filter or pattern matching operators
--------------------------------------------------
The ``_like``, ``_nlike``, ``_ilike``, ``_nilike``, ``_similar``, ``_nsimilar`` operators behave exactly like their `SQL counterparts <https://www.postgresql.org/docs/10/static/functions-matching.html>`_  and are used for pattern matching on string/Text fields.

Example: _like
^^^^^^^^^^^^^^
Fetch a list of authors with articles whose titles begin with “The”: 

.. graphiql::
   :query:
        query {
            author (where: { articles: {title: {_like: "The%"}}})
            {
                id
                name
            }
        }
   :response:
        {
            "data": {
                "author": []
            }
        }

Example: _similar
^^^^^^^^^^^^^^^^^
Fetch a list of authors whose names begin with A or C (``similar`` is case-sensitive):

.. graphiql::
   :query:
        query {
            author(where: {name: {_similar: "(A|C)%"}}) {
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
                        "name": "Chrissie"
                    },
                    {
                        "id": 2,
                        "name": "Aubrey"
                    },
                    {
                        "id": 4,
                        "name": "Axel"
                    },
                    {
                        "id": 14,
                        "name": "Chrotoem"
                    }
                ]
            }
        }

Filter or check for null values
-------------------------------
Checking for null values is pretty straightforward using the `_eq` or `_neq` operators.

Example: Filter null values in a field
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of articles that have some boolean value in the `is_published` field:

.. graphiql::
   :query:
        query {
            article (where: {is_published: {_neq: null}}) {
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
                        "id": 6,
                        "title": "some title",
                        "is_published": true
                    },
                    {
                        "id": 2,
                        "title": "a some title",
                        "is_published": true
                    },
                    {
                        "id": 1,
                        "title": "b-something",
                        "is_published": false
                    }
                ]
            }
        }

Using multiple filters in the same query
----------------------------------------
You can group multiple parameters in the same ``where`` argument using the ``_and`` or the ``_or`` operators to filter results based on more than one criteria. 

Example:  _and
^^^^^^^^^^^^^^
Fetch a list of articles published in a specific time-frame:

.. graphiql::
   :query:
        query {
            article ( where: {
                _and: [
                    { published_on: {_gte: "2016-06-13"}}, 
                    { published_on: {_lte: "2018-06-17"}}
                ]
            })
            {
                id
                title
                author_id
            }
        }
   :response:
        {
            "data": {
                "article": [
                    {
                        "id": 6,
                        "title": "some title",
                        "author_id": 20
                    },
                    {
                        "id": 2,
                        "title": "a some title",
                        "author_id": 10
                    }
                ]
            }
        }

Example:  _or
^^^^^^^^^^^^^
Fetch a list of articles rated more than 4 or published after a certain date:

.. graphiql::
   :query:
        query {
            article (where: {
                _or: [
                    {rating: {_gt: 4}}, 
                    {published_on: {_gt: "2016-06-14"}}
                ]}) {
                id
                title
                author_id
            }
        }
   :response:
        {
            "data": {
                "article": [
                    {
                        "id": 6,
                        "rating": 2,
                        "published_on": "2018-06-11"
                    },
                    {
                        "id": 10,
                        "rating": 5,
                        "published_on": null
                    },
                    {
                        "id": 79,
                        "rating": 5,
                        "published_on": null
                    }
                ]
            }
        }