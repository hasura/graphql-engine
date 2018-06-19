Queries
=======

.. contents:: :local:

Introduction
------------
HasuraDB auto-generates queries and mutations as part of the GraphQL schema from your Postgres schema model. It
generates a range of possible queries and operators that also work with relationships defined in your SQL schema.
All tracked tables in the public schema of hasuradb database can be queried and modified over the GraphQL endpoint.
If you have a table named “author” in your database, a query and a mutation each are added as nested fields under
the root level types, query_root and mutation_root respectively. For e.g. the auto-generated field for the ‘author’
table may look like this:

.. code-block:: none

    author (
      where: author_bool_exp
      limit: Int
      offset: Int
      order_by: [String]
    ): [author]


You can explore the entire schema and the available queries using the GraphiQL interface in the API-Console or using
the "Docs" link in the top, right corner of the embedded, interactive GraphiQL window in the docs. Let’s take a
look at the different queries you can run using the HasuraDB GraphQL endpoint. We’ll use examples based on a
typical Authors and Articles schema for reference.

Simple Object queries
---------------------
You can fetch a single node or multiple nodes of the same type using a simple object query. 

E.g. Fetch a list of authors:

.. graphiql::
   :query:
      query {
        author {
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
              "id": 4,
              "name": "Axel"
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
              "id": 13,
              "name": "Quintus"
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
              "id": 25,
              "name": "Niki"
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

Nested object queries using relationships
-----------------------------------------
You can use the 1:1 (object)  or 1:m (array) relationships defined in your schema (in the API-console) to make a
nested query i.e. fetch data for a type along with data from a nested or related type.

Example: Nested object query over a 1:many relationship
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of authors and a nested list of the each author’s articles:

.. graphiql::
   :query:
        query {
          author {
            id
            name
            articles {
              id
              title
            }
          }
        }
   :response:
        {
        "data": {
            "author": [
            {
                "id": 1,
                "name": "Chrissie",
                "articles": [
                {
                    "id": 98,
                    "title": "some title"
                },
                {
                    "id": 73,
                    "title": "some title"
                },
                {
                    "id": 87,
                    "title": "some title"
                }
                ]
            },
            {
                "id": 2,
                "name": "Aubrey",
                "articles": [
                {
                    "id": 51,
                    "title": "some title"
                },
                {
                    "id": 41,
                    "title": "some title"
                },
                {
                    "id": 19,
                    "title": "some title"
                }
                ]
            },
            {
                "id": 29,
                "name": "Carmella",
                "articles": [
                {
                    "id": 78,
                    "title": "some title"
                },
                {
                    "id": 64,
                    "title": "some title"
                }
                ]
            }
            ]
        }
        }


Example: Nested object query over a 1:1 relationship
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of articles and the name of each article’s author:

.. graphiql::
   :query:
        query {
          article {
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
                "id": 3,
                "title": "some title",
                "author": {
                "name": "Derril"
                }
            },
            {
                "id": 4,
                "title": "some title",
                "author": {
                "name": "Dreddy"
                }
            },
            {
                "id": 5,
                "title": "some title",
                "author": {
                "name": "Mallorie"
                }
            },
            {
                "id": 6,
                "title": "some title",
                "author": {
                "name": "Saunderson"
                }
            }
            ]
        }
        }

.. note::
    The name of the nested object is the same as the name of the `1:many` or `1:1` relationship configured in the
    API-Console

Query filters or search queries
-------------------------------
You can use the ``where`` argument in your queries to filter the results based on a fields’s values (including in a
nested object’s fields). You can even use multiple filters in the same ``where`` *clause* using the ``_and`` or the
``_or`` operators.

For e.g. Fetch data for an author whose name is “Mallorie”:

.. code-block:: JSON

    query {
        author(where: {name: {_eq: "Mallorie"}}) {
        id
        name
      }
    }

You can also use the ``_gt`` comparison operator with the nested ``articles`` object to filter rows from the
``author`` table. This query fetches a list of authors whose articles have ids that are greater than 10:

.. code-block:: none

    query {
      author(where: {articles: {id: {_gt: 10}}}) {
        id
        name
      }
    }

``_eq`` and ``_gt`` are examples of comparison operators that can be used in the ``where`` argument to filter on
equality. Let’s take a look at different operators that can be used to filter results and the field types these
operators are compatible with.

Equality operators (_eq and _neq)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The ``_eq`` (equal to) or the ``_neq`` (not equal to) operators are compatible with any Postgres type other than
json or jsonB (like Integer, Float, Double, Text, Boolean, Date/Time/Timestamp, etc.). The following are examples of
using the equality operators on different types.

Example: Integer (works with Double, Float, Numeric, etc.)
**********************************************************
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
***********************
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
****************
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
***********************************************
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
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The ``_gt`` (greater than), ``_lt`` (less than), ``_gte`` (greater than or equal to), ``_lte`` (less than or equal
to) operators are compatible with any Postgres type other than json or jsonB (like Integer, Float, Double, Text,
Boolean, Date/Time/Timestamp, etc.). The following are examples of using these operators on different types:


Example: Integer (works with Double, Float, etc.)
*************************************************
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
***********************
Fetch a list of authors whose names begin with S or any letter that follows S (*essentially, a filter based on a
dictionary sort*):

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
***********************************************
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
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The ``_in`` (in a list) and ``_nin`` (not in list) operators are used to comparing field values to a list of values.
They are compatible with any Postgres type other than json or jsonB (like Integer, Float, Double, Text, Boolean,
Date/Time/Timestamp, etc.). The following are examples of using these operators on different types:

Example: Integer (works with Double, Float, etc.)
*************************************************
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
***********************
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
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The ``_like``, ``_nlike``, ``_ilike``, ``_nilike``, ``_similar``, ``_nsimilar`` operators behave exactly like their
`SQL counterparts <https://www.postgresql.org/docs/10/static/functions-matching.html>`_  and are used for pattern
matching on string/Text fields.

Example: _like
**************
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
*****************
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
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Checking for null values is pretty straightforward using the `_eq` or `_neq` operators.

Example: Filter null values in a field
**************************************
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
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can group multiple parameters in the same ``where`` argument using the ``_and`` or the ``_or`` operators to
filter results based on more than one criteria.

Example:  _and
**************
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
*************
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

Aggregations in queries
-----------------------
GraphQL’s query language for a ``select`` query is designed to be simple yet powerful. There will still be queries
that you cannot express with the ``select`` query. For example, getting the number of likes for each article. To
express complex queries like aggregations (or custom joins etc.), use SQL, which is designed for this purpose. If
you can express your aggregation query in SQL, define a view with it and then use the newly created type in the
GraphQL query.

Let’s see an example of how to do that with our reference schema, assuming we also have a table ``article_like``
with a row for each unique like for an article (columns are id, article_id, date_liked, etc.). Our aim is to get a
total number of likes per article.

Create a view
^^^^^^^^^^^^^
A view that sums up the number of likes for each article is to be created, using the following SQL query:

.. code-block:: SQL

    CREATE VIEW article_like_count AS
    SELECT article_id, COUNT(author_id) AS like_count
    FROM article_like
    GROUP BY article_id;

Add a relationship
^^^^^^^^^^^^^^^^^^
Relationships are generally defined using foreign key constraints. However, you cannot define foreign key
constraints on/to views. So, in these cases, we can define a relationship without using a foreign keys as described
`here <https://docs.hasura.io/0.15/manual/data/relationships/create-relationships.html>_` (we create an object
relationship, ``total_likes``, by mapping ``article``:``id`` -> ``article_like_count``:``article_id``).

Query using the relationship
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Now that we have the relationship between the `article` table and the ``total_likes`` view has been set up, we can
query the aggregate data in ``total_likes`` as with any regular nested object.

Example: aggregate data
***********************
Fetch a list of articles along with the total number of likes received by each one:

.. graphiql::
   :query:
        query  {
            article {
                id
                rating
                total_likes{
                    like_count
                }
            }
        }
   :response:
        {
            "data": {
                "article": [
                    {
                        "id": 3,
                        "rating": 4,
                        "total_likes": {
                        "like_count": 2
                        }
                    },
                    {
                        "id": 4,
                        "rating": 4,
                        "total_likes": {
                        "like_count": 1
                        }
                    },
                    {
                        "id": 10,
                        "rating": 5,
                        "total_likes": {
                        "like_count": 2
                        }
                    }
                ]
            }
        }

This example can be easily extended to cover any use-case involving a SQL aggregate function that you may want to use.

Sort query results (order_by)
-----------------------------
Results from your query can be sorted by using the ``order_by`` argument. The argument can be used to sort based on
fields in nested objects too. The sort order (ascending vs. descending) is set by specifying the ``+`` or ``-`` in
front of the column name in the ``order_by`` argument. The ``order_by`` argument takes an array of parameters to
allow sorting by multiple columns.

Example - order_by in a simple object query
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of authors that is sorted by their names in a descending order:

.. graphiql::
   :query:
        query {
            author(order_by: ["-name"]) {
                id
                name
            }
        }
   :response:
        {
            "data": {
                "author": [
                    {
                        "id": 26,
                        "name": "Wenda"
                    },
                    {
                        "id": 21,
                        "name": "Sophey"
                    },
                    {
                        "id": 20,
                        "name": "Saunderson"
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
                        "id": 13,
                        "name": "Quintus"
                    }
                ]
            }
        }

Example - order_by in a nested object query
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of authors that is sorted by their names and a list of each of their articles that is sorted in the
reverse chronological order of their publication date:

.. graphiql::
   :query:
        query {
            author(order_by: ["-id"]) {
                id
                name
                articles (order_by: ["-published_on"])  {
                    id
                    published_on
                }
            }
        }
   :response:
        {
            "data": {
                "author": [
                    {
                        "id": 29,
                        "name": "Carmella",
                        "articles": [
                        {
                            "id": 78,
                            "published_on": null
                        },
                        {
                            "id": 64,
                            "published_on": null
                        }
                        ]
                    },
                    {
                        "id": 28,
                        "name": "Derril",
                        "articles": [
                        {
                            "id": 3,
                            "published_on": null
                        },
                        {
                            "id": 10,
                            "published_on": null
                        },
                        {
                            "id": 34,
                            "published_on": null
                        },
                        {
                            "id": 38,
                            "published_on": null
                        },
                        {
                            "id": 59,
                            "published_on": null
                        },
                        {
                            "id": 88,
                            "published_on": null
                        }
                        ]
                    },
                    {
                        "id": 27,
                        "name": "Ashby",
                        "articles": [
                        {
                            "id": 17,
                            "published_on": null
                        },
                        {
                            "id": 7,
                            "published_on": null
                        },
                        {
                            "id": 99,
                            "published_on": null
                        }
                        ]
                    }
                ]
            }
        }

Example - order_by multiple fields
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of articles that is sorted by the date they were published (descending) and then alphabetically based
on their title:

.. graphiql::
   :query:
        query {
            article(order_by: ["-published_on", "+title"]) {
                id
                title
                content
                published_on
            }
        }
   :response:
        {
            "data": {
                "article": [
                {
                    "id": 2,
                    "title": "a some title",
                    "content": "some content",
                    "published_on": "2018-06-14"
                },
                {
                    "id": 6,
                    "title": "some title",
                    "content": "some content",
                    "published_on": "2018-06-11"
                }
                ]
            }
        }

Paginate query results
----------------------
The operators ``limit`` and ``offset`` are used for pagination, etc. ``limit`` specifies the number of rows to
retain from the result set and ``offset`` determines which slice to retain from the results.

Example - query with limit
^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch the first 5 authors from a list of all authors: 

.. graphiql::
   :query:
        query {
            article(limit: 5) {
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
                        "id": 3,
                        "title": "some title",
                        "content": "some content"
                    },
                    {
                        "id": 4,
                        "title": "some title",
                        "content": "some content"
                    },
                    {
                        "id": 5,
                        "title": "some title",
                        "content": "some content"
                    },
                    {
                        "id": 6,
                        "title": "some title",
                        "content": "some content"
                    },
                    {
                        "id": 8,
                        "title": "some title",
                        "content": "some content"
                    }
                ]
            }
        }

Example - query with limit and offset
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch 5 authors from a list of all authors, starting with the 11th one:

.. graphiql::
   :query:
        query {
            article(limit: 5, offset:10) {
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
                        "id": 14,
                        "title": "some title",
                        "content": "some content"
                    },
                    {
                        "id": 78,
                        "title": "some title",
                        "content": "some content"
                    },
                    {
                        "id": 79,
                        "title": "some title",
                        "content": "some content"
                    },
                    {
                        "id": 80,
                        "title": "some title",
                        "content": "some content"
                    },
                    {
                        "id": 22,
                        "title": "some title",
                        "content": "some content"
                    }
                ]
            }
        }

Example - query with pagination in a nested object  
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of authors and a list of 5 of each of their most recently published articles:

.. graphiql::
   :query:
        query {
            author {
                id
                name
                articles (order_by: ["-published_on"], limit:5)  {
                    id
                    published_on
                }
            }
        }
   :response:
        {
            "data": {
                "author": [
                    {
                        "id": 1,
                        "name": "Chrissie",
                        "articles": [
                        {
                            "id": 73,
                            "published_on": null
                        },
                        {
                            "id": 87,
                            "published_on": null
                        },
                        {
                            "id": 98,
                            "published_on": null
                        }
                        ]
                    },
                    {
                        "id": 20,
                        "name": "Saunderson",
                        "articles": [
                        {
                            "id": 60,
                            "published_on": null
                        },
                        {
                            "id": 6,
                            "published_on": "2018-06-11"
                        }
                        ]
                    },
                    {
                        "id": 29,
                        "name": "Carmella",
                        "articles": [
                        {
                            "id": 78,
                            "published_on": null
                        },
                        {
                            "id": 64,
                            "published_on": null
                        }
                        ]
                    }
                ]
            }
        }

Use multiple arguments together
-------------------------------
Multiple arguments can be used together in the same query. For e.g. if you want to use the where argument to filter
the results and then use the order_by argument to sort them, you can use a query similar to the following one

Example: multiple arguments in the same query
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Fetch a list of authors and only their published articles that are sorted by the date of publication (descending):

.. graphiql::
   :query:
        query {
            author {
                id
                name
                articles (
                where: {is_published:{_eq:true}}
                order_by: ["-published_on"]
                )  {
                    id
                    title
                    published_on
                }
            }
        }
   :response:
        {
            "data": {
                "author": [
                    {
                        "id": 1,
                        "name": "Chrissie",
                        "articles": []
                    },
                    {
                        "id": 2,
                        "name": "Aubrey",
                        "articles": []
                    },
                    {
                        "id": 3,
                        "name": "Mallorie",
                        "articles": []
                    },
                    {
                        "id": 10,
                        "name": "Obie",
                        "articles": [
                        {
                            "id": 2,
                            "title": "a some title",
                            "published_on": "2018-06-14"
                        }
                        ]
                    }
                ]
            }
        }


Control Access
--------------
If you want to control access to sensitive fields in a table, use views to expose only the safe fields. Here’s how
you can do this. Our aim here is to mask access to the ``article`` table and only expose the ``id``, ``title`` and
``rating`` columns from this table.

Create a view
^^^^^^^^^^^^^
Create a view with data from only the required (or safe) columns:

.. code-block:: SQL

    CREATE VIEW article_safe AS
    SELECT id, title, rating 
    FROM article;

Modify permissions
^^^^^^^^^^^^^^^^^^
You will need to revoke permission (if already granted) from the source table and grant access to the newly created
view. So, in our example, we do the following:

#. Remove access permissions from the ``article`` table

#. Grant access permissions to the ``article_safe`` view

Query the view
^^^^^^^^^^^^^^
You can now query the newly created view like you would a regular table. 

Example: query a view with limited fields
*****************************************
The following query will accesses only the *safe* fields:

.. graphiql::
   :query:
        query {
            article_safe {
                id
                title
                rating
            }
        }
   :response:
        {
            "data": {
                "article_safe": [
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
                        "id": 5,
                        "title": "some title",
                        "rating": 3
                    },
                    {
                        "id": 6,
                        "title": "some title",
                        "rating": 2
                    }
                ]
            }
        }
