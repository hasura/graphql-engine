.. meta::
   :description: Use variables, aliases, fragments and directives in Hasura queries
   :keywords: hasura, docs, query, variable, alias, fragment, directive

.. _variables_aliases_fragments_directives:

Using variables / aliases / fragments / directives in queries
=============================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Using variables
---------------

In order to make a query re-usable, it can be made dynamic by using variables.

**Example:** Fetch an author by their ``author_id``:

.. graphiql::
  :view_only:
  :query:
    query getArticles($author_id: Int!) {
      articles(
        where: { author_id: { _eq: $author_id } }
      ) {
        id
        title
      }
    }
  :response:
    {
      "data": {
        "articles": [
          {
            "id": 15,
            "title": "How to climb Mount Everest"
          },
          {
            "id": 6,
            "title": "How to be successful on broadway"
          }
        ]
      }
    }
  :variables:
    {
      "author_id": 1
    }

Variables and performance
^^^^^^^^^^^^^^^^^^^^^^^^^

To optimise for performance, use non-nullable variables by adding a ``!`` at the end of the variable type (like in the example above).
Refer to :ref:`this page<query_performance>` to learn more about Hasura's query plan caching and about optimizing with variables.

Using aliases
-------------

Aliases can be used to return objects with a different name than their field name. This is especially useful while
fetching the same type of objects with different arguments in the same query.

**Example:** First, fetch all articles. Second, fetch the two top-rated articles. Third, fetch the worst-rated article:

.. graphiql::
  :view_only:
  :query:
    query getArticles {
      articles {
        title
        rating
      }
      topTwoArticles: articles(
        order_by: {rating: desc},
        limit: 2
      ) {
        title
        rating
      }
      worstArticle: articles(
        order_by: {rating: asc},
        limit: 1
      ) {
        title
        rating
      }
    }
  :response:
    {
      "data": {
        "articles": [
          {
            "title": "How to climb Mount Everest",
            "rating": 4
          },
          {
            "title": "How to be successful on broadway",
            "rating": 20
          },
          {
            "title": "How to make fajitas",
            "rating": 6
          }
        ],
        "topTwoArticles": [
          {
            "title": "How to be successful on broadway",
            "rating": 20
          },
          {
            "title": "How to make fajitas",
            "rating": 6
          }
        ],
        "worstArticle": [
          {
            "title": "How to climb Mount Everest",
            "rating": 4
          }
        ]
      }
    }

Using fragments
---------------

Sometimes, queries can get long and confusing. A fragment is a set of fields with any chosen name. This fragment
can then be used to represent the defined set.

**Example:** Creating a fragment for a set of ``article`` fields (``id`` and ``title``) and using it in a query:

.. graphiql::
  :view_only:
  :query:
    fragment articleFields on articles {
      id
      title
    }
    query getArticles {
      articles {
        ...articleFields
      }
      topTwoArticles: articles(
        order_by: {rating: desc},
        limit: 2
      ) {
        ...articleFields
      }
    }
  :response:
    {
      "data": {
        "articles": [
          {
            "id": 3,
            "title": "How to make fajitas"
          },
          {
            "id": 15,
            "title": "How to climb Mount Everest"
          },
          {
            "id": 6,
            "title": "How to be successful on broadway"
          }
        ],
        "topTwoArticles": [
          {
            "id": 6,
            "title": "How to be successful on broadway"
          },
          {
            "id": 3,
            "title": "How to make fajitas"
          }
        ]
      }
    }

Using directives
----------------

Directives make it possible to include or skip a field based on a boolean expression passed as a query
variable.

@include(if: Boolean)
^^^^^^^^^^^^^^^^^^^^^

With ``@include(if: Boolean)``, it is possible to include a field in the query result based on a Boolean expression.

**Example:** The query result includes the field ``publisher``, as ``$with_publisher`` is set to ``true``:

.. graphiql::
  :view_only:
  :query:
    query getArticles($with_publisher: Boolean!) {
      articles {
        title
        publisher @include(if: $with_publisher)
      }
    }
  :response:
    {
      "data": {
        "articles": [
          {
            "title": "How to climb Mount Everest",
            "publisher": "Mountain World"
          },
          {
            "title": "How to be successful on broadway",
            "publisher": "Broadway World"
          },
          {
            "title": "How to make fajitas",
            "publisher": "Fajita World"
          }
        ]
      }
    }
  :variables:
    {
      "with_publisher": true
    }

**Example:** The query result doesn't include the field ``publisher``, as ``$with_publisher`` is set to ``false``:

.. graphiql::
  :view_only:
  :query:
    query getArticles($with_publisher: Boolean!) {
      articles {
        title
        publisher @include(if: $with_publisher)
      }
    }
  :response:
    {
      "data": {
        "articles": [
          {
            "title": "How to climb Mount Everest"
          },
          {
            "title": "How to be successful on broadway"
          },
          {
            "title": "How to make fajitas"
          }
        ]
      }
    }
  :variables:
    {
      "with_publisher": false
    }

@skip(if: Boolean)
^^^^^^^^^^^^^^^^^^

With ``@skip(if: Boolean)``, it is possible to exclude (skip) a field in the query result based on a Boolean expression.

**Example:** The query result doesn't include the field ``publisher``, as ``$with_publisher`` is set to ``true``:

.. graphiql::
  :view_only:
  :query:
    query getArticles($with_publisher: Boolean!) {
      articles {
        title
        publisher @skip(if: $with_publisher)
      }
    }
  :response:
    {
      "data": {
        "articles": [
          {
            "title": "How to climb Mount Everest"
          },
          {
            "title": "How to be successful on broadway"
          },
          {
            "title": "How to make fajitas"
          }
        ]
      }
    }
  :variables:
    {
      "with_publisher": true
    }

**Example:** The query result includes the field ``publisher``, as ``$with_publisher`` is set to ``false``:

.. graphiql::
  :view_only:
  :query:
    query getArticles($with_publisher: Boolean!) {
      articles {
        title
        publisher @skip(if: $with_publisher)
      }
    }
  :response:
    {
      "data": {
        "articles": [
          {
            "title": "How to climb Mount Everest",
            "publisher": "Mountain World"
          },
          {
            "title": "How to be successful on broadway",
            "publisher": "Broadway World"
          },
          {
            "title": "How to make fajitas",
            "publisher": "Fajita World"
          }
        ]
      }
    }
  :variables:
    {
      "with_publisher": false
    }
