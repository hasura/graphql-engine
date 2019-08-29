Using directives in a query
==========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Directives make it possible to include or skip a field based on a boolean expression passed as a query variable.

@include(if: Boolean)
---------------------

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
------------------

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