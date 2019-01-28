Delete mutation
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Auto-generated delete mutation schema
-------------------------------------

Here’s the schema for the delete mutation field for a table ``article``:

.. code-block:: graphql

  delete_article (
    where: article_bool_exp!
  ): article_mutation_response

  # response of any mutation on the table "article"
  type article_mutation_response {
    # number of affected rows by the mutation
    affected_rows: Int!
    #data of the affected rows by the mutation
    returning: [article!]!
  }

As you can see from the schema:

- ``where`` argument is compulsory to filter rows to be deleted. See :doc:`Filter queries <../queries/query-filters>`
  for filtering options. Objects can be deleted based on filters on their own fields or those in their nested objects.
- You can return the number of affected rows and the affected objects (with nested objects) in the response.

.. note::

  If a table is not in the ``public`` Postgres schema, the delete mutation field will be of the format
  ``delete_<schema_name>_<table_name>``.

Delete based on an object's fields
----------------------------------
**Example:** Delete all articles rated less than 3:

.. graphiql::
  :view_only:
  :query:
    mutation delete_low_rated_articles {
      delete_article(
        where: {rating: {_lt: 3}}
      ) {
        affected_rows
      }
    }
  :response:
    {
      "data": {
        "delete_low_rated_articles": {
          "affected_rows": 8
        }
      }
    }


Delete based on a nested object's fields
----------------------------------------
**Example:** Delete all articles written by a particular author:

.. graphiql::
  :view_only:
  :query:
    mutation delete_authors_articles {
      delete_article(
        where: {author: {name: {_eq: "Corny"}}}
      ) {
        affected_rows
      }
    }
  :response:
    {
      "data": {
        "delete_authors_articles": {
          "affected_rows": 2
        }
      }
    }