.. meta::
   :description: Delete an object from the database using a mutation
   :keywords: hasura, docs, mutation, delete

.. _delete:

Delete mutation
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Auto-generated delete mutation schema
-------------------------------------

**For example**, the auto-generated schema for the delete mutation field for a table ``article`` looks like this:

.. code-block:: graphql

  delete_article (
    where: article_bool_exp!
  ): article_mutation_response

  # response of any mutation on the table "article"
  type article_mutation_response {
    # number of affected rows by the mutation
    affected_rows: Int!
    # data of the affected rows by the mutation
    returning: [article!]!
  }

  # single object delete (supported from v1.2.0)
  delete_article_by_pk (
    # all primary key columns args
    id: Int
  ): article

As you can see from the schema:

- The ``where`` argument is compulsory to filter rows to be deleted. See :ref:`Filter queries <filter_queries>`
  for filtering options. Objects can be deleted based on filters on their own fields or those in their nested objects.
  The ``{}`` expression can be used to delete all rows.
- You can return the number of affected rows and the affected objects (with nested objects) in the response.

See the :ref:`delete mutation API reference <delete_syntax>` for the full specifications.

.. note::

  If a table is not in the ``public`` Postgres schema, the delete mutation field will be of the format
  ``delete_<schema_name>_<table_name>``.

Delete an object by its primary key
-----------------------------------

You can delete a single object in a table using the primary key.
The output type is the nullable table object. The mutation returns the deleted
row object or ``null`` if the row does not exist.


**Example:** Delete an article where ``id`` is ``1``:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    .. graphiql::
      :view_only:
      :query:
        mutation delete_an_object {
          delete_article_by_pk (
            id: 1
          ) {
            id
            title
            user_id
          }
        }
      :response:
        {
          "data": {
            "delete_article_by_pk": {
              "id": 1,
              "title": "Article 1",
              "user_id": 1
            }
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "mutation delete_an_object { delete_article_by_pk (id: 1) { id title user_id }}"
      }

**Example:** Delete a non-existent article:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    .. graphiql::
      :view_only:
      :query:
        mutation delete_an_object {
          delete_article_by_pk (
            id: 100
          ) {
            id
            title
            user_id
          }
        }
      :response:
        {
          "data": {
            "delete_article_by_pk": null
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "mutation delete_an_object { delete_article_by_pk (id: 100) { id title user_id }}"
      }

.. admonition:: Supported from

   The ``delete_<table>_by_pk`` mutation is supported in versions ``v1.2.0``
   and above.


Delete objects based on an their fields
---------------------------------------
**Example:** Delete all articles rated less than 3:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

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

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "mutation delete_low_rated_articles { delete_article(where: {rating: {_lt: 3}}) { affected_rows }}"
      }


Delete objects based on nested objects' fields
----------------------------------------------
**Example:** Delete all articles written by a particular author:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

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

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "mutation delete_authors_articles { delete_article(where: {author: {name: {_eq: \"Corny\"}}}) { affected_rows }}"
      }

Delete all objects
------------------

You can delete all objects in a table using the ``{}`` expression as the ``where`` argument. ``{}`` basically
evaluates to ``true`` for all objects.

**Example:** Delete all articles:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    .. graphiql::
      :view_only:
      :query:
        mutation delete_all_articles {
          delete_article (
            where: {}
          ) {
            affected_rows
          }
        }
      :response:
        {
          "data": {
            "delete_article": {
              "affected_rows": 20
            }
          }
        }

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "mutation delete_all_articles { delete_article (where: {}) { affected_rows }}"
      }
