.. meta::
   :description: Insert an object into MS SQL Server using a mutation
   :keywords: hasura, docs, ms sql server, mutation, insert

.. _ms_sql_server_insert:

MS SQL Server: Insert mutation
==============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Auto-generated insert mutation schema
-------------------------------------

**For example**, the auto-generated schema for the insert mutation field for a table ``article`` looks like the following:

.. code-block:: graphql

  insert_article (
    objects: [article_insert_input!]!
    if_matched: article_if_matched
  ): article_mutation_response

  # response of any mutation on the table "article"
  type article_mutation_response {
    # number of affected rows by the mutation
    affected_rows: Int!
    # data of the affected rows by the mutation
    returning: [article!]!
  }

  # single object insert
  insert_article_one (
    object: article_insert_input!
    if_matched: article_if_matched
  ): article

As you can see from the schema:

- ``objects`` argument is mandatory and you can pass multiple ``objects`` to the mutation.
- You can pass an ``if_matched`` argument to convert the mutation to an :ref:`upsert mutation <ms_sql_server_upsert>`.
- You can return the number of affected rows and the affected objects (with nested objects) in the response.
- You can use the single object insert to get the inserted object directly as the mutation response.

..
  See the :ref:`insert mutation API reference <insert_upsert_syntax>` for the full specifications.

.. note::

  If a table is not in the ``dbo`` MS SQL Server schema, the insert mutation field will be of the format
  ``insert_<schema_name>_<table_name>``.

Insert a single object
----------------------

**Example:** Insert a new ``article`` object and return the inserted article object in the response:

.. graphiql::
  :view_only:
  :query:
    mutation insert_single_article {
      insert_article_one(
        object: {
          title: "Article 1",
          content: "Sample article content",
          author_id: 3
        }
      ) {
        id
        title
      }
    }
  :response:
    {
      "data": {
        "insert_article_one": {
          "id": 21,
          "title": "Article 1"
        }
      }
    }

Using variables:

.. graphiql::
  :view_only:
  :query:
    mutation insert_single_article($object: article_insert_input! ) {
      insert_article_one(object: $object) {
        id
        title
      }
    }
  :response:
    {
      "data": {
        "insert_article_one": {
          "id": 21,
          "title": "Article 1"
        }
      }
    }
  :variables:
    {
      "object": {
        "title": "Article 1",
        "content": "Sample article content",
        "author_id": 3
      }
    }

.. note::

   The ``insert_<object>_one`` mutation will **only** be available if you have select permissions on the table, as it returns the inserted row.

Insert multiple objects of the same type in the same mutation
-------------------------------------------------------------
**Example:** Insert 2 new ``article`` objects and return both the article objects in the response:

.. graphiql::
  :view_only:
  :query:
    mutation insert_multiple_articles {
      insert_article(
        objects: [
          {
            title: "Article 2",
            content: "Sample article content",
            author_id: 4
          },
          {
            title: "Article 3",
            content: "Sample article content",
            author_id: 5
          }
        ]
      ) {
        returning {
          id
          title
        }
      }
    }
  :response:
    {
      "data": {
        "insert_article": {
          "affected_rows": 2,
          "returning": [
            {
              "id": 22,
              "title": "Article 2"
            },
            {
              "id": 23,
              "title": "Article 3"
            }
          ]
        }
      }
    }

Using variables:

.. graphiql::
  :view_only:
  :query:
    mutation insert_multiple_articles($objects: [article_insert_input!]! ) {
      insert_article(objects: $objects) {
        returning {
          id
          title
        }
      }
    }
  :response:
    {
        "data": {
          "insert_article": {
            "affected_rows": 2,
            "returning": [
              {
                "id": 22,
                "title": "Article 2"
              },
              {
                "id": 23,
                "title": "Article 3"
              }
            ]
          }
        }
      }
  :variables:
    {
      "objects": [
        {
          "title": "Article 2",
          "content": "Sample article content",
          "author_id": 4
        },
        {
          "title": "Article 3",
          "content": "Sample article content",
          "author_id": 5
        }
      ]
    }


Insert an object and get a nested object in response
----------------------------------------------------

**Example:** Insert a new ``article`` object and return the inserted article object with its author in the response:

.. graphiql::
  :view_only:
  :query:
    mutation insert_article {
      insert_article(
        objects: [
          {
            title: "Article 1",
            content: "Sample article content",
            author_id: 3
          }
        ]
      ) {
        returning {
          id
          title
          author {
            id
            name
          }
        }
      }
    }
  :response:
    {
      "data": {
        "insert_article": {
          "affected_rows": 1,
          "returning": [
            {
              "id": 21,
              "title": "Article 1",
              "author": {
                "id": 3,
                "name": "Sidney"
              }
            }
          ]
        }
      }
    }


Set a field to its default value during insert
----------------------------------------------

To set a field to its ``default`` value, just omit it from the input object, irrespective of the
default value configuration i.e. via MS SQL Server defaults or using column presets.

**Example:** If the default value of ``id`` is set to auto-incrementing integer, there's no need to pass the ``id`` field to the input object:

.. graphiql::
  :view_only:
  :query:
    mutation insert_article_with_def_id {
      insert_article(
        objects: [
          {
            title: "Article 1",
            content: "Sample article content",
            author_id: 3
          }
        ]
      ) {
        returning {
          id
          title
        }
      }
    }
  :response:
    {
      "data": {
        "insert_article": {
          "affected_rows": 1,
          "returning": [
            {
              "id": 21,
              "title": "Article 1"
            }
          ]
        }
      }
    }

Set a field to NULL during insert
---------------------------------

If a field is ``nullable`` in the database, to set its value to ``null``, either pass its value as ``null`` or
just omit it from the input object.

**Example:** If ``age`` is a nullable field, to set it to ``null``, either don't pass the age field to the input object
or pass it as ``null``:

.. graphiql::
  :view_only:
  :query:
    mutation insert_author_with_null_age {
      insert_author(
        objects: [
          {
            name: "Jeff"
          }
        ]
      ) {
        returning {
          id
          name
          age
        }
      }
    }
  :response:
    {
      "data": {
        "insert_author": {
          "returning": [
            {
                "id": 11,
                "name": "Jeff",
                "age": null
            }
          ]
        }
      }
    }

OR

.. graphiql::
  :view_only:
  :query:
    mutation insert_author_with_null_age {
      insert_author(
        objects: [
          {
            name: "Jeff",
            age: null
          }
        ]
      ) {
        returning {
          id
          name
          age
        }
      }
    }
  :response:
    {
      "data": {
        "insert_author": {
          "returning": [
            {
                "id": 11,
                "name": "Jeff",
                "age": null
            }
          ]
        }
      }
    }
