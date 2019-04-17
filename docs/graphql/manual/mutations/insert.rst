Insert mutation
===============

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Auto-generated insert mutation schema
-------------------------------------

**For example**, the auto-generated schema for the insert mutation field for a table ``article`` looks like this:

.. code-block:: graphql

  insert_article (
    objects: [article_insert_input!]!
    on_conflict: article_on_conflict
  ): article_mutation_response

  # response of any mutation on the table "article"
  type article_mutation_response {
    # number of affected rows by the mutation
    affected_rows: Int!
    #data of the affected rows by the mutation
    returning: [article!]!
  }

As you can see from the schema:

- ``objects`` argument is necessary and you can pass multiple ``objects`` to the mutation.
- You can pass an ``on_conflict`` argument to convert the mutation to an :doc:`upsert mutation <upsert>`
- You can return the number of affected rows and the affected objects (with nested objects) in the response.

See the :ref:`insert mutation API reference <insert_upsert_syntax>` for the full specifications

.. note::

  If a table is not in the ``public`` Postgres schema, the insert mutation field will be of the format
  ``insert_<schema_name>_<table_name>``.

Insert a single object
----------------------
**Example:** Insert a new ``article`` object and return the inserted article object in the response

.. graphiql::
  :view_only:
  :query:
    mutation insert_article {
      insert_article(
        objects: [
          {
            id: 21,
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

Using variables:

.. graphiql::
  :view_only:
  :query:
    mutation insert_article($objects: [article_insert_input!]! ) {
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
  :variables:
    {
      "objects": [
        {
          "id": 21,
          "title": "Article 1",
          "content": "Sample article content",
          "author_id": 3
        }
      ]
    }

Insert multiple objects of the same type in the same mutation
-------------------------------------------------------------
**Example:** Insert 2 new ``article`` objects and return both the article objects in the response

.. graphiql::
  :view_only:
  :query:
    mutation insert_article {
      insert_article(
        objects: [
          {
            id: 22,
            title: "Article 2",
            content: "Sample article content",
            author_id: 4
          },
          {
            id: 23,
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

Insert an object and get a nested object in response
----------------------------------------------------
**Example:** Insert a new ``article`` object and return the inserted article object with its author in the response

.. graphiql::
  :view_only:
  :query:
    mutation insert_article {
      insert_article(
        objects: [
          {
            id: 21,
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


Insert an object and its nested object in the same mutation
-----------------------------------------------------------
**Example:** Insert a new ``article`` object with its ``author`` and return the inserted article object with its author
in the response

.. graphiql::
  :view_only:
  :query:
    mutation insert_article {
      insert_article(
        objects: [
          {
            id: 21,
            title: "Article 1",
            content: "Sample article content",
            author: {
              data: {
                id: 11,
                name: "Cory"
              }
            }
          }
        ]
      ) {
        affected_rows
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
          "affected_rows": 2,
          "returning": [
            {
              "id": 21,
              "title": "Article 1",
              "author": {
                "id": 11,
                "name": "Cory"
              }
            }
          ]
        }
      }
    }

Insert an object with a JSONB column
------------------------------------
**Example:** Insert a new ``author`` object with a JSONB ``address`` column

.. graphiql::
  :view_only:
  :query:
    mutation insert_author($address: jsonb) {
      insert_author (
        objects: [
          {
            id: 1,
            name: "Ash",
            address: $address
          }
        ]
      ) {
        affected_rows
        returning {
          id
          name
          address
        }
      }
    }
  :response:
    {
      "data": {
        "insert_author": {
          "affected_rows": 1,
          "returning": [
            {
              "id": 1,
              "name": "Ash",
              "address": {
                "city": "Bengaluru",
                "phone": "9090909090",
                "state": "Karnataka",
                "pincode": 560095,
                "street_address": "161, 19th Main Road, Koramangala 6th Block"
              }
            }
          ]
        }
      }
    }
  :variables:
    {
      "address": {
        "street_address": "161, 19th Main Road, Koramangala 6th Block",
        "city": "Bengaluru",
        "phone": "9090909090",
        "state": "Karnataka",
        "pincode": 560095
      }
    }

Set a field to its default value during insert
----------------------------------------------

To set a field to its ``default`` value, just omit it from the input object, irrespective of the
:doc:`default value configuration <../schema/default-values/index>` i.e. via Postgres defaults or using column presets.

**Example:** if default value of ``id`` is set to auto-incrementing integer, no need to pass ``id`` field in input object

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

**Example:** if ``age`` is a nullable field, either don't pass ``age`` field in input object or pass it as ``null``
to set it to ``null``

.. graphiql::
  :view_only:
  :query:
    mutation insert_author_with_null_age {
      insert_author(
        objects: [
          {
            name: "Jeff",
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
