Insert mutation
===============

Insert single object
--------------------
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

OR

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

with variables:

.. code-block:: json

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

Insert nested object and get nested object in response
------------------------------------------------------
**Example:** Insert a new ``article`` object with its ``author`` and return the inserted article object with its author in the response


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
                id: 3,
                name: "Sidney"
              }
            }
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
          "affected_rows": 2,
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


Insert object and get nested object in response
-----------------------------------------------
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


Set field to its default value during insert
--------------------------------------------

To set a field to its ``default`` value, just omit it from the input object.

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

Set field to null during insert
-------------------------------

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
