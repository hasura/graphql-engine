.. meta::
   :description: Run multiple mutations in a request in Hasura
   :keywords: hasura, docs, mutation, multiple mutations, request

Multiple mutations in a request
===============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Execution
---------

If multiple mutations are part of the same request, they are executed **sequentially** in a single **transaction**.
If any of the mutations fail, all the executed mutations will be rolled back.

Run multiple top level mutations in the same request
----------------------------------------------------

**Example:** Delete all ``article`` objects written by an author and update the ``author`` object:

.. graphiql::
  :view_only:
  :query:
    mutation reset_author {
      delete_article (
        where: {author_id: {_eq: 6}}
      ) {
        affected_rows
      }
      update_author (
        where: {id: {_eq: 6}}
        _set: {name: "Cory"}
      ) {
        returning {
          id
          name
          articles {
            id
            title
          }
        }
      }
    }
  :response:
    {
        "data": {
          "delete_article": {
            "affected_rows": 2
          },
          "update_author": {
            "returning": [
              {
                "id": 6,
                "name": "Cory",
                "articles": []
              }
            ]
          }
        }
      }

Insert an object and a nested object in the same mutation
---------------------------------------------------------

If you are trying to insert multiple objects which have relationships between them, you can use nested inserts.

**Example:** Insert a new ``article`` object with its ``author`` and return the inserted article object with its author
in the response:

.. graphiql::
  :view_only:
  :query:
    mutation insert_article {
      insert_article(
        objects: [
          {
            title: "Article 1",
            content: "Sample article content",
            author: {
              data: {
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
