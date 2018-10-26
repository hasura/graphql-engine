Multiple mutations in a request
===============================
If multiple mutations are part of the same request, they are executed **sequentially**. If any of the mutations fail,
all the executed mutations will be rolled back (i.e. all the mutations are run as a **transaction**).

Example: Insert objects of different types in the same mutation
---------------------------------------------------------------
Insert an ``author`` object and an ``article`` object written by the author:

.. graphiql::
  :view_only:
  :query:
    mutation insert_author_and_article {
      insert_author(
        objects: [
          {id: 11, name: "Jane"}
        ]
      ) {
        affected_rows,
        returning {
          id
          name
        }
      }
      insert_article(
        objects: [
          {id: 21, title: "Article 1", content: "Sample content", author_id: 11}
        ]
      ) {
        affected_rows,
        returning {
          id
          title
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
              "id": 11,
              "name": "Jane"
            }
          ]
        },
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

..
  Insert an object and a nested object in the same mutation
  ---------------------------------------------------------
  *This is currently work in progress*.