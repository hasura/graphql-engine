Insert mutation
===============

Insert single object
--------------------
Insert a new ``article`` object and return the inserted article object in the response:

.. graphiql::
  :view_only: true
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

Insert multiple objects of the same type in the same mutation
-------------------------------------------------------------
Insert 2 new ``article`` objects and return both the article objects in the response:

.. graphiql::
  :view_only: true
  :query:
    mutation insert_article {
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