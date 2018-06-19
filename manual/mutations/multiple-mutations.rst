Multiple mutations
==================
If multiple mutations are part of the same request, they are executed **sequentially**.

Insert objects of different unrelated types in the same mutation
----------------------------------------------------------------
Insert an ``article`` object and an unrelated ``review`` object:

.. graphiql::
   :query:
        mutation insert_article_and_review {
          insert_article(objects: [{title: "Article 6", content: "Sample article content", author_id: 4}]) {
            returning {
              id
              title
            }
          }
          insert_reviews(objects: [{content: "Nice Article!", article_id: 9}]) {
            affected_rows
            returning {
              id
              article_id
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
                  "id": 109,
                  "title": "Article 6"
                }
              ]
            },
            "insert_reviews": {
              "affected_rows": 1,
              "returning": [
                {
                  "id": 3,
                  "article_id": 9
                }
              ]
            }
          }
        }

Insert and object and a nested object in the same mutation
----------------------------------------------------------
*This is currently work in progress*.