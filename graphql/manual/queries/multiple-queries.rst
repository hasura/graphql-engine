Multiple queries in a request
=============================
If multiple queries are part of the same request, they are executed **parallely**, the individual responses are collated and returned. You can fetch objects of different unrelated types in the same query. For e.g. to fetch a list of ``authors`` and a list of ``reviews``:

.. graphiql::
   :query:
        query {
            author (limit: 2){
                id
                name
            },
            reviews{
                content
                article_id
            }
        }
   :response:
        {
            "data": {
                "author": [
                {
                    "id": 1,
                    "name": "Chrissie"
                },
                {
                    "id": 2,
                    "name": "Aubrey"
                }
                ],
                "reviews": [
                {
                    "content": "Great article!",
                    "article_id": 8
                },
                {
                    "content": "such content, much wow!",
                    "article_id": 10
                }
                ]
            }
        }