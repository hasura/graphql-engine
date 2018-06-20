Using multiple arguments
========================
Multiple arguments can be used together in the same query. For e.g. if you want to use the where argument to filter the results and then use the order_by argument to sort them, you can use a query similar to the following one to fetch a list of authors and only their published articles that are sorted by the date of publication (descending):

.. graphiql::
   :query:
        query {
            author {
                id
                name
                articles (
                where: {is_published:{_eq:true}}
                order_by: ["-published_on"]
                )  {
                    id
                    title
                    published_on
                }
            }
        }
   :response:
        {
            "data": {
                "author": [
                    {
                        "id": 1,
                        "name": "Chrissie",
                        "articles": []
                    },
                    {
                        "id": 2,
                        "name": "Aubrey",
                        "articles": []
                    },
                    {
                        "id": 3,
                        "name": "Mallorie",
                        "articles": []
                    },
                    {
                        "id": 10,
                        "name": "Obie",
                        "articles": [
                        {
                            "id": 2,
                            "title": "a some title",
                            "published_on": "2018-06-14"
                        }
                        ]
                    }
                ]
            }
        }
