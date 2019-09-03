Using aliases in a query
========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Aliases can be used to return a certain selection of objects based on any filter criteria (e.g. ``order_by``, ``limit`` etc.), and with any name.

**Example:** First, fetch all articles. Second, fetch the two top-rated articles. Third, fetch the worst-rated article:

.. graphiql::
  :view_only:
  :query:
    query getArticles {
        articles {
            title
            rating
        }
        topTwoArticles: articles(order_by: {rating: desc}, limit: 2) {
            title
            rating
        }
        worstArticle: articles(order_by: {rating: asc}, limit: 1) {
            title
            rating
        }
    }
  :response:
    {
        "data": {
            "articles": [
            {
                "title": "How to climb Mount Everest",
                "rating": 4
            },
            {
                "title": "How to be successful on broadway",
                "rating": 20
            },
            {
                "title": "How to make fajitas",
                "rating": 6
            }
            ],
            "topTwoArticles": [
            {
                "title": "How to be successful on broadway",
                "rating": 20
            },
            {
                "title": "How to make fajitas",
                "rating": 6
            }
            ],
            "worstArticle": [
            {
                "title": "How to climb Mount Everest",
                "rating": 4
            }
            ]
        }
    }