Using fragments in a query
==========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:


Sometimes, queries can get long and confusing. A fragment is a set of fields with any chosen name. This fragment can then be used to represent the defined set.

**Example:** Creating a fragment for a set of ``article`` fields (``id`` and ``title``) and using it in a query:

.. graphiql::
  :view_only:
  :query:
    fragment articleFields on articles {
        id
        title
    }
    query getArticles {
        articles {
            ...articleFields
        }
        topTwoArticles: articles(order_by: {rating: desc}, limit: 2) {
            ...articleFields
        }
    }
  :response:
    {
        "data": {
            "articles": [
            {
                "id": 3,
                "title": "How to make fajitas"
            },
            {
                "id": 15,
                "title": "How to climb Mount Everest"
            },
            {
                "id": 6,
                "title": "How to be successful on broadway"
            }
            ],
            "topTwoArticles": [
            {
                "id": 6,
                "title": "How to be successful on broadway"
            },
            {
                "id": 3,
                "title": "How to make fajitas"
            }
            ]
        }
    }

