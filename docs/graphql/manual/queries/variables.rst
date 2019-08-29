Using variables in a query
==========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

In order to make a query re-usable, it can be made dynamic by using variables.

**Example:** Fetch an author by their ``author_id``:

.. graphiql::
  :view_only:
  :query:
    query getArticles($author_id: Int!) {
        articles(where: {author_id: {_eq: $author_id}}) {
            id
            title
        }
    }
  :response:
    {
        "data": {
            "articles": [
            {
                "id": 15,
                "title": "How to climb Mount Everest"
            },
            {
                "id": 6,
                "title": "How to be successful on broadway"
            }
            ]
        }
    }
  :variables:
    {
      "author_id": 1
    }
