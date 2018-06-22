Delete mutation
===============

Objects can be updated based on filters on their own fields or those in their nested objects. 

Filter on an object's fields
----------------------------
Delete all articles rated less than 3:

.. graphiql::
   :view_only: true
   :query:
        mutation delete_low_rated_articles{
            delete_article(
                where: {rating: {_lt: 3}}
            ) {
                affected_rows
            }
        }
   :response:
        {
            "data": {
                "delete_low_rated_articles": {
                "affected_rows": 49
                }
            }
        }


Filter on a nested object's fields
----------------------------------
Delete all articles written by an author:

.. graphiql::
   :query:
        mutation delete_authors_articles{
            delete_article(
                where: {author: {id: {_eq: 7}}}
            ) {
                affected_rows
            }
        }
   :response:
        {
            "data": {
                "delete_authors_articles": {
                "affected_rows": 1
                }
            }
        }