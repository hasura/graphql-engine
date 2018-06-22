Update mutation
===============

Objects can be updated based on filters on their own fields or those in their nested objects. 

Update based on a filter on an object's fields
----------------------------------------------
Update the ``name`` of the author with a given ``id``:

.. graphiql::
   :view_only: true
   :query:
        mutation update_author{
            update_author(
                where: {id: {_eq:3}},
                _set: {name: "Jane Doe"}
            ) {
                affected_rows
            }
        }
   :response:
        {
            "data": {
                "update_author": {
                  "affected_rows": 1
                }
            }
        }

Update based on a filter on a nested object's fields
----------------------------------------------------
Update the ``rating`` of all articles that belong to an author:

.. graphiql::
   :view_only: true
   :query:
        mutation update_ratings{
            update_article(
                where: {author: {name: {_eq:"John Doe"}}},
                _set: {rating: 1}
            ){
                affected_rows
            }
        }
   :response:
        {
            "data": {
                "update_article": {
                "affected_rows": 7
                }
            }
        }