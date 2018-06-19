Mutations
=========

.. contents:: :local:

Introduction
--------------
GraphQL mutations are used to modify server-side data i.e. write, update or delete data. As with queries, mutation fields are auto-generated on the Postgres schema. Here’s a sample mutation field from our reference Authors/Articles schema:

.. code-block:: graphql

    insert_article(
      objects: [article_input!] 
      on_conflict: conflict_clause
      ): article_mutation_response

As you can see from the schema, you can:

#. Pass multiple objects to the mutation.
#. Return objects, from the affected rows, in the response.

.. note::
    
    You cannot return a nested object in the response. This is not supported by the GraphQL specification (*we are working on this as a feature*).

Let's use this reference Authors/Articles schema to look at different types of mutations.


Insert
------

Example: Insert single object
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Insert a new ``article`` object and return the inserted article object in the response:

.. graphiql::
   :query:
        mutation insert_article {
            insert_article (
                objects: [
                    {
                        title: "Article 1", 
                        content: "Sample article content",
                        author_id: 3
                    }
                ]
            ) 
            {
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
                          "id": 102,
                          "title": "Article 1"
                        }
                    ]
                }
            }
        }

Example: Insert multiple objects (same type) in the same mutation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Insert 2 new ``article`` objects and return both the article objects in the response:

.. graphiql::
   :query:
        mutation insert_article {
            insert_article (
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
            ) 
            {
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
                          "id": 102,
                          "title": "Article 1"
                        },
                        {
                          "id": 104,
                          "title": "Article 3"
                        }
                    ]
                }
            }
        }

Bulk mutations
--------------
If multiple mutations are part of the same request, they are executed **sequentially**.

Example: Insert objects of different unrelated types in the same mutation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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

Example: Insert and object and a nested object in the same mutation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*This is currently work in progress*.

Upsert
------
To convert an *insert* mutation into an *upsert* one, you need to specify the unique constraint(s) and the action to be taken in the case of a conflict or violation. There are two ways to specify unique constraints, either specify the name of a unique constraint (using the ``constraint`` argument) or a list of columns that have unique constraints on them (using the ``constraint_on`` argument). On conflict, you can choose to either ignore the mutation (``action: "ignore"``) or update the row that caused the conflict (``action: "update"``).

For the following examples, assume there's a unique constraint on the ``name`` column of the ``author`` table.

.. note::
    
    You can fetch the name of unqiue constraints by quering the ``information_schema.table_constraints`` table. Typically, the constraint is automatically named as ``<table-name>_<column-name>_key`` when using the console to add it. The API-console will soon carry this information in the ``Data`` section.

Example: Upsert with unique constraint name (update)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Insert a new object in the author table or, if the unique constraint, ``author_name_key``, is violated, update the existing object:

.. graphiql::
   :query:
        mutation insert_author {
            insert_author (
                    objects: [
                        {
                        name: "john doe",
                        id:1231
                        }
                    ],
                    on_conflict:{
                    constraint: "author_name_key",
                    action: "update"
                    }
            ) {
                affected_rows
            }
        }
   :response:
        {
            "data": {
                "insert_author": {
                  "affected_rows": 1
                }
            }
        }

The response shown above assumes that the name of the author in our object is not unique and then *updates* the corresponding row in the database.

Example: Upsert with unique constraint name (ignore)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Insert a new object into the author table or, if the unique constraint, ``author_name_key``, is violated, ignore the request:

.. graphiql::
   :query:
        mutation insert_author {
            insert_author (
                    objects: [
                        {
                        name: "john doe",
                        id:1231
                        }
                    ],
                    on_conflict:{
                    constraint: "author_name_key",
                    action: "ignore"
                    }
            ) {
                affected_rows
            }
        }
   :response:
        {
            "data": {
                "insert_author": {
                  "affected_rows": 0
                }
            }
        }

In this case, the insert mutation is ignored because there is a conflict.

Example: Upsert with columns with unique constraint (update)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Insert a new object into the author table or, if a unique constraint on the specified columns, in this case - ``name``, is violated, update the existing object with values from the fields (in this case - ``id``):

.. graphiql::
   :query:
        mutation insert_author {
            insert_author (
                    objects: [
                        {
                        name: "john doe",
                        id:1231
                        }
                    ],
                    on_conflict:{
                    constraint_on: ["name"],
                    action: "update"
                    }
            ) {
                affected_rows
            }
        }
   :response:
        {
            "data": {
                "insert_author": {
                  "affected_rows": 1
                }
            }
        }

Example: Upsert with columns with unique constraint (ignore)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Insert a new object into the author table or, if a unique constraint on the specified columns, in this case - ``name``, is violated, ignore the request:

.. graphiql::
   :query:
        mutation insert_author {
            insert_author (
                    objects: [
                        {
                        name: "john doe",
                        id:1231
                        }
                    ],
                    on_conflict:{
                    constraint_on: ["name"],
                    action: "ignore"
                    }
            ) {
                affected_rows
            }
        }
   :response:
        {
            "data": {
                "insert_author": {
                  "affected_rows": 0
                }
            }
        }

.. note::
    Primary key constraint is not the same as a unique constraint. So, if you include a column that is only part of a primary key as one of the ``constraint_on`` argument's parameters, you will run into the following error: ``there is no unique or exclusion constraint on target column(s)``.

Update
------
Objects can be updated based on filters on their own fields or those in their nested objects. 

Example: Update based on a filter on an object's fields
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Update the ``name`` of the author with a given ``id``:

.. graphiql::
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

Example: Update based on a filter on a nested object's fields
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Update the ``rating`` of all articles that belong to an author:

.. graphiql::
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

Delete
------
Objects can be updated based on filters on their own fields or those in their nested objects. 

Example: Delete based on a filter on an object's fields
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Delete all articles rated less than 3:

.. graphiql::
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


Example: Delete based on a filter on a nested object's fields
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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



