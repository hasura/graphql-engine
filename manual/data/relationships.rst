Data relationships
==================

There is usually some kind of association between tables in a database. These associations are typically captured by foreign key constraints when the data is modelled. The data APIs lets you define relationships based on these foreign key constraints.

For example, an ``article`` table might have a column called ``author_id`` which points to a row in the ``author`` table. Because of this, you may wish to fetch the *articles* of each category when fetching authors, or fetch the *author* of an article when fetching articles.

These additional properties, *articles* of an *author* and *author* of an *article*, made possible because of foreign key constraints are what we call relationships. *articles* of *author* is an **array relationship** while *author* of *article* is an **object relationship**.

Creating relationships
----------------------

You can create relationship metadata for tables via the API console.

Let's say you wish to add an object relationship for ``article::author_id -> author::id``. Navigate to the *Relationships* tab in the ``article`` table.

You'll see an entry in *suggested object relationships*:

.. image:: ../../img/complete-tutorial/tutorial-suggested-relationships.png

Click on *Add* to add a new object relationship and name the relationship:

.. image:: ../../img/complete-tutorial/tutorial-create-relationship.png

The relationship is created:

.. image:: ../../img/complete-tutorial/tutorial-created-relationship.png


Fetching over relationships
---------------------------

To obtain the **author**'s name from the article table, we issue,

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if request needs particular user role

   {
       "type" : "select",
       "args" : {
           "table" : "article",
           "columns": [
               "title",
               {
                   "name": "author",
                   "columns": ["name"]
               }
           ],
           "where" : {"is_published" : true}
       }
   }

The same syntax can be used to obtain the titles of all articles across all **authors**.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if request needs particular user role

   {
       "type" : "select",
       "args" : {
           "table" : "author",
           "columns": [
               "name",
               {
                   "name": "articles",
                   "columns": ["title"]
               }
           ]
       }
   }

You can use relationships inside ``where`` clause. For example, if we wish to only fetch all published articles by author with name ``Warren`` , we could :

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if request needs particular user role

   {
       "type" : "select",
       "args" : {
           "table" : "article",
           "columns": [ "id", "title"],
           "where" : {
               "is_published" : true,
               "author" : {
                   "name" : "Warren"
               }
           }
       }
   }

Let's fetch authors who have never published anything.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if request needs particular user role

   {
       "type" : "select",
       "args" : {
           "table" : "author",
           "columns": ["name"],
           "where" : {
               "$not" : {
                   "articles" : { "$any" : { "is_published" : true }}
               }
           }
       }
   }

As you probably guessed, relationships can be nested. Let's get all published articles with author information, comments and the author who posted the comment.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if request needs particular user role

   {
       "type" : "select",
       "args" : {
           "table" : "article",
           "columns": [
               "title",
               {
                   "name": "author",
                   "columns": ["name"]
               },
               {
                   "name" : "comments",
                   "columns" : [
                       "comment",
                       {
                           "name" : "commented_by",
                           "columns" : ["name"]
                       }
                   ]
               }
           ],
           "where" : {"is_published" : true}
       }
   }

We can also use ``where``, ``limit``, ``offset`` inside array relationships. Let's say we want to fetch all authors and only their published articles:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if request needs particular user role

   {
       "type" : "select",
       "args" : {
           "table" : "author",
           "columns": [
               "name",
               {
                   "name": "articles",
                   "columns": ["title"],
                   "where" : { "is_published" : true }
               }
           ]
       }
   }
