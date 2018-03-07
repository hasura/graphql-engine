Data relationships
==================

There is usually some kind of association between tables in a database. These associations are typically captured by foreign key constraints when the data is modelled. The data APIs lets you define relationships based on these foreign key constraints.

For example, an ``article`` table might have a column called ``category_id`` which points to a row in the ``category`` table. Because of this, you may wish to fetch the *articles* of each category when fetching categories, or fetch the *category* of an article when fetching articles.

These additional properties, *articles* of a *category* and *category* of an *article*, made possible because of foreign key constraints are what we call relationships. *articles* of *category* is an **array relationship** while *category* of *article* is an **object relationship**.

Head to the ``Relationships`` section of a table in the API console. It'll suggest you the appropriate relationships based on the foreign key constraints that the table is involved in.

To obtain the **author**'s name from the article table, we issue,

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

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
   Authorization: <admin-token>

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
   Authorization: <admin-token>

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
   Authorization: <admin-token>

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
   Authorization: <admin-token>

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
   Authorization: <admin-token>

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


