Fetching data over relationships
================================

Let's assume that we have an array relationship from the ``author`` table to the ``article`` table and an object
relationship from the ``article`` table to the ``author`` table.

To obtain the **author**'s name from the article table, we issue,

.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

         query fetch_articles {
           article (where: {is_published: {_eq: true}}) {
              title
              author {
                 name
              }
           }
         }

   .. tab:: JSON API

      .. code-block:: http

         POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
         Content-Type: application/json
         Authorization: Bearer <auth-token> # optional if cookie is set
         X-Hasura-Role: <role>  # optional. Required if only specific user role has access

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


.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

         query fetch_authors {
           author {
              name
              articles {
                 title
              }
           }
         }

   .. tab:: JSON API

      .. code-block:: http

         POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
         Content-Type: application/json
         Authorization: Bearer <auth-token> # optional if cookie is set
         X-Hasura-Role: <role>  # optional. Required if only specific user role has access

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



You can use relationships inside ``where`` clause. For example, if we wish to only fetch all published articles by
author with name ``Warren`` , we could :


.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

         query fetch_articles {
           article (where: { is_published: true, author: {name: "Warren"}}) {
              id
              title
              author {
                 name
              }
           }
         }

   .. tab:: JSON API

      .. code-block:: http

         POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
         Content-Type: application/json
         Authorization: Bearer <auth-token> # optional if cookie is set
         X-Hasura-Role: <role>  # optional. Required if only specific user role has access

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

.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

         query fetch_author {
           author (where: { $not: { article: { $any: { is_published: true}}}}) {
              id
              name
           }
         }

   .. tab:: JSON API

      .. code-block:: http

         POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
         Content-Type: application/json
         Authorization: Bearer <auth-token> # optional if cookie is set
         X-Hasura-Role: <role>  # optional. Required if only specific user role has access

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

As you probably guessed, relationships can be nested. Let's get all published articles with author information, comments
and the author who posted the comment.

.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

         query fetch_article {
           article (where: {is_published: true}) {
              title
              author {
                 name
              }
              comments {
                 comment
                 commented_by {
                    name
                 }
              }
           }
         }

   .. tab:: JSON API

      .. code-block:: http

         POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
         Content-Type: application/json
         Authorization: Bearer <auth-token> # optional if cookie is set
         X-Hasura-Role: <role>  # optional. Required if only specific user role has access

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


We can also use ``where``, ``limit``, ``offset`` inside array relationships. Let's say we want to fetch all authors and
only their published articles:

.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

         query fetch_article {
            author {
              articles (where: {is_published: true}) {
                 title
              }
            }
         }

   .. tab:: JSON API

      .. code-block:: http

         POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
         Content-Type: application/json
         Authorization: Bearer <auth-token> # optional if cookie is set
         X-Hasura-Role: <role>  # optional. Required if only specific user role has access

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
