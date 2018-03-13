.. .. meta::
   :description: Query templates: Learn how to template Data microservice queries and to define and manage your own REST interfaces with detailed examples.
   :keywords: hasura, docs, data, query templates, REST interface, REST endpoint, RESTful API

Data API Reference: Query Templates
===================================

Query templates are used to template queries and to define your own REST interface. Let's jump into some examples.

Let's say we would like to query the article table to fetch ``id`` and ``title`` of all published articles written by an author. We can issue a ``select`` query as follows:

.. code-block:: http
   :emphasize-lines: 12

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type" : "select",
       "args" : {
           "table" : "article",
           "columns": ["id", "title"],
           "where": {
               "is_published": true,
               "author_id" : 1
           }
       }
   }

Now, if we find ourselves using this query quite often but with a different value for ``author_id``, it can be convenient to give this query a name and provide ``author_id`` as an argument instead of specifying the entire query. This is accomplished with query templates. We can create one using the ``create_query_template`` query:

.. code-block:: http
   :emphasize-lines: 8-9, 16

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type" : "create_query_template",
       "args" : {
           "name" : "article_minimal",
           "template" : {
               "type" : "select",
               "args" : {
                   "table" : "article",
                   "columns": ["id", "title"],
                   "where": {
                       "author_id": {
                           "$eq" : { "param" : "author_id" }
                       },
                       "is_published" : true
                   }
               }
           }
       }
   }

In the above request, we've templated the ``author_id`` in the ``select`` query with the parameter ``author_id`` instead of a concrete value like ``1`` and given it the name "article_minimal". Now to execute this query template:

.. code-block:: http
   :emphasize-lines: 9-11

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type" : "execute_query_template",
       "args" : {
           "name" : "article_minimal",
           "args" : {
               "author_id" : 1
           }
       }
   }

This query executes the query template ``article_minimal`` with ``author_id`` set to ``1``.

As you may have noticed, query templates are extremely useful in fetching highly nested data. The query templates are not limited to ``select``, you can also template ``insert``, ``update``, ``delete``, ``count`` and ``bulk`` queries.

Query templates also let you define a REST interface for your application. For example, you can also execute the above query template as follows:

.. code-block:: http
   :emphasize-lines: 1

   GET data.<cluster-name>.hasura-app.io/v1/template/article_minimal?author_id=1 HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

While the above example uses the ``GET`` method, you can also use ``PUT``, ``DELETE``, ``POST`` as appropriate to the query that you have templated.

Here is another example where a delete query is templated:

.. code-block:: http
   :emphasize-lines: 15,19

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type" : "create_query_template",
       "args" : {
           "name" : "delete_article_tag",
           "template" : {
               "type" : "delete",
               "args" : {
                   "table" : "article_tag",
                   "where": {
                       "article_id": {
                           "$eq" : { "param" : "article_id" }
                       },
                       "tag": {
                           "name" : {
                               "$eq" : { "param" : "tag_name" }
                           }
                       },
                       "article" : {
                           "is_published" : false
                       }
                   }
               }
           }
       }
   }

We would like to allow authors to delete a tag on articles if they are not yet published. The above query template can be used for this purpose. It has two parameters ``article_id`` and ``tag_name``. To execute the query template:

.. code-block:: http
   :emphasize-lines: 9-12

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type" : "execute_query_template",
       "args" : {
           "name" : "article_minimal",
           "args" : {
               "article_id" : 1,
               "tag_name" : "opinion"
           }
       }
   }

or

.. code-block:: http
   :emphasize-lines: 1

   DELETE data.<cluster-name>.hasura-app.io/v1/template/delete_article_tag?article_id=1&tag_name=opinion HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

Permissions on query templates
------------------------------

When executing a query template, the template is essentially converted into a query. So, the permissions on the query automatically apply. For example, if there are *no* permissions for ``anonymous`` role to ``select`` from a table, then, there are *no* permissions for ``anonymous`` role to execute a templated ``select`` query on the table.

.. _create_query_template:

create_query_template
----------------------

``create_query_template`` is used to template a query and attach a name to it.

Here is an example for a select query which uses the ``default`` values in the template parameters.

.. code-block:: http
   :emphasize-lines: 14, 18, 23-24, 27-28

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type" : "create_query_template",
       "args" : {
           "name" : "article_homepage",
           "template": {
             "type": "select",
             "args": {
               "table" : "article",
               "columns": ["id", "title"],
               "where": {
                   "is_published": {
                       "$eq" : { "param" : "is_published" }
                   },
                   "article_like_count" : {
                       "like_count" : {
                           "$gt" : { "param" : "like_count_min" }
                       }
                   }
               },
               "limit" : {
                   "param" : "limit",
                   "default" : 10
               },
               "offset" : {
                   "param" : "offset",
                   "default" : 0
               }
             }
          }
       }
   }

The ``limit`` and ``offset`` parameters each define a default value. These are used when these parameters are not provided during the execution.

The following section describes the syntax of ``create_query_template`` and the parts of each query that can be templated.

.. _create_query_template_syntax:

Syntax
^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - String
     - Name of the query template
   * - template
     - true
     - TemplatedQuery_
     - A query which is templated
   * - comment
     - false
     - Text
     - A comment, probably describing the query template

``TemplatedQuery``
&&&&&&&&&&&&&&&&&&

A :ref:`Query <query_def>` with TemplateParam_ for concrete values in a query. The queries that can be templated are ``select``, ``insert``, ``update``, ``delete``, ``count`` and ``bulk``. The templatable parts of these queries are as follows:

.. list-table::
   :header-rows: 1

   * - Query type
     - Templatable parts
   * - select
     - values in the ``where`` clause, ``limit`` and ``offset``
   * - insert
     - only the ``objects`` key
   * - update
     - values in the ``where`` clause, values of ``$set``, ``$inc``, ``$mul``
   * - delete
     - values in the ``where`` clause
   * - count
     - values in the ``where`` clause
   * - bulk
     - templatable parts of the included queries

``TemplateParam``
&&&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - param
     - true
     - Text
     - A parameter name
   * - default
     - false
     - Value
     - A default value which is used when this parameter is not provided during execution

Let's look at examples for other query types mentioned above.

Here's an example for an ``insert`` query.

.. code-block:: http
   :emphasize-lines: 13-15

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type" : "create_query_template",
       "args" : {
           "name" : "insert_article",
           "template" : {
             "type" : "insert",
             "args" : {
                 "table" : "article",
                 "objects" : {
                    "param" : "article_objects"
                 }
             }
          }
       }
   }

As mentioned above, only the objects key in a insert query is templatable. This would insert ``n`` number of articles into the ``article`` table, where ``n`` is the length of the ``article_objects`` array. 

Here's an example for an ``update`` query.

.. code-block:: http
   :emphasize-lines: 13-15

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type" : "create_query_template",
       "args" : {
           "name" : "update_article_author",
           "template" : {
             "type" : "update",
             "args" : {
                 "table" : "article",
                 "$set" : {
                    "author_id" : {
                      "param" : "author_id"
                    }
                  },
                  "where" : {
                    "is_published" : {
                      "$eq" : true
                    }
                  }
             }
          }
       }
   }

In the above example, we are trying to update the author of all articles which are published. Note that ``$set`` has the templated param ``author_id``.

.. _execute_query_template:

execute_query_template
----------------------

Execute any query template. Let's execute the query template defined above:

.. code-block:: http
   :emphasize-lines: 9-13

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type" : "execute_query_template",
       "args" : {
           "name" : "article_homepage",
           "args" : {
               "is_published" : true,
               "article_like_count" : 20,
               "limit" : 50
           }
       }
   }

This would fetch the first 50 published articles which have at least 20 likes.

You can also execute a query template by making a HTTP request to ``/v1/template/<template_name>``. The HTTP method used defines how the arguments to the template are obtained.

.. list-table::
   :header-rows: 1

   * - HTTP Method
     - Template arguments
   * - ``GET``
     - url parameters
   * - ``POST``
     - url parameters and json body. The arguments in the body take precedence.
   * - ``PUT``
     - url parameters and json body. The arguments in the body take precedence.
   * - ``DELETE``
     - url parameters

For example, the ``article_homepage`` template can be executed by any of the following means. All are equivalent.

.. code-block:: http

   GET data.<cluster-name>.hasura-app.io/v1/template/article_homepage?is_published=true&like_count_min=20&limit=50 HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/template/article_homepage?is_published=true&limit=20 HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "article_like_count" : 20,
       "limit" : 50
   }

.. code-block:: http

   PUT data.<cluster-name>.hasura-app.io/v1/template/article_homepage?is_published=true&limit=20 HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "article_like_count" : 20,
       "limit" : 50
   }

.. code-block:: http

   DELETE data.<cluster-name>.hasura-app.io/v1/template/article_homepage?is_published=true&like_count_min=20&limit=50 HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

In the above example, in case of ``POST`` and ``PUT``, the parameters in the body override the url parameters and hence they are all equivalent.

.. note::

   There is no enforcement from the data microservice on the method used to execute a query template (i.e, you can use the ``DELETE`` verb to execute a query template on ``select``). It is left to the developer to use the method appropriate for the template.

Syntax
^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - String
     - Name of the query template
   * - args
     - true
     - Object (TemplateParam : Value)
     - An object with parameter names for keys and template arguments for values

.. _drop_query_template:

drop_query_template
-------------------

``drop_query_template`` is used to drop an existing query template.

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type" : "drop_query_template",
       "args" : {
           "name" : "article_minimal"
       }
   }

Syntax
^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - String
     - Name of the query template

.. _set_query_template_comment:

set_query_template_comment
--------------------------

``set_query_template_comment`` is used to set/update the comment on a query template. Setting the comment to ``null`` removes it. For example,

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type": "set_query_template_comment",
       "args": {
           "name": "article_homepage",
           "comment" : "used for homepage data"
       }
   }

Syntax
^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - String
     - The name of the query template
   * - comment
     - false
     - Text
     - comment

List existing query templates
-----------------------------

To fetch a list of created query templates, run the following query using psql or adminer:

.. code-block:: sql

   select * from hdb_catalog.hdb_query_template;

You can also issue the following query to the data microservice.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type": "select",
       "args": {
           "table" : {
               "schema" : "hdb_catalog",
               "name" : "hdb_query_template"
           },
           "columns": ["template_name", "template_defn", "comment"]
       }
   }

.. note::

   Query templates are in beta. However, the API has been stabilised and will *not* change in future.
