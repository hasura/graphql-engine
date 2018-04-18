===========================
Self referencing in a table
===========================

Sometimes, you would want a row in a table to refer to another row in the same table. For example, to model nested
comments for an article.

In this section we will create a relationship from a table to itself.

Consider the ``comment`` table below. Let's try to model replies to comments.

``comment``

+--------------+-----------------------------+
| column       | type                        |
+==============+=============================+
| comment_id   | serial NOT NULL primary key |
+--------------+-----------------------------+
| text         | text NOT NULL               |
+--------------+-----------------------------+
| commented_at | timestamp NOT NULL          |
+--------------+-----------------------------+
| author_id    | int NOT NULL                |
+--------------+-----------------------------+
| article_id   | int NOT NULL                |
+--------------+-----------------------------+


Creating a relationship to self
-------------------------------

Let us add ``parent_comment`` and ``child_comments`` relationships to our comment table.

#. Open the :doc:`API console <../../api-console/index>` and navigate to the *Data > comment > Modify* section.
#. Add a new column called ``parent_comment_id`` (Make it nullable because the top level comments will have no
   ``parent_comment``).
#. Once the column is created, click on the ``Edit`` button next to it.
#. Check the ``Foreign Key`` checkbox and make the reference table and reference column to be ``comment`` and
   ``comment_id`` respectively.

   .. image:: img/self-referencing-fk.png


#. Next, go to the ``Relationships`` tab.
#. Add the suggested relationships. Name the object and array relationships as ``parent_comment`` and
   ``children_comments`` respectively.

   .. image:: img/self-referencing-relationship.png

Fetching over a self relationship
---------------------------------

To fetch all the list of all top level comments along with their children comments:

.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

         query fetch_comment {
            comment (where: {parent_comment_id: null}) {
                text
                commented_at
                author_id
                article_id
                children_comments {
                    text
                    commented_at
                    article_id
                    author_id
                }
            }
         }

   .. tab:: JSON API

      .. code-block:: http
        :emphasize-lines: 14

        POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
        Content-Type: application/json
        Authorization: Bearer <auth-token> # optional if cookie is set
        X-Hasura-Role: admin

        {
            "type" : "select",
            "args" : {
                "table" : "comment",
                "columns": [
                    "*",
                    {
                        "name": "children_comments",
                        "columns": ["*"]
                    }
                ],
                "where": {
                    "parent_comment_id": null
                }
            }
        }
