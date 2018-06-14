.. _one_to_many:

=========================
One-to-many relationships
=========================

In a one-to-many relationship, one record in a table can be associated with one or more records in another table. In the
Hasura data APIs, one-to-many relationships are referred to as ``array relationships``.

For example, in an ``article`` ``author`` schema, ``author`` can have multiple ``articles``.

Creating a one-to-many relationship
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To add a one-to-many relationship from ``author`` to ``article``,

#. :doc:`Create a foreign key constraint <../alter-schema/adding-foreign-key-constraint>`  on the ``article`` table to
   the ``author`` table.
#. Open the :doc:`API console <../../api-console/index>`, navigate to *Data > author > Relationships* section.
#. Add the suggested array relationship and give it a desired name.

.. image:: img/one-to-many-rel.png

Fetching over a one-to-many relationship
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To fetch an ``author`` with ``name = "Clara"`` and all the ``articles`` authored by her, use a query like so:

.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

         query fetch_author {
            author (where: { name: "Clara" ){
                name
                article {
                    title
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
                "table" : "author",
                "columns": [
                    "name",
                    {
                        "name": "article",
                        "columns": ["title"]
                    }
                ],
                "where" : {
                    "name": "Clara"
                }
            }
        }
