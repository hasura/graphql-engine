========================
One-to-one relationships
========================

One-to-One (1-1) relationship is defined as the relationship between two tables where both the tables should be associated with each other based on only one matching row.

Lets consider two sample tables: ``person`` and ``voter``

+----------------------------------------+----------------------------------------+
|Table                                   |Columns                                 |
+========================================+========================================+
|person                                  |id (p_key), name, age, gender           |
+----------------------------------------+----------------------------------------+
|voter                                   |voter_id (p_key), person_id             |
+----------------------------------------+----------------------------------------+

In the above schema, a ``person`` can have only one ``voter_id`` and every ``voter_id`` can identify only one person.

Lets add a one-to-one relationship between the ``person`` and ``voter`` tables.

Creating a one-to-one relationship
----------------------------------

To create a one-to-one relationship, we have to create a relationship over a foreign key created on **unique** columns
in both the tables. In this case, we will add a uniqueness constraint over the ``person_id`` column in the ``voter`` table.
In the case of ``person`` table, ``id`` is a primary key and therefore already unique.

#. Open the :doc:`API console <../../api-console/index>` and navigate to the *Data > SQL* section.

#. Add uniqueness constraint:

   #. Add the following SQL command to create a uniqueness constraint on the ``person_id`` column of the ``voter`` table.

      .. code-block:: sql

         CREATE UNIQUE INDEX ON voter (person_id)

   #. Check the ``This is a migration`` checkbox so that the query is saved as a migration.
   #. Hit ``Run``.

#. :doc:`Create an object relationship <../relationships/create-relationships>` from ``person``
   to ``voter``.

#. Similarly, :doc:`create an object relationship <../relationships/create-relationships>` from
   ``voter`` to ``person``.

Fetching over a one-to-one relationship
---------------------------------------

To fetch the list of all entries from the ``person`` table along with their ``voter_id``:

.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

        query fetch_person {
            person {
                name
                age
                gender
                voter_info {
                    voter_id
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
                "table" : "person",
                "columns": [
                    "*",
                    {
                        "name": "voter_info",
                        "columns": ["voter_id"]
                    }
                ]
            }
        }


Similarly, to fetch all the entries from the ``voter`` table along with the associated ``person`` info:

.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

         query fetch_voter {
            voter {
                id
                person_info {
                   name
                   age
                   gender
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
                "table" : "voter",
                "columns": [
                    "*",
                    {
                        "name": "person_info",
                        "columns": ["*"]
                    }
                ]
            }
        }
