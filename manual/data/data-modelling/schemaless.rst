===============
Schemaless Data
===============

Since Hasura uses Postgresql, you get support for NoSQL data on a Relational database. The ``JSONB`` column type can be used to store data as JSON and then query them based on the values of different keys inside of the JSON.

When to use JSON data types?
----------------------------

Relational modeling can be used for most applications. But there are some use-cases where storing data as a JSON document makes sense

- You can avoid complicated JOINS on tables that hold isolated data by storing them as a JSON document.
- If you are dependent on data coming from an external API as JSON, you can avoid the process of normalizing this data into different tables. Instead, you can store this data in the same format and structure that you received it in.
- It also helps in cases where you are dependent on data whose schema is not fixed.

.. note::

  PostgreSQL provides two data types to store JSON elements: JSON and JSONB. The main difference between them is their efficiency. JSON is stored as test and JSONB is first decomposed into binary components and then stored. This makes inserts into JSONB slower but parsing of sub elements is faster.

Creating a table to store JSON data
-----------------------------------

Let's take an example of a case where we have a ``user`` table which stores information like ``id``, ``name`` and ``address``. ``address`` will store the user address with data like house or apartment number and name, streetname, landmark, city, pincode etc. Since this structure is not stringent, it makes sense to store this data as a ``JSON``

- Head to the *Data > Create Table* section of the :doc:`API console <../../api-console/index>` to :doc:`create a table <../create-table>` called ``user`` with the following schema

+----------------------------------------+----------------------------------------+
|Columns                                 |Type                                    |
+========================================+========================================+
|id                                      |Integer                                 |
+----------------------------------------+----------------------------------------+
|name                                    |Text                                    |
+----------------------------------------+----------------------------------------+
|address                                 |JSONB                                   |
+----------------------------------------+----------------------------------------+

The above schema will look like this:

.. image:: img/schemaless-create-table-user.png

Inserting JSON data
-------------------

To insert data into the ``user`` table, you have to execute an :doc:`insert query <../insert>`

.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

        mutation addUser($objects: [user_input]) {
          insert_user(objects: $objects) {
            affected_rows
          }
        }

        Variables->

        {
          "objects": [
            {
              "id": 1,
              "name": "Jack Smith",
              "address": {
                "house_number": "112",
                "house_name": "XYZ Apartments",
                "street_name": "ABC Street",
                "city": "Bengaluru",
                "pincode": "123456"
              }
            }
          ]
        }

   .. tab:: JSON API

      .. code-block:: http

         POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
         Content-Type: application/json
         Authorization: Bearer <auth-token> # optional if cookie is set
         X-Hasura-Role: <role>  # optional. Required if request needs particular user role

         {
           "type": "insert",
           "args" : {
               "table": "user",
               "objects": [
               {
                 "id": 2,
                 "name": "Jack Smith",
                 "address": {
                   "house_number": "112",
                   "house_name": "XYZ Apartments",
                   "street_name": "ABC Street",
                   "city": "Bengaluru",
                   "pincode": "123456"
                   }
                 }
               ]
            }
          }

Fetching data based on JSON values
----------------------------------

Currently, we cannot directly use the GraphQL or JSON APIs to filter data by the different properties of the stored JSON. We can however, create a view which holds all of this data and then query that view.

Head to *Data > SQL* section of the :doc:`API console <../../api-console/index>` and run the following SQL command.

.. note::

  Ensure that you check the ``Track Table`` checkbox before running the query so that you can use Data APIs to query the view.

.. code-block:: SQL

  CREATE VIEW user_address AS
  SELECT id as user_id, address->>'city' as city, address->>'pincode' as pincode
  FROM "user";

This will create a view called ``user_address`` with ``user_id``, ``city`` and ``pincode`` as columns.

.. image:: img/schemaless-view-user-address.png

We can now :doc:`fetch data <../select>` from this view just like you would from a table.

Moreover, you can also :ref:`create a manual relationship <relationship_without_fkey>` to this view from your user table on the `user_id` column.

- Relationship Type will be ``Object Relationship``
- Relationship Name can be "address_info"
- Configuration: ``id :: address_info -> user_id``

You can now filter the ``user`` table by city or pincode

.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

        query {
          user(where: {address_info: {city: {_eq: "Chennai" }}}) {
            id
            name
            address_info{
              city
              pincode
            }
          }
        }

   .. tab:: JSON API

      .. code-block:: http

         POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
         Content-Type: application/json
         Authorization: Bearer <auth-token> # optional if cookie is set
         X-Hasura-Role: <role>  # optional. Required if request needs particular user role

         {
            "type": "select",
            "args": {
                "table": "user",
                "columns": [
                    "id",
                    "name",
                    "address",
                    {
                        "name": "address_info",
                        "columns": [
                            "city",
                            "pincode"
                            ]
                    }
                ],
                "where": {
                    "address_info": {
                        "city": "Chennai"
                    }
                }
            }
         }

Updating JSON data
-------------------------------

Currently, there is no direct support within the Data APIs to directly manipulate data inside of the JSON. To update, you will have to replace the whole JSON document using the :doc:`update query <../update>`.
