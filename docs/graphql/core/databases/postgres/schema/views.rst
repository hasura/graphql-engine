.. meta::
   :description: Customise the Hasura GraphQL schema with views
   :keywords: hasura, docs, schema, view

.. _custom_views:

Postgres: Extend schema with views
==================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:


What are views?
---------------

`Views <https://www.postgresql.org/docs/current/sql-createview.html>`__ can be used to expose the results of a custom
query as a virtual table. Views are not persisted physically i.e. the query defining a view is executed whenever
data is requested from the view.

Hasura GraphQL engine lets you expose views over the GraphQL API to allow querying them using both ``queries`` and
``subscriptions`` just like regular tables.

.. _create_views:

Creating views
--------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    Views can be created using SQL which can be run in the Hasura console:

    - Head to the ``Data -> SQL`` section of the Hasura console
    - Enter your `create view SQL statement <https://www.postgresql.org/docs/current/static/sql-createview.html>`__
    - Hit the ``Run`` button

  .. tab:: CLI

    1. :ref:`Create a migration manually <manual_migrations>` and add your `create view SQL statement <https://www.postgresql.org/docs/current/static/sql-createview.html>`__ to the ``up.sql`` file. Also, add an SQL statement to the ``down.sql`` file that reverts the previous statement.

    2. Apply the migration and metadata by running:

       .. code-block:: bash

          hasura migrate apply

  .. tab:: API

    You can add a view by using the :ref:`run_sql metadata API <run_sql>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
          "sql": "<create view statement>"
        }
      }

Tracking views
--------------

Views can be present in the underlying Postgres database without being exposed over the GraphQL API.
In order to expose a view over the GraphQL API, it needs to be **tracked**.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    While creating views from the ``Data -> SQL`` page, selecting the ``Track this`` checkbox
    will expose the new view over the GraphQL API right after creation.

    You can track any existing views in your database from the ``Data -> Schema`` page:

    .. thumbnail:: /img/graphql/core/schema/schema-track-views.png
       :alt: Track views


  .. tab:: CLI

    1. To track the view and expose it over the GraphQL API, edit the ``tables.yaml`` file in the ``metadata`` directory as follows:

       .. code-block:: yaml
         :emphasize-lines: 7-9

            - table:
                schema: public
                name: author
            - table:
                schema: public
                name: article
            - table:
                schema: public
                name: <name of view>

    2. Apply the metadata by running:

       .. code-block:: bash

        hasura metadata apply

  .. tab:: API

    To track the view and expose it over the GraphQL API, make the following API call to the :ref:`track_table metadata API <track_table>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "track_table",
        "args": {
          "schema": "public",
          "name": "<name of view>"
        }
      }


Use cases
---------

Views are ideal solutions for retrieving some derived data based on some custom business logic. If your custom logic
requires any user input, you should use :ref:`custom SQL functions <custom_sql_functions>` instead.

Let's look at a few example use cases for views:

Example: Group by and then aggregate
************************************

Sometimes we might want to fetch some data derived by aggregating (avg, min, max, etc.) over a group of rows in a table.

Let’s say we want to fetch the average article rating for each author in the following schema:

.. code-block:: plpgsql

  author(id integer, name text, city text, email text, phone integer, address text)

  article(id integer, title text, content text, rating integer, author_id integer)

A view that averages the rating of articles for each author can be created using the following SQL query:

.. code-block:: SQL

  CREATE VIEW author_average_rating AS
    SELECT author_id, avg(rating)
      FROM article
      GROUP BY author_id


Example: Hide certain fields of a table
***************************************

Sometimes we might have some sensitive information in a table which we wouldn't want to expose.

Let's say, we want to expose the following ``author`` table without the fields ``email``, ``phone`` and ``address``:

.. code-block:: plpgsql

  author(id integer, name text, city text, email text, phone integer, address text)

A view that only exposes the non-sensitive fields of the ``author`` table can be created using the following SQL query:

.. code-block:: SQL

  CREATE VIEW author_public AS
    SELECT id, name, city
      FROM author
