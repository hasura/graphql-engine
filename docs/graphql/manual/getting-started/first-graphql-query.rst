.. meta::
   :description: Make a first GraphQL query with Hasura
   :keywords: hasura, docs, start, query, graphql

.. _first_graphql_query:

Making your first GraphQL query
===============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Let's create a sample table and query data from it using the Hasura console, a UI tool meant for doing exactly this:

Create a table
--------------

Let's add the following table:

.. code-block:: sql

  profile (
    id SERIAL PRIMARY KEY, -- serial -> auto-incrementing integer
    name TEXT
  )

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    Head to the Hasura console, navigate to ``Data -> Create table`` and create a sample table called ``profile`` with
    the following columns:

    .. thumbnail:: /img/graphql/manual/getting-started/create-profile-table.png
      :alt: Create a table

  .. tab:: Via CLI

    :ref:`Create a migration manually <manual_migrations>` and add the following statement to it:

    .. code-block:: sql

      CREATE TABLE profile(id serial NOT NULL, name text NOT NULL);

    Apply the migration by running:

    .. code-block:: bash

      hasura migrate apply

    To track the table and expose it over the GraphQL API, add it to the ``tables.yaml`` file in the ``metadata`` directory as follows:

    .. code-block:: yaml
       :emphasize-lines: 1-3

        - table:
            schema: public
            name: profile

    Apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: Via API

    Create a table by using the :ref:`run_sql metadata API <run_sql>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "run_sql",
        "args": {
          "sql": "CREATE TABLE profile(id serial NOT NULL, name text NOT NULL);"
        }
      }

    To track the table and expose it over the GraphQL API, make the following API call to the :ref:`track_table metadata API <track_table>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "track_table",
        "args": {
          "schema": "public",
          "name": "profile"
        }
      }


Now, insert some sample data into the table using the ``Insert Row`` tab of the ``profile`` table.

Try out a query
---------------

Head to the ``GraphiQL`` tab in the console and try running the following query:

.. code-block:: graphql

    query {
      profile {
        id
        name
      }
    }

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    .. thumbnail:: /img/graphql/manual/getting-started/profile-query.png
      :alt: Try out a query

  .. tab:: Via API

    .. code-block:: http

      POST /v1/graphql HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "query": "query { profile { id name }}"
      }

You'll see that you get all the inserted data!

Next steps
----------

Read more about:

- :ref:`Building your schema <schema>`
- :ref:`Queries <queries>`

