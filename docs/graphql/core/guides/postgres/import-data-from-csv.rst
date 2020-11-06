.. meta::
   :description: import data from csv into postgres
   :keywords: hasura, docs, postgres, import, data

.. _postgres_import_data_from_csv:

Import data from CSV into Postgres
===================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

You might have existing data stored in a CSV file that you need to import into your Postgres database. The following
guide will show how to do so.

Let's assume we have the following CSV file, which is named ``profile.csv``:

.. thumbnail:: /img/graphql/core/guides/sample-data-csv-file.png
   :alt: .csv data file
   :width: 500px

Step 1: Add a corresponding table to your PG database
-----------------------------------------------------

Let us create a table to match the data structure in your CSV file.

On the Hasura console, on the Data tab, click on the SQL tab and add the following statement:

.. code-block:: bash

  CREATE TABLE profile (firstName text, lastName text, email varchar);

Step 2: Connect to your Postgres database
------------------------------------------

You can connect to your Postgres database using the ``psql`` command on the terminal like the following: 

.. code-block:: bash

  psql postgres://<postgres>:<postgres>@localhost:<port>/<database>

  ## for example

  psql <postgres://postgres:postgres@localhost:5432/user>

Step 3: Import the data from the CSV file
-----------------------------------------

Once connected to the database, use the following command from inside ``psql`` to
import the data:

.. code-block:: bash

  \copy <tablename> from '/path/to/file/<filename>.csv' delimiter ',' CSV HEADER;

  ## for example

  \copy profile from '/Users/sarahlewis/documents/profile.csv' delimiter ',' CSV HEADER;

Your data would have been successfully copied into a table in the Postgres database. 
