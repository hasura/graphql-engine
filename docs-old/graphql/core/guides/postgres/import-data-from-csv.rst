.. meta::
   :description: import data from csv into postgres
   :keywords: hasura, docs, postgres, import, data

.. _postgres_import_data_from_csv:

Import data from CSV into Postgres
==================================

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

.. code-block:: sql

  profile (
    firstName TEXT, 
    lastName TEXT, 
    email VARCHAR
  )

Step 2: Connect to your Postgres database
-----------------------------------------

Connect to your Postgres database by using the ``psql`` command on the terminal: 

.. code-block:: bash

  psql postgres://<username>:<password>@<host>:<port>/<database>

  # for example
  psql postgres://postgres:postgres@localhost:5432/postgres

Step 3: Import the data from the CSV file
-----------------------------------------

Once connected to the database, use the following command from inside ``psql`` to
import the data:

.. code-block:: bash

  \copy <table_name> from '</path/to/file/filename.csv>' delimiter ',' CSV HEADER;

  # for example
  \copy profile from '/Users/sarahlewis/documents/profile.csv' delimiter ',' CSV HEADER;

Your data would have been successfully copied into the Postgres database. 