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
-------------

You might have existing data stored in a CSV file that you need to import into your Postgres database. The following
guide will show how to do so.

In this guide we will use the following CSV file as an example. 

Also, make sure the file is saved as ``profile.csv``.

.. thumbnail:: /img/graphql/core/guides/sample-data-csv-file.png
   :alt: .csv data file

Step 1: Add a table to your PG database
-----------------------------------------

Let us create a table to match the data structure in your CSV file.

For example:

.. code-block:: bash

  CREATE TABLE profile (firstName text, lastName text, email varchar);

Step 2: Connect to the postgres database
------------------------------------------

You can connect to the postgres database using the ``psql`` command on the terminal like the following: 

.. code-block:: bash

  psql 'postgres://<postgres>:<postgres>@localhost:<port>/<database>'

  ## for example

  psql <postgres://postgres:postgres@localhost:5432/kittykat>

Step 3: Copy the data successfully
-----------------------------------

Once you are connected successfully to the database, use the following command in the psql terminal to
copy the data:

.. code-block:: bash

  \copy <tablename> from '/path/to/file/<filename>.csv' delimiter ',' CSV HEADER;

  ## for example

  \copy profile from '/Users/sarahlewis/documents/profile.csv' delimiter ',' CSV HEADER;

Your data would have been successfully copied into the ``profile`` table in the PG database. 
