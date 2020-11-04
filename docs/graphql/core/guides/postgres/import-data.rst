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

It is possible to import a set of data from a ``csv`` file into a postgres table. 


Step 1: Add a table to your PG database
-----------------------------------------

Let's create a table called ``profile`` with the following set of fields:

.. code-block:: bash

  CREATE TABLE profile (firstName text, lastName text, email varchar);

Step 2: A CSV file that holds some data
-----------------------------------------

Let's assume you have a set of data you want to import into your PG database like the following:

.. thumbnail:: /img/graphql/core/guides/sample-data-csv-file.png
   :alt: .csv data file

.. note::

  Make sure the file is saved with the file extension ``.csv``   

Step 3: Connect to the postgres instance
------------------------------------------

You can connect to the postgres instance from ``psql`` on the command line like the following: 

.. code-block:: bash

   psql <postgres://kattykat:bG66KOrqxrVLkBIk8LSOR3b-lHwmfgUH@lallah.db.elephantsql.com:5432/kattykat>

Step 4: Copy the data successfully
-----------------------------------

Once you are connected successfully to the database/instance, use the following command in the psql terminal to
copy the data:

.. code-block:: bash

  \copy <tablename> from '/path/to/file/<filename>.csv' delimiter ',' CSV HEADER;

  ## for example

  \copy profile from '/Users/funmilayoolaiya/documents/profile.csv' delimiter ',' CSV HEADER;

Your data would have been successfully copied into a table in the database. 
