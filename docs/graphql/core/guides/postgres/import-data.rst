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

CSV files are plain text files that makes it easy for any developer to create and add data. 
Also, the data are easy to import into any database. 

Step 1: Create a Postgres instance
------------------------------------

Create a postgres database and run the command below on the console/terminal to create a table callled ``profile``
with the following set of fields.

.. code-block:: bash

  CREATE TABLE profile (firstName text, lastName text, email varchar);

Step 2: Let's create a new CSV file that holds some data
---------------------------------------------------------

On the microsoft excel sheet, let's create a new file called ``profile``. 
In the file, let us add a set of data like below:

.. thumbnail:: /img/graphql/core/guides/sample-data-csv-file.png
   :alt: Add data to .csv file

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
