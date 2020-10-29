.. meta::
   :description: import data from csv in postgres
   :keywords: hasura, docs, postgres, import, data

.. _postgres_import_data_from_csv:

Import data from csv into database
===================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
-------------

CSV files are plain text files that makes it easy for any developer to create and add data. 
Also, the data are easy to import into any database. 

Step 1: Let's create a new CSV file that holds data
----------------------------------------------------

On the microsoft excel sheet, let's create a new file called ``dbhasura``. 
In the file, let us add a set of data with these fields below and with a table name called ``profile``.

.. code-block:: bash

   firstName, lastName, email

.. thumbnail:: /img/graphql/core/guides/sample-data-csv-file.png
   :alt: Add data to .csv file

.. note::

  Make sure the file is saved with the file extension ``.csv``   

Step 2: Connect to a postgres instance
---------------------------------------

You can connect to a postgres instance from ``psql`` on the command line like the following: 

.. code-block:: bash

   psql <postgres://asdad413143l/asdl12355/aws:344d/34314jf>

.. note::

  You can create any free postgres database instance. E.g elephantSQL, and grab the instance url.

Step 3: Copy the data successfully
-----------------------------------

Once you are connected successfully to the database, use the following command in the psql terminal:

.. code-block:: bash

  \copy <tablename> from '/path/to/file/<filename>.csv' delimiter ',' CSV HEADER;

  ## for instance 

  \copy profile from '/Users/funmilayoolaiya/documents/dbhasura.csv' delimiter ',' CSV HEADER;

Your data would have been successfully copied into a table in the database. 
