.. meta::
   :description: import data from csv in postgres
   :keywords: hasura, docs, postgres, import, data

.. _postgres_import_data_from_csv:

Import data from csv
=====================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

You can connect to a postgres instance from ``psql`` on the command line like the following: 

.. code-block:: bash

   psql 'postgres://asdad413143l/asdl12355/aws:344d/34314jf'


And once you are connected successfully to the database, use the following command in the psql terminal:

.. code-block:: bash

  \copy <tablename> from '/path/to/file/<filename>.csv' delimiter ',' CSV HEADER;
