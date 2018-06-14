Storing file metadata
======================

It is a common use-case to store extra information or metadata along with a file. For this, you
should create a custom table in the ``data`` microservice and use that to store the file metadata.

**To store file metadata:**

1. Create a table (e.g ``file_info``) in the data microservice. (refer :doc:`../data/create-table`)
2. Along with all the columns you want to add to the table, add a column called ``file_id``.
3. Whenever a successful file upload happens, enter a row about that file in your
   table. Use the ``file_id`` value returned by the File API to link the row to the uploaded file.
