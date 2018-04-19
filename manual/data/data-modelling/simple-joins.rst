============
Simple Joins
============

Sometimes, you would want to fetch data from multiple tables using a single query. In these situations, you should
create a view by joining the tables and query the view for the combined data.

Let's take a simple example where we have two tables ``student`` and ``student_batch_info``.

+----------------------------------------+-----------------------------------------------+
|Table                                   |Columns                                        |
+========================================+===============================================+
|student                                 |student_id, name                               |
+----------------------------------------+-----------------------------------------------+
|student_batch_info                      |student_id, batch_id, teacher_name, class_name |
+----------------------------------------+-----------------------------------------------+

As you can see above, both the tables have ``student_id`` as a common column. Now let's create a view to join all of this data:

.. code-block:: sql

     CREATE VIEW student_info_all AS
       SELECT s.student_id, s.name, sbi.teacher_name, sbi.class_name
       From student s, student_batch_info sbi
       WHERE s.student_id = sbi.student_id;

Head to *Data > SQL* section of the :doc:`API console <../../api-console/index>` and run the above SQL command.
Ensure that you check the ``Track Table`` checkbox before running the query so that you can use Data APIs to query the view.

Query the data
--------------

.. rst-class:: api_tabs
.. tabs::

   .. tab:: GraphQL

      .. code-block:: none

        query {
          student_info_all {
            student_id
            name
            teacher_name
            class_name
          }
        }

   .. tab:: JSON API

      .. code-block:: http

         POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
         Content-Type: application/json
         Authorization: Bearer <auth-token> # optional if cookie is set
         X-Hasura-Role: <role>  # optional. Required if only specific user role has access

         {
          "type": "select",
          "args": {
              "table": "student_extra_info",
              "columns": [
                  "student_id",
                  "name",
                  "teacher_name",
                  "class_name"
              ]
          }
         }
