.. .. meta::
   :description: Overview, description, and detailed API reference for creating and managing SQL relationships amongst tables along with examples.
   :keywords: hasura, docs, data, relationships


Data API Reference: Relationships
=================================

Relationships are used to capture the connectedness of data amongst tables. In a relational database, when modelling data we add foreign key constraints to establish connections between various tables.

Let's consider a simple scenario where there are two tables ``article`` and ``author``. The ``article`` table (child) has a column ``author_id`` which points to the ``id`` column of ``author`` table (parent). When we fetch a row from the article table, we may need the author information along with article columns. Similarly, when we fetch a row from the author table, we may need the articles written by the author along with the author columns.

In general, when retrieving data from tables, it is very helpful if we can also fetch the related data along side the columns. This is where relationships come in. They can be considered as pseudo columns for a table to access the related data. In the above example, the relationships that we can define are

1. ``author`` in ``article`` table
2. ``articles`` in ``author`` table

As you may have noticed, there are two kinds of relationships, object/many-to-one relationships (author) and array/one-to-many relationships (articles). From now on, we'll only use ``object`` and ``array`` relationships as terminology.

.. _obj_rel_example:

Let's define these relationships:

.. code-block:: http
   :emphasize-lines: 11

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if request needs particular user role

   {
       "type": "create_object_relationship",
       "args": {
           "table": "article",
           "name": "author",
           "using": {
               "foreign_key_constraint_on" : "author_id"
           }
       }
   }

This reads as follows:

Create an *object relationship* ``author`` on ``article`` *table*,  *using* the *foreign_key_constraint_on* the ``author_id`` column.

.. _arr_rel_example:

.. code-block:: http
   :emphasize-lines: 11-14

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if request needs particular user role

   {
       "type": "create_array_relationship",
       "args": {
           "table": "author",
           "name": "articles",
           "using": {
               "foreign_key_constraint_on" : {
                   "table" : "article",
                   "column" : "author_id"
               }
           }
       }
   }

Create an *array relationship* ``articles`` on ``author`` *table*,  *using* the *foreign_key_constraint_on* the ``author_id`` column of the ``article`` table.

The syntax is slightly different for creating an object and an array relationship. But, note that in both the cases, we are doing the same thing, i.e, specifying the column on which the foreign key constraint is defined.

.. _create_object_relationship:

create_object_relationship
--------------------------

``create_object_relationship`` is used to create an object relationship on a table. There cannot be an existing column or relationship with the same name.

There are 2 ways in which you can create an object relationship.

1. Using foreign key constraint on a column
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can look at :ref:`this <obj_rel_example>` example.

2. Manual configuration
^^^^^^^^^^^^^^^^^^^^^^^

This is an advanced feature which is mostly used to define relationships on or to views. We cannot rely on foreign key constraints as they are not valid to or from views. So, when using manual configuration, we have to specify the remote table and how each of the columns in this table are mapped to the columns of the remote table.

Let's say we have a view called ``article_detail`` which has three columns ``article_id`` and ``view_count`` and ``average_rating``. We can now define an object relationship called ``article_detail`` on the ``article`` table as follows:

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if request needs particular user role

   {
       "type": "create_object_relationship",
       "args": {
           "table": "article",
           "name": "article_detail",
           "using": {
               "manual_configuration" : {
                   "remote_table" : "article_detail",
                   "column_mapping" : {
                       "id" : "article_id"
                   }
               }
           }
       }
   }

.. note::
   It is easy to shoot yourself in the foot when using ``manual_configuration``. One simple check is to ensure that foreign key constraint semantics are valid on the columns being used in ``column_mapping``. In the previous example, if it was allowed, a foreign key constraint could have been defined on ``article`` table's ``id`` column to ``article_count`` view's ``article_id`` column.

Syntax
^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - table
     - true
     - :ref:`TableName <TableName>`
     - Name of the table
   * - name
     - true
     - :ref:`RelationshipName <RelationshipName>`
     - Name of the new relationship
   * - using
     - true
     - ObjRelUsing_
     - Use one of the available ways to define object relationship
   * - comment
     - false
     - text
     - comment

.. _ObjRelUsing:

``ObjRelUsing``
&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - foreign_key_constraint_on
     - false
     - :ref:`PGColumn <PGColumn>`
     - The column with foreign key constraint
   * - manual_configuration
     - false
     - ObjRelUsingManualMapping_
     - Manual mapping of table and columns

.. note::
   There has to be at least one and only one of ``foreign_key_constraint_on`` and ``manual_mapping``.


``ObjRelUsingManualMapping``
&&&&&&&&&&&&&&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - remote_table
     - true
     - :ref:`TableName <TableName>`
     - The table to which the relationship has to be established
   * - column_mapping
     - true
     - Object (:ref:`PGColumn <PGColumn>` : :ref:`PGColumn <PGColumn>`)
     - Mapping of columns from current table to remote table

.. _create_array_relationship:

create_array_relationship
-------------------------

``create_array_relationship`` is used to create an array relationship on a table. There cannot be an existing column or relationship with the same name.

There are 2 ways in which you can create an array relationship.

1. Using foreign key constraint on a column
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can look at :ref:`this <arr_rel_example>` example.

2. Manual configuration
^^^^^^^^^^^^^^^^^^^^^^^

This is an advanced feature which is mostly used to define relationships on or to views. We cannot rely on foreign key constraints as they are not valid to or from views. So, when using manual configuration, we have to specify the remote table and how each of the columns in this table are mapped to the columns of the remote table.

Syntax
^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - table
     - true
     - :ref:`TableName <TableName>`
     - Name of the table
   * - name
     - true
     - :ref:`RelationshipName <RelationshipName>`
     - Name of the new relationship
   * - using
     - true
     - ArrRelUsing_
     - Use one of the available ways to define array relationship
   * - comment
     - false
     - text
     - comment

.. _ArrRelUsing:

``ArrRelUsing``
&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - foreign_key_constraint_on
     - false
     - ArrRelUsingFKeyOn_
     - The column with foreign key constraint
   * - manual_mapping
     - false
     - ArrRelUsingManualMapping_
     - Manual mapping of table and columns

``ArrRelUsingFKeyOn``
&&&&&&&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - table
     - true
     - :ref:`TableName <TableName>`
     - Name of the table
   * - column
     - true
     - :ref:`PGColumn <PGColumn>`
     - Name of the column with foreign key constraint

``ArrRelUsingManualMapping``
&&&&&&&&&&&&&&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - remote_table
     - true
     - :ref:`TableName <TableName>`
     - The table to which the relationship has to be established
   * - column_mapping
     - true
     - Object (:ref:`PGColumn <PGColumn>` : :ref:`PGColumn <PGColumn>`)
     - Mapping of columns from current table to remote table

.. _drop_relationship:

drop_relationship
-----------------

``drop_relationship`` is used to drop a relationship (both object and array) on a table. If there are other objects dependent on this relationship like permissions and query templates, the query will fail reporting the dependencies unless ``cascade`` is set to ``true``. If ``cascade`` is set to ``true``, the dependent objects are also dropped.

An example:

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if request needs particular user role

   {
       "type": "drop_relationship",
       "args": {
           "table": "article",
           "relationship": "article_detail"
       }
   }


Syntax
^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - table
     - true
     - :ref:`TableName <TableName>`
     - Name of the table
   * - name
     - true
     - :ref:`RelationshipName <RelationshipName>`
     - Name of the relationship that needs to be dropped
   * - cascade
     - false
     - Boolean
     - When set to ``true``, all the dependent items on this relationship are also dropped

.. note:: Be careful when using ``cascade``. First, try running the query without ``cascade`` or ``cascade`` set to ``false``.

.. _set_relationship_comment:

set_relationship_comment
------------------------

``set_relationship_comment`` is used to set/update the comment on a relationship. Setting the comment to ``null`` removes it.

An example:

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: <role>  # optional. Required if request needs particular user role

   {
       "type": "set_relationship_comment",
       "args": {
           "table": "article",
           "name": "article_detail",
           "comment" : "has extra information about an article like count etc."
       }
   }

Syntax
^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - table
     - true
     - :ref:`TableName <TableName>`
     - Name of the table
   * - name
     - true
     - :ref:`RelationshipName <RelationshipName>`
     - The relationship
   * - comment
     - false
     - Text
     - comment
