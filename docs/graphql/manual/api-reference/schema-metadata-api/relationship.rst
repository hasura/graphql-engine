.. meta::
   :description: Use relationships with the Hasura schema/metadata API
   :keywords: hasura, docs, schema/metadata API, API reference, relationship

.. _api_relationship:

Schema/Metadata API Reference: Relationships
============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

When retrieving data from tables, it is very helpful if we can also
fetch the related data alongside the columns. This is where relationships come
in. They can be considered as pseudo columns for a table to access the related
data.

For a simple ``article/author`` schema, the following relationships exist:

- ``author`` of an ``article``
- ``articles`` of an ``author``

There are two kinds of relationships:

- one-to-one or ``object relationships`` (e.g. ``author``).
- one-to-many or ``array relationships`` (e.g. ``articles``).

.. _create_object_relationship:

create_object_relationship
--------------------------

``create_object_relationship`` is used to create an object relationship on a
table. There cannot be an existing column or relationship with the same name. 

There are 2 ways in which you can create an object relationship.

1. Using foreign key constraint on a column
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an ``object relationship`` ``author`` on ``article`` *table*,  *using* the
*foreign_key_constraint_on* the ``author_id`` column:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

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


.. _manual_obj_relationship:

2. Manual configuration
^^^^^^^^^^^^^^^^^^^^^^^

This is an advanced feature which is mostly used to define relationships on or
to views. We cannot rely on foreign key constraints as they are not valid to or
from views. So, when using manual configuration, we have to specify the remote
table and how columns in this table are mapped to the columns of the
remote table. 

Let's say we have a view called ``article_detail`` which has three columns
``article_id`` and ``view_count`` and ``average_rating``. We can now define an
object relationship called ``article_detail`` on the ``article`` table as
follows: 
 
.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

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

   It is easy to make mistakes while using ``manual_configuration``.
   One simple check is to ensure that foreign key constraint semantics are valid
   on the columns being used in ``column_mapping``. In the previous example, if
   it was allowed, a foreign key constraint could have been defined on
   ``article`` table's ``id`` column to ``article_detail`` view's ``article_id``
   column.

.. _create_object_relationship_syntax:

Args syntax
^^^^^^^^^^^

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
     - Use one of the available ways to define an object relationship
   * - comment
     - false
     - text
     - comment

.. _ObjRelUsing:

ObjRelUsing
&&&&&&&&&&&

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

   There has to be at least one and only one of ``foreign_key_constraint_on``
   and ``manual_configuration``. 


ObjRelUsingManualMapping
&&&&&&&&&&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - remote_table
     - true
     - :ref:`TableName`
     - The table to which the relationship has to be established
   * - column_mapping
     - true
     - Object (:ref:`PGColumn` : :ref:`PGColumn`)
     - Mapping of columns from current table to remote table

.. _create_array_relationship:

create_array_relationship
-------------------------

``create_array_relationship`` is used to create an array relationship on a
table. There cannot be an existing column or relationship with the same name. 

There are 2 ways in which you can create an array relationship.

1. Using foreign key constraint on a column
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an ``array relationship`` ``articles`` on ``author`` *table*,  *using* the
*foreign_key_constraint_on* the ``author_id`` column of the ``article`` table:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

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


2. Manual configuration
^^^^^^^^^^^^^^^^^^^^^^^

This is an advanced feature which is mostly used to define relationships on or
to views. We cannot rely on foreign key constraints as they are not valid to or
from views. So, when using manual configuration, we have to specify the remote
table and how columns in this table are mapped to the columns of the
remote table.

Let's say we have a view called ``article_detail`` which has four columns
``author_id``, ``article_id``, ``view_count`` and ``average_rating``. We can now define an
array relationship called ``article_details`` on the ``author`` table as
follows:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "create_array_relationship",
       "args": {
           "table": "author",
           "name": "article_details",
           "using": {
               "manual_configuration" : {
                   "remote_table" : "article_detail",
                   "column_mapping" : {
                       "id" : "author_id"
                   }
               }
           }
       }
   }

.. note::

   It is easy to make mistakes while using ``manual_configuration``.
   One simple check is to ensure that foreign key constraint semantics are valid
   on the columns being used in ``column_mapping``. In the previous example, if
   it was allowed, a foreign key constraint could have been defined on the
   ``author`` table's ``id`` column to ``article_detail`` view's ``author_id``
   column.

.. _create_array_relationship_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - table
     - true
     - :ref:`TableName`
     - Name of the table
   * - name
     - true
     - :ref:`RelationshipName`
     - Name of the new relationship
   * - using
     - true
     - ArrRelUsing_
     - Use one of the available ways to define an array relationship
   * - comment
     - false
     - text
     - comment

.. _ArrRelUsing:

ArrRelUsing
&&&&&&&&&&&

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
   * - manual_configuration
     - false
     - ArrRelUsingManualMapping_
     - Manual mapping of table and columns

ArrRelUsingFKeyOn
&&&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - table
     - true
     - :ref:`TableName`
     - Name of the table
   * - column
     - true
     - :ref:`PGColumn`
     - Name of the column with foreign key constraint

ArrRelUsingManualMapping
&&&&&&&&&&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - remote_table
     - true
     - :ref:`TableName`
     - The table to which the relationship has to be established
   * - column_mapping
     - true
     - Object (:ref:`PGColumn` : :ref:`PGColumn`)
     - Mapping of columns from current table to remote table

.. _drop_relationship:

drop_relationship
-----------------

``drop_relationship`` is used to drop a relationship (both object and array) on
a table. If there are other objects dependent on this relationship like
permissions and query templates, etc., the request will fail and report the dependencies
unless ``cascade`` is set to ``true``. If ``cascade`` is set to ``true``, the
dependent objects are also dropped. 

An example:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "drop_relationship",
       "args": {
           "table": "article",
           "relationship": "article_detail"
       }
   }

.. _drop_relationship_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - table
     - true
     - :ref:`TableName`
     - Name of the table
   * - relationship
     - true
     - :ref:`RelationshipName`
     - Name of the relationship that needs to be dropped
   * - cascade
     - false
     - Boolean
     - When set to ``true``, all the dependent items on this relationship are also dropped

.. note::

   Be careful when using ``cascade``. First, try running the request without
   ``cascade`` or ``cascade`` set to ``false``.

.. _set_relationship_comment:

set_relationship_comment
------------------------

``set_relationship_comment`` is used to set/update the comment on a
relationship. Setting the comment to ``null`` removes it. 

An example:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "set_relationship_comment",
       "args": {
           "table": "article",
           "name": "article_detail",
           "comment" : "has extra information about an article like count etc."
       }
   }

.. _set_relationship_comment_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - table
     - true
     - :ref:`TableName`
     - Name of the table
   * - relationship
     - true
     - :ref:`RelationshipName`
     - The relationship
   * - comment
     - false
     - Text
     - Comment

.. _rename_relationship:

rename_relationship
-------------------

``rename_relationship`` is used to modify the name of an existing relationship.

An example:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "rename_relationship",
       "args": {
           "table": "article",
           "name": "article_details",
           "new_name": "article_detail"
       }
   }

.. _rename_relationship_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - table
     - true
     - :ref:`TableName`
     - Name of the table
   * - name
     - true
     - :ref:`RelationshipName`
     - The relationship
   * - new_name
     - true
     - :ref:`RelationshipName`
     - New relationship name
