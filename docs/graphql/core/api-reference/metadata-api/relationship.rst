.. meta::
   :description: Use relationships with the Hasura metadata API
   :keywords: hasura, docs, metadata API, API reference, relationship

.. _metadata_api_relationship:

Metadata API Reference: Relationships (v2.0 and above)
======================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

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

The above represents the same table relationship from different perspectives:
there is a single ``author`` for every ``article`` (one-to-one), but there
may be multiple ``articles`` for every ``author`` (one-to-many).

A table relationship may be one-to-one from both perspectives. For
example, given tables ``author`` and ``author_details``, if the ``author_details``
table has a primary key ``author_id`` which is a foreign key to the
``author`` table's primary key ``id``. In this case there will be a single ``author``
for every ``author_details`` and a single ``details`` for every ``author``

.. admonition:: Supported from

  The metadata API is supported for versions ``v2.0.0`` and above and replaces the older
  :ref:`schema/metadata API <schema_metadata_apis>`.

.. _pg_create_object_relationship:

pg_create_object_relationship
-----------------------------

``create_object_relationship`` is used to create an object relationship on a
table. There cannot be an existing column or relationship with the same name. 

There are 3 ways in which you can create an object relationship.

1. Using foreign key constraint on a column
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an ``object relationship`` ``author`` on ``article`` *table*,  *using* the
*foreign_key_constraint_on* the ``author_id`` column:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "pg_create_object_relationship",
       "args": {
           "table": "article",
           "name": "author",
           "source": "default",
           "using": {
               "foreign_key_constraint_on" : ["author_id"]
           }
       }
   }

.. note::

  In the case that the key uses only a single column it is permissible to give
  just a string instead of a list, i.e.: ``"foreign_key_constraint_on" : "author_id"``.


2. Using foreign key constraint on a remote table
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an ``object relationship`` ``details`` on ``author`` *table*, *using* the
*foreign_key_constraint_on* the ``author_details`` *table*'s ``author_id`` *column*:

.. code-block:: http


   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "create_object_relationship",
       "args": {
           "table": "author",
           "name": "details",
           "using": {
               "foreign_key_constraint_on" : {
                   "table": "author_details",
                   "columns": ["author_id"]
               }
           }
       }
   }

.. admonition:: Deprecation

    For compatibility with previous versions we also support the form of
    ``foreign_key_constraint_on`` with a ``column``-field, e.g.:

    .. code-block:: json

      {
        "foreign_key_constraint_on" : {
          "table": "author_details",
          "column": "author_id"
        }
      }


    This form is deprecated in favor of the more general ``columns`` field.

.. admonition:: Supported from

    Relationships via remote table are supported for versions ``v2.0.0-alpha.3`` and above.

.. _pg_manual_obj_relationship:

3. Manual configuration
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

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "pg_create_object_relationship",
       "args": {
           "table": "article",
           "name": "article_detail",
           "source": "default",
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

.. _pg_create_object_relationship_syntax:

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
     - :ref:`ObjRelUsing`
     - Use one of the available ways to define an object relationship
   * - comment
     - false
     - text
     - comment
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _pg_create_array_relationship:

pg_create_array_relationship
----------------------------

``create_array_relationship`` is used to create an array relationship on a
table. There cannot be an existing column or relationship with the same name. 

There are 2 ways in which you can create an array relationship.

1. Using foreign key constraint on a column
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an ``array relationship`` ``articles`` on ``author`` *table*,  *using* the
*foreign_key_constraint_on* the ``author_id`` column of the ``article`` table:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "pg_create_array_relationship",
       "args": {
           "table": "author",
           "name": "articles",
           "source": "default",
           "using": {
               "foreign_key_constraint_on" : {
                   "table" : "article",
                   "columns" : ["author_id"]
               }
           }
       }
   }

.. admonition:: Deprecation

    For compatibility with previous version we also support the form of
    ``foreign_key_constraint_on`` with a ``column``-field, e.g.:

    .. code-block:: json

      {
        "foreign_key_constraint_on" : {
          "table": "author_details",
          "column": "author_id"
        }
      }

    This form is deprecated in favor of the more general ``columns`` field.


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

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "pg_create_array_relationship",
       "args": {
           "table": "author",
           "name": "article_details",
           "source": "default",
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

.. _pg_create_array_relationship_syntax:

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
     - :ref:`ArrRelUsing`
     - Use one of the available ways to define an array relationship
   * - comment
     - false
     - text
     - comment
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _pg_drop_relationship:

pg_drop_relationship
--------------------

``pg_drop_relationship`` is used to drop a relationship (both object and array) on
a table. If there are other objects dependent on this relationship like
permissions and query templates, etc., the request will fail and report the dependencies
unless ``cascade`` is set to ``true``. If ``cascade`` is set to ``true``, the
dependent objects are also dropped. 

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "pg_drop_relationship",
       "args": {
           "table": "article",
           "source": "default",
           "relationship": "article_detail"
       }
   }

.. _pg_drop_relationship_syntax:

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
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. note::

   Be careful when using ``cascade``. First, try running the request without
   ``cascade`` or ``cascade`` set to ``false``.

.. _pg_set_relationship_comment:

pg_set_relationship_comment
---------------------------

``pg_set_relationship_comment`` is used to set/update the comment on a
relationship. Setting the comment to ``null`` removes it.

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "pg_set_relationship_comment",
       "args": {
           "table": "article",
           "source": "default",
           "name": "article_detail",
           "comment" : "has extra information about an article like count etc."
       }
   }

.. _pg_set_relationship_comment_syntax:

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
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)


.. _pg_rename_relationship:

pg_rename_relationship
----------------------

``pg_rename_relationship`` is used to modify the name of an existing relationship.

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type": "pg_rename_relationship",
       "args": {
           "table": "article",
           "name": "article_details",
           "source": "default",
           "new_name": "article_detail"
       }
   }

.. _pg_rename_relationship_syntax:

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
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _mssql_create_object_relationship:

mssql_create_object_relationship
--------------------------------

``create_object_relationship`` is used to create an object relationship on a
table. There cannot be an existing column or relationship with the same name. 

There are 3 ways in which you can create an object relationship.

1. Using foreign key constraint on a column
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an ``object relationship`` ``author`` on ``article`` *table*,  *using* the
*foreign_key_constraint_on* the ``author_id`` column:

.. code-block:: http

  POST /v1/metadata HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
      "type": "mssql_create_object_relationship",
      "args": {
          "table": "article",
          "name": "author",
          "source": "default",
          "using": {
              "foreign_key_constraint_on" : ["author_id"]
          }
      }
  }

.. note::

  In the case that the key uses only a single column it is permissible to give
  just a string instead of a list, i.e.: ``"foreign_key_constraint_on" : "author_id"``.


2. Using foreign key constraint on a remote table
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an ``object relationship`` ``details`` on ``author`` *table*, *using* the
*foreign_key_constraint_on* the ``author_details`` *table*'s ``author_id`` *column*:

.. code-block:: http


  POST /v1/metadata HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
      "type": "create_object_relationship",
      "args": {
          "table": "author",
          "name": "details",
          "using": {
              "foreign_key_constraint_on" : {
                  "table": "author_details",
                  "columns": ["author_id"]
              }
          }
      }
  }

.. admonition:: Deprecation

    For compatibility with previous versions we also support the form of
    ``foreign_key_constraint_on`` with a ``column``-field, e.g.:

    .. code-block:: json

      {
        "foreign_key_constraint_on" : {
            "table": "author_details",
            "column": "author_id"
        }
      }

    This form is deprecated in favor of the more general ``columns`` field.

.. admonition:: Supported from

    Relationships via remote table are supported for versions ``v2.0.0-alpha.3`` and above.

.. _mssql_manual_obj_relationship:

3. Manual configuration
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

  POST /v1/metadata HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
      "type": "mssql_create_object_relationship",
      "args": {
          "table": "article",
          "name": "article_detail",
          "source": "default",
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

.. _mssql_create_object_relationship_syntax:

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
    - :ref:`ObjRelUsing`
    - Use one of the available ways to define an object relationship
  * - comment
    - false
    - text
    - comment
  * - source
    - false
    - :ref:`SourceName <SourceName>`
    - Name of the source database of the table (default: ``default``)

.. _mssql_create_array_relationship:

mssql_create_array_relationship
-------------------------------

``create_array_relationship`` is used to create an array relationship on a
table. There cannot be an existing column or relationship with the same name. 

There are 2 ways in which you can create an array relationship.

1. Using foreign key constraint on a column
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an ``array relationship`` ``articles`` on ``author`` *table*,  *using* the
*foreign_key_constraint_on* the ``author_id`` column of the ``article`` table:

.. code-block:: http

  POST /v1/metadata HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
      "type": "mssql_create_array_relationship",
      "args": {
          "table": "author",
          "name": "articles",
          "source": "default",
          "using": {
              "foreign_key_constraint_on" : {
                  "table" : "article",
                  "columns" : ["author_id"]
              }
          }
      }
  }

.. admonition:: Deprecation

    For compatibility with previous version we also support the form of
    ``foreign_key_constraint_on`` with a ``column``-field, e.g.:

    .. code-block:: json

      {
       "foreign_key_constraint_on" : {
           "table": "author_details",
           "column": "author_id"
       }
      }
      
    This form is deprecated in favor of the more general ``columns`` field.


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

  POST /v1/metadata HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
      "type": "mssql_create_array_relationship",
      "args": {
          "table": "author",
          "name": "article_details",
          "source": "default",
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

.. _mssql_create_array_relationship_syntax:

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
    - :ref:`ArrRelUsing`
    - Use one of the available ways to define an array relationship
  * - comment
    - false
    - text
    - comment
  * - source
    - false
    - :ref:`SourceName <SourceName>`
    - Name of the source database of the table (default: ``default``)

.. _mssql_drop_relationship:

mssql_drop_relationship
-----------------------

``mssql_drop_relationship`` is used to drop a relationship (both object and array) on
a table. If there are other objects dependent on this relationship like
permissions and query templates, etc., the request will fail and report the dependencies
unless ``cascade`` is set to ``true``. If ``cascade`` is set to ``true``, the
dependent objects are also dropped. 

An example:

.. code-block:: http

  POST /v1/metadata HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
      "type": "mssql_drop_relationship",
      "args": {
          "table": "article",
          "source": "default",
          "relationship": "article_detail"
      }
  }

.. _mssql_drop_relationship_syntax:

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
  * - source
    - false
    - :ref:`SourceName <SourceName>`
    - Name of the source database of the table (default: ``default``)

.. note::

  Be careful when using ``cascade``. First, try running the request without
  ``cascade`` or ``cascade`` set to ``false``.

.. _mssql_set_relationship_comment:

mssql_set_relationship_comment
------------------------------

``mssql_set_relationship_comment`` is used to set/update the comment on a
relationship. Setting the comment to ``null`` removes it.

An example:

.. code-block:: http

  POST /v1/metadata HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
      "type": "mssql_set_relationship_comment",
      "args": {
          "table": "article",
          "source": "default",
          "name": "article_detail",
          "comment" : "has extra information about an article like count etc."
      }
  }

.. _mssql_set_relationship_comment_syntax:

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
  * - source
    - false
    - :ref:`SourceName <SourceName>`
    - Name of the source database of the table (default: ``default``)

.. _mssql_rename_relationship:

mssql_rename_relationship
-------------------------

``mssql_rename_relationship`` is used to modify the name of an existing relationship.

An example:

.. code-block:: http

  POST /v1/metadata HTTP/1.1
  Content-Type: application/json
  X-Hasura-Role: admin

  {
      "type": "mssql_rename_relationship",
      "args": {
          "table": "article",
          "name": "article_details",
          "source": "default",
          "new_name": "article_detail"
      }
  }

.. _mssql_rename_relationship_syntax:

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
  * - source
    - false
    - :ref:`SourceName <SourceName>`
    - Name of the source database of the table (default: ``default``)
