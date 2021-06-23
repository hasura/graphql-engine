.. meta::
   :description: Manage permissions with the Hasura metadata API
   :keywords: hasura, docs, metadata API, API reference, permission

.. _metadata_api_permission:

Metadata API Reference: Permissions (v2.0 and above)
====================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

The permission layer is designed to restrict the operations that can be
performed by various users. Permissions can be defined on various operations
(insert/select/update/delete) at a role level granularity. By default, the ``admin``
role has unrestricted access to all operations.

.. admonition:: Variables in rules

   All ``X-Hasura-*`` header values can be used in the permission rules. These
   values can come with the request and can be validated using webhook or can be
   sent with the JWT token.

.. admonition:: Supported from

  The metadata API is supported for versions ``v2.0.0`` and above and replaces the older
  :ref:`schema/metadata API <schema_metadata_apis>`.

.. _pg_create_insert_permission:

pg_create_insert_permission
---------------------------

An insert permission is used to enforce constraints on the data that is being
inserted.

Let's look at an example, a permission for the ``user`` role to insert into the
``article`` table. What is the constraint that we would like to enforce here? *A
user can only insert articles for themselves* .

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "pg_create_insert_permission",
       "args" : {
           "table" : "article",
           "source": "default",
           "role" : "user",
           "permission" : {
               "check" : {
                   "author_id" : "X-HASURA-USER-ID"
               },
               "set":{
                   "id":"X-HASURA-USER-ID"
               },
               "columns":["name","author_id"]
           }
       }
   }

This reads as follows - for the ``user`` role:

* For every row that is being inserted into the *article* table, allow insert only if the ``check`` passes i.e. that the ``author_id`` column value is the same as the value in the request header ``X-HASURA-USER-ID``".

* If the above ``check`` passes, then access for insert will be limited to columns ``name`` and ``author_id`` only.

* When this insert happens, the value of the column ``id`` will be automatically ``set`` to the value of the resolved session variable ``X-HASURA-USER-ID``.


The argument for ``check`` is a boolean expression which has the same syntax as the ``where`` clause in the ``select`` query, making it extremely expressive. 

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "pg_create_insert_permission",
       "args" : {
           "table" : "article",
           "source": "default",
           "role" : "user",
           "permission" : {
               "check" : {
                   "author_id" : "X-HASURA-USER-ID",
                   "$or" : [
                       {
                           "category" : "editorial",
                           "is_reviewed" : false
                       },
                       {
                           "category" : { "$neq" : "editorial"}
                       }
                   ]
               }
           }
       }
   }

In the above definition, the row is allowed to be inserted if the ``author_id``
is the same as the request's user id and ``is_reviewed`` is ``false`` when the
``category`` is "editorial".

.. _pg_create_insert_permission_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - permission
     - true
     - :ref:`InsertPermission`
     - The permission definition
   * - comment
     - false
     - text
     - Comment
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _pg_drop_insert_permission:

pg_drop_insert_permission
-------------------------

The ``pg_drop_insert_permission`` API is used to drop an existing insert permission for a role on a table.

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "pg_drop_insert_permission",
       "args" : {
           "table" : "article",
           "source": "default",
           "role" : "user"
       }
   }

.. _pg_drop_insert_permission_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _pg_create_select_permission:

pg_create_select_permission
---------------------------

A select permission is used to restrict access to only the specified columns and rows.

Let's look at an example, a permission for the ``user`` role to select from the
``article`` table: all columns can be read, as well as the rows that have been published or
authored by the user themselves.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "pg_create_select_permission",
       "args" : {
           "table" : "article",
           "role" : "user",
           "source": "default",
           "permission" : {
               "columns" : "*",
               "filter" : {
                   "$or" : [
                       { "author_id" : "X-HASURA-USER-ID" },
                       { "is_published" : true }
                   ]
                },
                "limit": 10,
                "allow_aggregations": true
           }
       }
   }

This reads as follows - For the ``user`` role:

* Allow selecting rows where the ``check`` passes i.e. ``is_published`` is ``true`` or the ``author_id`` matches the value of the session variable ``X-HASURA-USER-ID``.

* Allow selecting all columns (because the ``columns`` key is set to  ``*``).

* ``limit`` the numbers of rows returned by a query to the ``article`` table by the ``user`` role to a maximum of 10.

* Allow aggregate queries.

.. _pg_create_select_permission_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - permission
     - true
     - :ref:`SelectPermission`
     - The permission definition
   * - comment
     - false
     - text
     - Comment
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _pg_drop_select_permission:

pg_drop_select_permission
-------------------------

The ``pg_drop_select_permission`` is used to drop an existing select permission for a role on a table.

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "pg_drop_select_permission",
       "args" : {
           "table" : "article",
           "role" : "user",
           "source": "default"
       }
   }

.. _pg_drop_select_permission_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _pg_create_update_permission:

pg_create_update_permission
---------------------------

An update permission is used to restrict the columns and rows that can be
updated. Its structure is quite similar to the select permission.

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "pg_create_update_permission",
       "args" : {
           "table" : "article",
           "source": "default",
           "role" : "user",
           "permission" : {
               "columns" : ["title", "content", "category"],
               "filter" : {
                   "author_id" : "X-HASURA-USER-ID"
               },
               "check" : {
                   "content" : {
                     "_ne": ""
                   }
               },
               "set":{
                   "updated_at" : "NOW()"
               }
           }
       }
   }

This reads as follows - for the ``user`` role:

* Allow updating only those rows where the ``filter`` passes i.e. the value of the ``author_id`` column of a row matches the value of the session variable ``X-HASURA-USER-ID``.

* If the above ``filter`` passes for a given row, allow updating only the ``title``, ``content`` and ``category`` columns (*as specified in the* ``columns`` *key*).

* After the update happens, verify that the ``check`` condition holds for the updated row i.e. that the value in the ``content`` column is not empty.

* When this update happens, the value of the column ``updated_at`` will be automatically ``set`` to the current timestamp.

.. note::

   It is important to deny updates to columns that will determine the row
   ownership. In the above example, the ``author_id`` column determines the
   ownership of a row in the ``article`` table. Columns such as this should
   never be allowed to be updated.

.. _pg_create_update_permission_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - permission
     - true
     - :ref:`UpdatePermission`
     - The permission definition
   * - comment
     - false
     - text
     - Comment
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _pg_drop_update_permission:

pg_drop_update_permission
-------------------------

The ``pg_drop_update_permission`` API is used to drop an existing update permission for a role on a table.

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "pg_drop_update_permission",
       "args" : {
           "table" : "article",
           "source": "default",
           "role" : "user"
       }
   }

.. _pg_drop_update_permission_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _pg_create_delete_permission:

pg_create_delete_permission
---------------------------

A delete permission is used to restrict the rows that can be deleted.

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "pg_create_delete_permission",
       "args" : {
           "table" : "article",
           "source": "default",
           "role" : "user",
           "permission" : {
               "filter" : {
                   "author_id" : "X-HASURA-USER-ID"
               }
           }
       }
   }

This reads as follows:

"``delete`` for the ``user`` role on the ``article`` table is allowed on rows where
``author_id`` is the same as the request header ``X-HASURA-USER-ID`` value."

.. _pg_create_delete_permission_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - permission
     - true
     - :ref:`DeletePermission`
     - The permission definition
   * - comment
     - false
     - text
     - Comment
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _pg_drop_delete_permission:

pg_drop_delete_permission
-------------------------

The ``pg_drop_delete_permission`` API is used to drop an existing delete permission for a role on a table.

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "pg_drop_delete_permission",
       "args" : {
           "table" : "article",
           "role" : "user",
           "source": "default"
       }
   }

.. _pg_drop_delete_permission_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _pg_set_permission_comment:

pg_set_permission_comment
-------------------------

``pg_set_permission_comment`` is used to set/update the comment on a permission.
Setting the comment to ``null`` removes it.

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: admin

   {
       "type": "pg_set_permission_comment",
       "args": {
           "table": "article",
           "source": "default",
           "role": "user",
           "type" : "update",
           "comment" : "can only modify his/her own rows"
       }
   }

.. _pg_set_permission_comment_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - The role in the permission
   * - type
     - true
     - permission type (one of select/update/delete/insert)
     - The type of the permission
   * - comment
     - false
     - Text
     - Comment
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _mssql_create_insert_permission:

mssql_create_insert_permission
------------------------------

An insert permission is used to enforce constraints on the data that is being
inserted.

Let's look at an example, a permission for the ``user`` role to insert into the
``article`` table. What is the constraint that we would like to enforce here? *A
user can only insert articles for themselves* .

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "mssql_create_insert_permission",
       "args" : {
           "table" : "article",
           "source": "default",
           "role" : "user",
           "permission" : {
               "check" : {
                   "author_id" : "X-HASURA-USER-ID"
               },
               "set":{
                   "id":"X-HASURA-USER-ID"
               },
               "columns":["name","author_id"]
           }
       }
   }

This reads as follows - for the ``user`` role:

* For every row that is being inserted into the *article* table, allow insert only if the ``check`` passes i.e. that the ``author_id`` column value is the same as the value in the request header ``X-HASURA-USER-ID``".

* If the above ``check`` passes, then access for insert will be limited to columns ``name`` and ``author_id`` only.

* When this insert happens, the value of the column ``id`` will be automatically ``set`` to the value of the resolved session variable ``X-HASURA-USER-ID``.


The argument for ``check`` is a boolean expression which has the same syntax as the ``where`` clause in the ``select`` query, making it extremely expressive. 

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "mssql_create_insert_permission",
       "args" : {
           "table" : "article",
           "source": "default",
           "role" : "user",
           "permission" : {
               "check" : {
                   "author_id" : "X-HASURA-USER-ID",
                   "$or" : [
                       {
                           "category" : "editorial",
                           "is_reviewed" : false
                       },
                       {
                           "category" : { "$neq" : "editorial"}
                       }
                   ]
               }
           }
       }
   }

In the above definition, the row is allowed to be inserted if the ``author_id``
is the same as the request's user id and ``is_reviewed`` is ``false`` when the
``category`` is "editorial".

.. _mssql_create_insert_permission_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - permission
     - true
     - :ref:`InsertPermission`
     - The permission definition
   * - comment
     - false
     - text
     - Comment
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _mssql_drop_insert_permission:

mssql_drop_insert_permission
----------------------------

The ``mssql_drop_insert_permission`` API is used to drop an existing insert permission for a role on a table.

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "mssql_drop_insert_permission",
       "args" : {
           "table" : "article",
           "source": "default",
           "role" : "user"
       }
   }

.. _mssql_drop_insert_permission_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _mssql_create_select_permission:

mssql_create_select_permission
------------------------------

A select permission is used to restrict access to only the specified columns and rows.

Let's look at an example, a permission for the ``user`` role to select from the
``article`` table: all columns can be read, as well as the rows that have been published or
authored by the user themselves.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "mssql_create_select_permission",
       "args" : {
           "table" : "article",
           "role" : "user",
           "source": "default",
           "permission" : {
               "columns" : "*",
               "filter" : {
                   "$or" : [
                       { "author_id" : "X-HASURA-USER-ID" },
                       { "is_published" : true }
                   ]
                },
                "limit": 10,
                "allow_aggregations": true
           }
       }
   }

This reads as follows - For the ``user`` role:

* Allow selecting rows where the ``check`` passes i.e. ``is_published`` is ``true`` or the ``author_id`` matches the value of the session variable ``X-HASURA-USER-ID``.

* Allow selecting all columns (because the ``columns`` key is set to  ``*``).

* ``limit`` the numbers of rows returned by a query to the ``article`` table by the ``user`` role to a maximum of 10.

* Allow aggregate queries.

.. _mssql_create_select_permission_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - permission
     - true
     - :ref:`SelectPermission`
     - The permission definition
   * - comment
     - false
     - text
     - Comment
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _mssql_drop_select_permission:

mssql_drop_select_permission
----------------------------

The ``mssql_drop_select_permission`` is used to drop an existing select permission for a role on a table.

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "mssql_drop_select_permission",
       "args" : {
           "table" : "article",
           "role" : "user",
           "source": "default"
       }
   }

.. _mssql_drop_select_permission_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _mssql_create_update_permission:

mssql_create_update_permission
------------------------------

An update permission is used to restrict the columns and rows that can be
updated. Its structure is quite similar to the select permission.

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "mssql_create_update_permission",
       "args" : {
           "table" : "article",
           "source": "default",
           "role" : "user",
           "permission" : {
               "columns" : ["title", "content", "category"],
               "filter" : {
                   "author_id" : "X-HASURA-USER-ID"
               },
               "check" : {
                   "content" : {
                     "_ne": ""
                   }
               },
               "set":{
                   "updated_at" : "NOW()"
               }
           }
       }
   }

This reads as follows - for the ``user`` role:

* Allow updating only those rows where the ``filter`` passes i.e. the value of the ``author_id`` column of a row matches the value of the session variable ``X-HASURA-USER-ID``.

* If the above ``filter`` passes for a given row, allow updating only the ``title``, ``content`` and ``category`` columns (*as specified in the* ``columns`` *key*).

* After the update happens, verify that the ``check`` condition holds for the updated row i.e. that the value in the ``content`` column is not empty.

* When this update happens, the value of the column ``updated_at`` will be automatically ``set`` to the current timestamp.

.. note::

   It is important to deny updates to columns that will determine the row
   ownership. In the above example, the ``author_id`` column determines the
   ownership of a row in the ``article`` table. Columns such as this should
   never be allowed to be updated.

.. _mssql_create_update_permission_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - permission
     - true
     - :ref:`UpdatePermission`
     - The permission definition
   * - comment
     - false
     - text
     - Comment
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _mssql_drop_update_permission:

mssql_drop_update_permission
----------------------------

The ``mssql_drop_update_permission`` API is used to drop an existing update permission for a role on a table.

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "mssql_drop_update_permission",
       "args" : {
           "table" : "article",
           "source": "default",
           "role" : "user"
       }
   }

.. _mssql_drop_update_permission_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _mssql_create_delete_permission:

mssql_create_delete_permission
------------------------------

A delete permission is used to restrict the rows that can be deleted.

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "mssql_create_delete_permission",
       "args" : {
           "table" : "article",
           "source": "default",
           "role" : "user",
           "permission" : {
               "filter" : {
                   "author_id" : "X-HASURA-USER-ID"
               }
           }
       }
   }

This reads as follows:

"``delete`` for the ``user`` role on the ``article`` table is allowed on rows where
``author_id`` is the same as the request header ``X-HASURA-USER-ID`` value."

.. _mssql_create_delete_permission_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - permission
     - true
     - :ref:`DeletePermission`
     - The permission definition
   * - comment
     - false
     - text
     - Comment
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _mssql_drop_delete_permission:

mssql_drop_delete_permission
----------------------------

The ``mssql_drop_delete_permission`` API is used to drop an existing delete permission for a role on a table.

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "mssql_drop_delete_permission",
       "args" : {
           "table" : "article",
           "role" : "user",
           "source": "default"
       }
   }

.. _mssql_drop_delete_permission_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - Role
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)

.. _mssql_set_permission_comment:

mssql_set_permission_comment
----------------------------

``mssql_set_permission_comment`` is used to set/update the comment on a permission.
Setting the comment to ``null`` removes it.

An example:

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <auth-token> # optional if cookie is set
   X-Hasura-Role: admin

   {
       "type": "mssql_set_permission_comment",
       "args": {
           "table": "article",
           "source": "default",
           "role": "user",
           "type" : "update",
           "comment" : "can only modify his/her own rows"
       }
   }

.. _mssql_set_permission_comment_syntax:

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
   * - role
     - true
     - :ref:`RoleName`
     - The role in the permission
   * - type
     - true
     - permission type (one of select/update/delete/insert)
     - The type of the permission
   * - comment
     - false
     - Text
     - Comment
   * - source
     - false
     - :ref:`SourceName <SourceName>`
     - Name of the source database of the table (default: ``default``)
