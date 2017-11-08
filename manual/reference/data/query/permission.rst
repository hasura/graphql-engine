.. .. meta::
   :description: Learn how to protect your data by restricting access to users at a role level granularity using the console or using the Data microservice endpoint itself.
   :keywords: hasura, docs, data, permissions, data security, ACL, access control, safety


Permissions
===========

The permission layer is designed to restrict the operations that can be performed by various users. Permissions can be defined on various operations (insert/select/update/delete) at a role level granularity. By default, the admin role has unrestricted access to all operations.

It is highly recommended that you read about the session middleware on Hasura. To summarise, the data microservice gets the current user's (the one who made the request) information (id and role) along with the request. So, the data microservice applies the permissions as appropriate, for the operation(s) in the request.

.. _create_insert_permission:

create_insert_permission
------------------------

An insert permission is used to enforce constraints on the data that is being inserted.

Let's look at an example, a permission for the ``user`` role to insert into ``article`` table. What is the constraint that we would like to enforce here? *A user can only insert articles for herself*

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type" : "create_insert_permission",
       "args" : {
           "table" : "article",
           "role" : "user",
           "permission" : {
               "check" : {
                   "author_id" : "REQ_USER_ID"
               }
           }
       }
   }

This reads as follows:

"For the *user* role, for every row that is being inserted into the *article* table, *check* that the ``author_id`` column value is the same as the one who is making the request, ``REQ_USER_ID``"

``REQ_USER_ID`` is a special value that refers to the id of the user who made the request. It can only be used in place of a value of type integer/smallint/bigint in a boolean expression.

The argument for ``check`` is a boolean expression which has the same syntax as the ``where`` clause in the ``select`` query, making it extremely expressive. For example,

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type" : "create_insert_permission",
       "args" : {
           "table" : "article",
           "role" : "user",
           "permission" : {
               "check" : {
                   "author_id" : "REQ_USER_ID",
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

In the above definition, the row is allowed to be inserted if the ``author_id`` is the same as the request's user id and ``is_reviewed`` is ``false`` when the ``category`` is "editorial".

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
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - Role
   * - permission
     - true
     - InsertPermission_
     - The permission definition
   * - comment
     - false
     - text
     - comment

.. _InsertPermission:

``InsertPermission``
&&&&&&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - check
     - true
     - :ref:`BoolExp <BoolExp>`
     - This expression has to hold true for every new row that is inserted

.. _drop_insert_permission:

drop_insert_permission
----------------------

Drop an existing insert permission for a role on a table.

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
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - Role

.. _create_select_permission:

create_select_permission
------------------------

A select permission is used to restrict access to only the specified columns and rows.

Let's look at an example, a permission for the ``user`` role to select from ``article`` table: all columns can be read, and rows that have been published or authored by herself.

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type" : "create_select_permission",
       "args" : {
           "table" : "article",
           "role" : "user",
           "permission" : {
               "columns" : "*",
               "filter" : {
                   "$or" : [
                       { "author_id" : "REQ_USER_ID" },
                       { "is_published" : true }
                   ]
               }
           }
       }
   }

This reads as follows:

1. Allow all ``columns`` (because of ``*``).
2. Allow rows where ``is_published`` is ``true`` or the ``author_id`` is same as the one who is making the request, ``REQ_USER_ID``.

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
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - Role
   * - permission
     - true
     - SelectPermission_
     - The permission definition
   * - comment
     - false
     - text
     - comment

.. _SelectPermission:

``SelectPermission``
&&&&&&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - columns
     - true
     - :ref:`PGColumn <PGColumn>` array (or) ``'*'``
     - Only these columns are selectable (or all when ``'*'`` is specified)
   * - filter
     - true
     - :ref:`BoolExp <BoolExp>`
     - Only the rows where this expression holds true are selectable

.. _drop_select_permission:

drop_select_permission
----------------------

Drop an existing select permission for a role on a table.

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
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - Role

.. _create_update_permission:


create_update_permission
------------------------

An update permission is used to restrict the columns and rows that can be updated. Its structure is quite similar to the select permission.

An example:

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type" : "create_update_permission",
       "args" : {
           "table" : "article",
           "role" : "user",
           "permission" : {
               "columns" : ["title", "content", "category"],
               "filter" : {
                   "author_id" : "REQ_USER_ID"
               }
           }
       }
   }

This reads as follows:

1. Allow only the ``columns`` : ``title``, ``content`` and ``category`` to be updated
2. Allow rows where ``author_id`` is same as the one who is making the request, ``REQ_USER_ID`` to be updated.

.. note::
   It is important to deny updates to columns that will determine the row ownership. In the above example, ``author_id`` column determines the ownership of a row in the ``article`` table. Columns such as this should never be allowed to be updated.

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
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - Role
   * - permission
     - true
     - UpdatePermission_
     - The permission definition
   * - comment
     - false
     - text
     - comment

.. _UpdatePermission:

``UpdatePermission``
&&&&&&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - columns
     - true
     - :ref:`PGColumn <PGColumn>` array
     - Only these columns are updatable
   * - filter
     - true
     - :ref:`BoolExp <BoolExp>`
     - Only the rows where this expression holds true are deletable

.. _drop_update_permission:

drop_update_permission
----------------------

Drop an existing update permission for a role on a table.

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
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - Role

.. _create_delete_permission:

create_delete_permission
------------------------

A delete permission is used to restrict the rows that can be deleted.

An example:

.. code-block:: http

   POST data.<cluster-name>.hasura-app.io/v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: <admin-token>

   {
       "type" : "create_delete_permission",
       "args" : {
           "table" : "article",
           "role" : "user",
           "permission" : {
               "filter" : {
                   "author_id" : "REQ_USER_ID"
               }
           }
       }
   }

This reads as follows:

"``delete`` for ``user`` role on ``article`` table is allowed on rows where ``author_id`` is same as the one who is making the request, ``REQ_USER_ID``."

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
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - Role
   * - permission
     - true
     - DeletePermission_
     - The permission definition
   * - comment
     - false
     - text
     - comment

.. _DeletePermission:

``DeletePermission``
&&&&&&&&&&&&&&&&&&&&

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - filter
     - true
     - :ref:`BoolExp <BoolExp>`
     - Only the rows where this expression holds true are deletable

.. _drop_delete_permission:

drop_delete_permission
----------------------

Drop an existing delete permission for a role on a table.

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
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - Role

.. _set_permission_comment:

set_permission_comment
------------------------

``set_permission_comment`` is used to set/update the comment on a permission. Setting the comment to ``null`` removes it.

An example:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   Authorization: Bearer <admin-token>

   {
       "type": "set_permission_comment",
       "args": {
           "table": "article",
           "role": "user",
           "type" : "update",
           "comment" : "can only modify his/her own rows"
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
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - The role in the permission
   * - type
     - true
     - permission type (one of select/update/delete/insert)
     - The type of the permission
   * - comment
     - false
     - Text
     - comment
