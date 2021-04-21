.. meta::
   :description: Manage actions with the Hasura schema/metadata API
   :keywords: hasura, docs, schema/metadata API, API reference, actions

.. _api_actions:

Schema/Metadata API Reference: Actions
======================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

**actions** are user defined mutations with custom business logic.

.. admonition:: Deprecation

  In versions ``v2.0.0`` and above, the schema/metadata API is deprecated in favour of the :ref:`schema API <schema_apis>` and the
  :ref:`metadata API <metadata_apis>`.

  Though for backwards compatibility, the schema/metadata APIs will continue to function.

.. _create_action:

create_action
-------------

``create_action`` is used to define an action. There shouldn't be an existing action with the same name.

Create a synchronous action with name ``create_user``:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
      "type":"create_action",
      "args":{
         "name":"create_user",
         "definition":{
            "kind":"synchronous",
            "arguments":[
               {
                  "name":"username",
                  "type":"String!"
               },
               {
                  "name":"email",
                  "type":"String!"
               }
            ],
            "output_type":"User",
            "handler":"https://action.my_app.com/create-user",
            "timeout":60
         },
         "comment": "Custom action to create user"
      }
   }


.. _create_action_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`ActionName <ActionName>`
     - Name of the action
   * - definition
     - true
     - :ref:`ActionDefinition`
     - Definition of the action
   * - comment
     - false
     - text
     - comment

.. _drop_action:

drop_action
-----------

``drop_action`` is used to remove an action. Permissions defined on the actions are also dropped automatically.

Drop an action ``create_user``:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
      "type":"drop_action",
      "args":{
         "name":"create_user",
         "clear_data": true
      }
   }

.. _drop_action_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`ActionName <ActionName>`
     - Name of the action
   * - clear_data
     - false
     - boolean
     - If set to ``true`` and action kind is ``asynchronous``, related data is deleted from catalog. (default: ``true``)

.. _update_action:

update_action
-------------

``update_action`` is used to update the definition of the action. Definition thus provided is
replaced with existing one.

Update an action ``create_user`` by making it's kind to ``asynchronous``:

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
      "type":"update_action",
      "args":{
         "name":"create_user",
         "definition":{
            "kind":"asynchronous",
            "arguments":[
               {
                  "name":"username",
                  "type":"String!"
               },
               {
                  "name":"email",
                  "type":"String!"
               }
            ],
            "output_type":"User",
            "handler":"https://action.my_app.com/create-user"
         },
         "comment": "Custom action to create user",
      }
   }


.. _update_action_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`ActionName <ActionName>`
     - Name of the action
   * - definition
     - true
     - :ref:`ActionDefinition`
     - Definition of the action to be replaced
   * - comment
     - false
     - text
     - comment

.. _create_action_permission:

create_action_permission
------------------------

``create_action_permission`` is used to define a permission to make action visible for a role.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
     "type": "create_action_permission",
     "args": {
       "action": "create_user",
       "role": "user"
     }
   }

.. _create_action_permission_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - action
     - true
     - :ref:`ActionName <ActionName>`
     - Name of the action
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - Name of the role
   * - comment
     - false
     - text
     - comment

.. _drop_action_permission:

drop_action_permission
----------------------

``drop_action_permission`` is used to drop a permission defined on an action.

.. code-block:: http

   POST /v1/query HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
     "type": "drop_action_permission",
     "args": {
       "action": "create_user",
       "role": "user"
     }
   }

.. _drop_action_permission_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - name
     - true
     - :ref:`ActionName <ActionName>`
     - Name of the action
   * - role
     - true
     - :ref:`RoleName <RoleName>`
     - Name of the role
