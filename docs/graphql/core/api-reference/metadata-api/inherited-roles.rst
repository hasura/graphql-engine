.. meta::
   :description: Manage inherited roles with the Hasura metadata API
   :keywords: hasura, docs, metadata API, API reference, inherited roles, multiple roles

.. _metadata_inherited_role:

Metadata API Reference: Inherited Roles
=======================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Inherited roles allow you to create a role which inherits permissions from other existing roles.

.. admonition:: Supported from

  The metadata API is supported for versions ``v2.0.0`` and above and replaces the older
  :ref:`schema/metadata API <schema_metadata_apis>`.

.. _metadata_add_inherited_role:

add_inherited_role
------------------

``add_inherited_role`` is used to create a new inherited role with other existing roles.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
      "type":"add_inherited_role",
      "args":{
         "role_name": "sample_inherited_role",
         "role_set": [
            "role1",
            "role2"
         ]
      }
   }

.. _metadata_add_inherited_role_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - role_name
     - true
     - :ref:`RoleName`
     - Name of the inherited role
   * - role_set
     - true
     - [:ref:`RoleName`]
     - List of non-inherited roles from which permissions should be inherited

.. _metadata_drop_inherited_role:

drop_inherited_role
-------------------

``drop_inherited_role`` is used to delete an existing inherited role.

.. code-block:: http

   POST /v1/metadata HTTP/1.1
   Content-Type: application/json
   X-Hasura-Role: admin

   {
       "type" : "drop_inherited_role",
       "args" : {
          "role": "sample_inherited_role"
       }
   }

.. _metadata_drop_inherited_role_syntax:

Args syntax
^^^^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Key
     - Required
     - Schema
     - Description
   * - role
     - true
     - :ref:`RoleName`
     - Name of the inherited role
