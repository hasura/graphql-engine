.. meta::
   :description: Set default field values for Postgres using role-based column presets
   :keywords: hasura, docs, postgres, schema, default value, role-based, column preset

.. _column_presets:

Postgres: Setting default values for fields using role-based column presets
===========================================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Let's say you want certain fields to have their values set automatically when not explicitly passed using session
variables or fixed values when a new row is created with a particular :ref:`user role <roles_variables>`.

Hasura GraphQL engine's column presets let you define role-based default values for any field/column. These values
can either be a session variable value or a static value.

.. admonition:: Column preset restricts mutation access for configured role

  If a column has a preset defined for a given role, access to the column for mutations will be restricted for users
  with that role.

**Example:** Say we have a field ``user_id`` in a table ``article`` which is to be set to the id of the user, from
the value of the user's session variable whenever a new row is added to the ``article`` table.

Step 1: Configure a column preset
---------------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    The column preset option is available under the ``Permissions`` tab of a table. Open the console and head to
    ``Data -> [article] -> Permissions``:

    .. thumbnail:: /img/graphql/core/schema/column-presets-option.png
      :alt: Add a column preset in the permissions tab

    Enable the column preset option to define presets for one or more columns. For each column, you can pick between
    setting the preset using a static value or from a session variable.

    .. thumbnail:: /img/graphql/core/schema/column-presets-value-options.png
      :alt: Configure the column preset

    For our chosen example, we'll use the ``from session variable`` option and configure the ``user_id`` column to be
    automatically populated based on the value of the ``X-Hasura-User-Id`` session variable.

  .. tab:: CLI

    You can set column presets in the ``tables.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml
       :emphasize-lines: 8-9

        - table:
            schema: public
            name: article
          insert_permissions:
          - role: user
            permission:
              check: {}
              set:
                user_id: x-hasura-User-Id
              columns:
              - content
              - rating
              - title
              backend_only: false

    Apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: API

    You can add column presets by using the :ref:`create_insert_permission metadata API <create_insert_permission>`:

    .. code-block:: http
      :emphasize-lines: 12-14

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type" : "create_insert_permission",
        "args" : {
          "table" : "article",
          "role" : "user",
          "permission" : {
            "check" : {},
            "set":{
              "user_id":"X-Hasura-User-Id"
            },
            "columns":["title","content", "rating"]
          }
        }
      }

.. note::

  To set a column preset for a nested object's column, simply set the corresponding column preset in the remote
  table.

Step 2: Run an insert mutation
------------------------------

Head to the GraphiQL interface in the console and try making an insert mutation on the ``article`` table with the
following headers (*to run through this example, don't forget to also grant the* ``user`` *role sufficient permissions
to select from the* ``article`` *table*):

- ``X-Hasura-Role`` --> ``user`` (*to test the behaviour for the configured role*)
- ``X-Hasura-User-Id`` --> ``1`` (*this is the value we should expect in the* ``user_id`` *field*)

As mentioned earlier, you'll notice when you add the ``X-Hasura-Role`` header that the field, ``user_id``, is no longer
available as the mutation type's field:

.. thumbnail:: /img/graphql/core/schema/column-preset-schema-change-for-role.png
  :alt: Write an insert mutation

Now, if we run the following insert mutation, we'll see that the ``user_id`` field is indeed being set with the value
passed in the ``X-Hasura-User-Id`` variable:

.. thumbnail:: /img/graphql/core/schema/column-preset-mutation-result.png
  :alt: Run the insert mutation

.. note::

  Not passing the configured header will result in a run-time error:
  
  .. code-block:: JSON

    {
        "errors": [
          {
            "path": "$",
            "error": "\"x-hasura-user-id\" header is expected but not found",
            "code": "not-found"
          }
        ]
    }


Also see
--------

- :ref:`postgres_defaults`
- :ref:`sql_functions_as_default`
