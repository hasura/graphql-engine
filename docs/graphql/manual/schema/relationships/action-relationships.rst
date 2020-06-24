.. meta::
   :description: Adding an action relationship with Hasura
   :keywords: hasura, docs, action relationship, remote join

.. _add_action_relationship:

Creating action relationships
=============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

:ref:`Actions <actions>` are a way to extend Hasura’s schema with custom business logic using custom queries and mutations. The resolvers for these custom fields are written in REST endpoints. They are especially useful for setting up serverless functions as resolvers.

Step 1: Create an action
------------------------

First, create an action, either :ref:`from scratch <create_actions>` or :ref:`derive it from an existing mutation <derive_actions>`.

Step 2: Open the action relationship section
--------------------------------------------

- From your action, go to the ``Relationships`` tab.
- Click ``Add a relationship``.

.. thumbnail:: /img/graphql/manual/remote-joins/action-rel.png
   :alt: Opening the action relationship section
   :width: 559px

In this example, we're creating a relationship for the ``createUser`` action.

Step 3: Define the relationship
-------------------------------

The following values can be defined for an action relationship:

- **Relationship type**: Select a :ref:`type of relationship <relationship_database_modelling>`.

  - **Object relationship**: For one-to-one relationships.
  - **Array relationship**: For one-to-many relationships.

- **Relationship name**: Create a name for the relationship.
- **Reference schema**: Select a reference schema from your database.
- **Reference table**: Select a table from your database.
- **From**: Select a field returned in the action response.
- **To**: Select a column from the reference table to join the field to.

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    In the section opened by the above step, fill out the following fields:

    .. thumbnail:: /img/graphql/manual/remote-joins/define-action-rel.png
      :alt: Defining the relationship
      :width: 850px

  .. tab:: CLI

    You can add an action relationship by adding it to the respective custom type in the ``actions.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml
      :emphasize-lines: 4-13

      - custom_types
        - objects
          - name: UserOutput
            relationships:
            - remote_table:
                schema: public
                name: users
              name: user
              type: object
              field_mapping:
                id: id

    Apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: API

    You can create an action relationship when defining custom types via the :ref:`set_custom_types metadata API <set_custom_types>`:

    .. code-block:: http
      :emphasize-lines: 20-29

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "set_custom_types",
        "args": {
          "scalars": [],
          "enums": [],
          "input_objects": [],
          "objects": [
            {
              "name": "UserOutput",
              "fields": [
                {
                  "name": "id",
                  "type": "Int!"
                }
              ],
              "relationships": [
                {
                  "name": "user",
                  "type": "object",
                  "remote_table": "users",
                  "field_mapping": {
                    "id": "id"
                  }
                }
              ]
            }
          ]
        }
      }

In this example, we're creating a relationship called ``user``, from the ``id`` field returned in the action response, to the ``id`` column of the ``users`` table.

Step 4: Explore with GraphiQL
-----------------------------

In the GraphiQL tab, test out your action relationship.

.. graphiql::
  :view_only:
  :query:
    mutation {
      createUser(name: "Hodor") {
        id
        user {
          name
          auth0_id
        }
      }
    }
  :response:
    {
      "data": {
        "createUser": {
          "id": "7ffd68ba-535e-4c72-9051-17cd4e8ed594",
          "user": {
            "name": "Hodor",
            "auth0_id": "hodor|hodor"
          }
        }
      }
    }

If your table has an existing :ref:`remote relationship <add_remote_relationship>`, you can also query the fields from the remote schema.

.. graphiql::
  :view_only:
  :query:
    mutation {
      createUser(name: "Hodor") {
        id
        user {
          name
          auth0_id
          auth0_profile {
            email
            nickname
            last_login
          }          
        }
      }
    }
  :response:
    {
      "data": {
        "createUser": {
          "id": "7ffd68ba-535e-4c72-9051-17cd4e8ed594",
          "user": {
            "name": "Hodor",
            "auth0_id": "hodor|hodor",
            "auth0_profile": {
              "email": "hodor@hodor.com",
              "nickname": "Hodor",
              "last_login": "2016-05-22T01:35:48.863Z"
            }
          }
        }
      }
    }

In the :ref:`add_remote_relationship` section, we joined our ``users`` table with a remote `Auth0 <https://auth0.com/>`__ schema. Here, we're able to get the Auth0 profile data of the user returned from our action.
