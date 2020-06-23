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

:ref:`Actions <actions>` are a way to extend Hasura’s schema with custom business logic using custom queries and mutations. They can be used to call a REST endpoint from Hasura as a resolver for custom types. They are especially useful for setting up serverless functions as resolvers.

After you :ref:`create <create_actions>` or :ref:`derive <derive_actions>` an action, you can create relationships for it.

Step 1: Open the action relationship section
--------------------------------------------

- From your action, go to the ``Relationships`` tab.
- Click ``Add a relationship``.

.. thumbnail:: /img/graphql/manual/remote-joins/action-rel.png
   :alt: Opening the action relationship section
   :width: 559px

In this example, we're creating a relationship for the ``createUser`` action.

Step 2: Define the relationship
-------------------------------

In the section opened by the above step, fill out the following fields:

- **Relationship type**: Select a :ref:`type of relationship <relationship_database_modelling>`.

  - **Object relationship**: For one-to-one relationships.
  - **Array relationship**: For one-to-many relationships.

- **Relationship name**: Create a name for the relationship.
- **Reference schema**: Select a reference schema from your database.
- **Reference table**: Select a table from your database.
- **From**: Select a field returned in the action response.
- **To**: Select a column from the reference table to join the field to.

.. thumbnail:: /img/graphql/manual/remote-joins/define-action-rel.png
   :alt: Defining the relationship
   :width: 850px

In this example, we're creating a relationship called ``user``, from the ``id`` field returned in the action response, to the ``id`` column of the ``users`` table.

Step 3: Explore with GraphiQL
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

|

If your table has an existing remote relationship, here you can also get the fields from the remote schema.

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
