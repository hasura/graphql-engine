.. meta::
   :description: Adding a remote schema relationship with Hasura
   :keywords: hasura, docs, remote relationship, remote join, remote schema, data federation

.. _add_remote_relationship:

Creating remote relationships
=============================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Remote relationships extend the concept of joining data across tables, to joining across tables *and* remote data sources. Once you create relationships between types from your database and types created from APIs, you can then "join" them by running GraphQL queries.

These APIs can be custom GraphQL servers you write, 3rd party SaaS APIs, or even other Hasura instances.

Because Hasura is meant to be a GraphQL server that you can expose directly to your apps, Hasura also handles security and authorization while providing remote joins.

After you :ref:`add a remote schema <adding_schema>`, you can create relationships from your tables to the remote schema.

.. note::
  To see example use cases, check out this `blog post <https://hasura.io/blog/remote-joins-a-graphql-api-to-join-database-and-other-data-sources/>`__.


Step 1. Open the remote relationship section
--------------------------------------------

- From your table, go to the ``Relationships`` tab.
- Click the ``Add a remote relationship`` button.

.. thumbnail:: /img/graphql/manual/remote-joins/add.png
   :alt: Opening the remote relationship section

Step 2. Define the relationship
-------------------------------

In the section opened by the above step, fill out the following fields:

- **Name**: Create a name for the relationship.
- **Remote Schema**: Select a remote schema among all the ones you've created.
- **Configuration**: Set up the join configuration, to inject values as input arguments of the remote schema field.

  - **From column**: Input injected from table column values.
  - **From static value**: Input injected from a static value of your choice.

.. thumbnail:: /img/graphql/manual/remote-joins/define.png
   :alt: Defining the relationship
   :width: 800px

In this example, we've added a remote schema which is a wrapper around `Auth0 <https://auth0.com/>`__'s REST API (see example 
`here <https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/remote-schemas/auth0-wrapper>`__).

1. We name the relationship ``auth0_profile``.
2. We select the ``auth0`` schema that we've added.
3. We set up the config to join the ``auth0_id`` input argument of our remote schema field to the ``auth0_id`` column of this table (in this case, the ``users`` table).


Step 3. Explore with GraphiQL
-----------------------------

In the GraphiQL tab, test out your remote relationship.


.. graphiql::
  :view_only:
  :query:
    query {
      users {
        name
        auth0_profile {
          nickname
          email
          last_login
        }
      }
    }
  :response:
    {
      "data": {
        "users": [
          {
            "name": "Daenerys Targaryen",
            "auth0_profile": {
              "nickname": "Stormborn",
              "email": "mother.of.dragons@unburnt.com",
              "last_login": "2019-05-19T01:35:48.863Z"
            }
          }
        ]
      }
    }
