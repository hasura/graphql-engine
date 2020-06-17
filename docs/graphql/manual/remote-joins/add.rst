.. meta::
   :description: Adding a remote schema relationship with Hasura
   :keywords: hasura, docs, remote relationship, remote join, remote schema

.. _add_remote_relationship:

Adding a remote relationship
----------------------------

After you :ref:`add a remote schema <adding_schema>`, you can create relationships from your tables to the remote schema.

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Step 1. Open the remote relationship section
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- From your table, go to the "Relationships" tab.
- Click the "Add a remote relationship" button.

.. thumbnail:: /img/graphql/manual/remote-joins/add.png
   :alt: Opening the remote relationship section

Step 2. Define the relationship
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the section opened by the above step, fill out these fields and click "Save":

- **Name**: Create a name for the relationship.
- **Remote Schema**: Select a remote schema among all the ones you've created.
- **Configuration**: Set up the join configuration, to inject values as input arguments of the remote schema field.

  - **From column**: Input injected from table column values.
  - **From static value**: Input injected from a static value of your choice.

.. thumbnail:: /img/graphql/manual/remote-joins/define.png
   :alt: Defining the relationship
   :width: 650px

In this example, we've added a remote schema which is a wrapper around `Auth0 <https://auth0.com/>`__'s REST API (see example 
`here <https://github.com/tirumaraiselvan/auth0-graphql-server>`__).

1. We name the relationship ``auth0_profile``.
2. We select the ``auth0`` schema that we've added.
3. We set up the config to join the ``auth0_id`` input argument of our remote schema field to the ``auth0_id`` column of this table (in this case, the ``users`` table).


Step 3. Explore with GraphiQL
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
