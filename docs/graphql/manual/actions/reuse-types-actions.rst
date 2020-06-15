.. meta::
   :description: Reusing existing types with actions
   :keywords: hasura, docs, actions, connect, existing types

.. _reuse_types_actions:

Reusing existing types with actions
===================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Use case
--------

Actions are a way to extend your GraphQL schema with custom queries or mutations. It
is quite a typical case that an action's response is actually related to
existing objects in the schema and the action needs to be connected with the rest of
the graph.

For example, a custom ``insertAuthor`` action will be
related to the ``author`` object in the schema. Hence, we would want to be able
to get information about the ``author`` from the graph as a response of the
``insertAuthor`` mutation.

Using action output type's relationships
----------------------------------------

Actions can be connected to the rest of the graph by setting up relationships on
its return output type.

This allows complex responses to be returned as an action's response traversing
the graph via the output type's relationships.

**For example**, given the action:

.. code-block:: graphql

  type Mutation {
    updateAuthor (
      id: Int!
      name: String!
    ): UpdateAuthorOutput
  }

  type UpdateAuthorOutput {
     author_id : Int!
  }

We can create an object relationship called ``updatedAuthor`` between the
``UpdateAuthorOutput`` object type and the ``author`` table using the
``UpdateAuthorOutput.author_id`` and  ``author.id`` fields.

The object type will now be modified as:

.. code-block:: graphql
  :emphasize-lines: 3

    type UpdateAuthorOutput {
       author_id : Int!
       updatedAuthor: author
    }

Now we can make a mutation request with a complex response such as:

.. code-block:: graphql

  mutation updateAuthorAndGetArticles($id: Int, $name: String) {
    updateAuthor(id: $id, name: $name) {
      author_id
      updatedAuthor {
        id
        name
        articles {
          id
          title
        }
      }
    }
  }

See more details at :ref:`custom object type relationships <custom_object_type_relationships>`

Creating relationships for custom object types
**********************************************

You can create relationships for custom output types by:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

     Head to the ``Actions -> [action-name] -> Relationships`` tab in the
     console for the action returning the output type.

     Set the output type relationship as shown below:

     .. thumbnail:: /img/graphql/manual/actions/actions-relationship.png
        :alt: Console action relationship

     Hit ``Save`` to create the relationship.

  .. tab:: CLI

     Go to ``metadata/actions.yaml`` in the Hasura project directory.

     Update the definition of the ``UpdateAuthorOutput`` object type as:

     .. code-block:: yaml
       :emphasize-lines: 4-11

       - custom_types
         - objects
           - name: UpdateAuthorOutput
             relationships:
             - name: updatedAuthor
               type: object
               remote_table:
                 schema: public
                 name: author
               field_mapping:
                 author_id: id


     Save the changes and run ``hasura metadata apply`` to create the relationship.

  .. tab:: Via API

    Action relationships can be added while defining custom types for an action via the :ref:`set_custom_types metadata API <set_custom_types>`:

    .. code-block:: http

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
              "name": "UpdateAuthorOutput",
              "fields": [
                {
                  "name": "author_id",
                  "type": "Int!"
                }
              ],
              "relationships": [
                {
                  "name": "updatedAuthor",
                  "type": "object",
                  "remote_table": "author",
                  "field_mapping": {
                    "author_id": "id"
                  }
                }
              ]
            }
          ]
        }
      }

    Once the custom types with relationships are defined, we can create an action via the :ref:`create_action metadata API <create_action>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "create_action",
        "args": {
          "name": "updateAuthor",
          "definition": {
            "kind": "synchronous",
            "arguments": [
              {
                "name": "username",
                "type": "String!"
              },
              {
                "name": "email",
                "type": "String!"
              }
            ],
            "output_type": "UpdateAuthorOutput",
            "handler": "https://action.my_app.com/create-user"
          }
        }
      }
