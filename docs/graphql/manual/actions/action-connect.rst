.. meta::
   :description: Connecting Hasura actions with the graph
   :keywords: hasura, docs, actions, connect 

.. _actions_connect:

Connecting actions with the graph
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Use case
--------

Actions are a way to extend your GraphQL schema with custom queries or mutations. It
is a typical use case that the custom actions' response is actually related to
existing objects in the schema. e.g. a custom ``insertAuthor`` action will be
related to the ``author`` object in the schema. Hence, we would want to be able
to get information about the ``author`` from the graph as a response of the
``insertAuthor`` mutation.

Using custom object types relationships
---------------------------------------

Actions can be connected to the rest of the graph by setting up
:ref:`relationships <custom_object_type_relationships>` on its return output
type.

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

We can create an **object relationship** called ``updatedAuthor`` between the
``UpdateAuthorOutput`` object type and the ``author`` table via the
``UpdateAuthorOutput::author_id -> author::id`` fields.

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

Creating custom object type relationships
*****************************************

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

