.. meta::
   :description: Permissions for Hasura actions
   :keywords: hasura, docs, actions, permissions

.. _actions_permissions:

Actions permissions
===================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

As with the other fields in the GraphQL schema, users need to be
given access to an action.

Set action permissions
----------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

     Head to the ``Actions -> [action-name] -> Permissions`` tab in the
     console.

     .. thumbnail:: /img/graphql/manual/actions/actions-permissions.png
        :alt: Console action permission

     Hit ``Save`` to give the role permission to access the action.

  .. tab:: CLI

     Go to ``metadata/actions.yaml`` in the Hasura project directory.

     Update the definition of the ``insertAuthor`` action as:

     .. code-block:: yaml
       :emphasize-lines: 6-8

         - actions
           - name: insertAuthor
             definition:
               kind: synchronous
               handler: '{{ACTIONS_BASE_URL}}/insertAuthor'
             permissions:
             - role: user
             - role: publisher

     Save the changes and run ``hasura metadata apply`` to set the
     permissions.
