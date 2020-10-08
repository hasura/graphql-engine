.. meta::
   :description: Track relationships in Hasura
   :keywords: hasura, docs, schema, relationship, track

.. _track_relationships:

Tracking relationships
======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

It is possible to have relationships in your underlying database without having them tracked and exposed over the GraphQL API. 
For example, this can be the case when you import a database schema into Hasura. This page explains how to track relationships and therefore expose them over the GraphQL API.

.. _track_single_relationships:

Tracking a single relationship
------------------------------

Assuming the relevant tables are tracked, but not the relationships, a single relationship can be tracked as follows:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    On the ``Data`` tab in the Hasura console, click the ``Track`` button for the relationships you'd like to track:

    .. thumbnail:: /img/graphql/core/schema/track-single-relationship.png
        :alt: Track single relationship
        :width: 700px

  .. tab:: CLI

    You can track relationships in the ``tables.yaml`` file inside the ``metadata`` directory:

    **Object relationship**

    .. code-block:: yaml
       :emphasize-lines: 4-7

        - table:
            schema: public
            name: article
          object_relationships:
          - name: author
            using:
              foreign_key_constraint_on: author_id
        - table:
            schema: public
            name: author

    **Array relationship**

    .. code-block:: yaml
      :emphasize-lines: 11-18

        - table:
            schema: public
            name: article
          object_relationships:
          - name: author
            using:
              foreign_key_constraint_on: author_id
        - table:
            schema: public
            name: author
          array_relationships:
          - name: articles
            using:
              foreign_key_constraint_on:
                column: author_id
                table:
                  schema: public
                  name: article

    Apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: API

    You can track an object relationship by using the :ref:`create_object_relationship metadata API <create_object_relationship>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "create_object_relationship",
        "args": {
          "table": "article",
          "name": "author",
          "using": {
            "foreign_key_constraint_on": "author_id"
          }
        }
      }

    You can track an array relationship by using the :ref:`create_array_relationship metadata API <create_array_relationship>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "create_array_relationship",
        "args": {
          "table": "author",
          "name": "articles",
          "using": {
            "foreign_key_constraint_on" : {
              "table" : "article",
              "column" : "author_id"
            }
          }
        }
      }

Tracking all relationships
--------------------------

Assuming the relevant tables are tracked, but not the relationships, all available relationships can be tracked as follows:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    On the ``Data`` tab in the Hasura console, click the button ``Track All``:

    .. thumbnail:: /img/graphql/core/schema/track-all-relationships.png
        :alt: Track all relationships
        :width: 700px

  .. tab:: CLI

    When working with the CLI, all relationships must be added manually, as described in the :ref:`previous section <track_single_relationships>`.
    You can use a script to automate this process.

  .. tab:: API

    When working with the API, all relationships must be added manually, as described in the :ref:`previous section <track_single_relationships>`.
    You can use a script to automate this process.
