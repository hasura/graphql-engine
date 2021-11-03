.. meta::
   :description: Rename Postgres relationships in Hasura
   :keywords: hasura, docs, postgres, schema, relationship, rename

.. _rename_relationships:

Postgres: Renaming relationships
================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

An existing relationship can be renamed as follows:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    - Head to ``Data -> [table-name] -> Relationships`` in the console
    - Drop the existing relationship
    - Recreate the relationship with the new name

  .. tab:: CLI

    You can rename a relationship by changing the relationship name in the ``tables.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml
       :emphasize-lines: 5

        - table:
            schema: public
            name: articles
          object_relationships:
          - name: author
            using:
              foreign_key_constraint_on: author_id
        - table:
            schema: public
            name: authors

    Apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: API

    You can rename a relationship by using the :ref:`rename_relationship metadata API <rename_relationship>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "rename_relationship",
        "args": {
          "table": "articles",
          "name": "article_details",
          "new_name": "article_detail"
        }
      }

.. note::

  You might not be allowed to drop a relationship if it has been referenced elsewhere (e.g. in a permissions rule).

  In this case you will have to delete the references first, rename the relationship, and then re-add the references.
