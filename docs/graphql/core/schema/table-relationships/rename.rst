.. meta::
   :description: Rename relationships in Hasura
   :keywords: hasura, docs, schema, relationship, rename

.. _rename_relationships:

Renaming relationships
======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

To rename a relationship:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Via console

    - Head to ``Data -> [table-name] -> Relationships`` in the console
    - Drop the existing relationship
    - Recreate the relationship with the new name

  .. tab:: Via CLI

    You can rename a relationship by changing the relationship name in the ``tables.yaml`` file inside the ``metadata`` directory:

    .. code-block:: yaml
       :emphasize-lines: 5

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

    Apply the metadata by running:

    .. code-block:: bash

      hasura metadata apply

  .. tab:: Via API

    You can rename a relationship by using the :ref:`rename_relationship metadata API <rename_relationship>`:

    .. code-block:: http

      POST /v1/query HTTP/1.1
      Content-Type: application/json
      X-Hasura-Role: admin

      {
        "type": "rename_relationship",
        "args": {
          "table": "article",
          "name": "article_details",
          "new_name": "article_detail"
        }
      }

.. note::

  You might not be allowed to drop a relationship if it has been referenced elsewhere (e.g. in a permissions rule).

  In this case you will have to delete the references first, rename the relationship, and then re-add the references.
