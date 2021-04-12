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

- Head to ``Data -> [table-name] -> Relationships`` in the console
- Drop the existing relationship
- Recreate the relationship with the new name

.. note::

  You might not be allowed to drop a relationship if it has been referenced elsewhere (e.g. in a permissions rule).

  In this case you will have to delete the references first, rename the relationship, and then re-add the references.
