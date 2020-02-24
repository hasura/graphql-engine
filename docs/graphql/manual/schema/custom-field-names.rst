.. meta::
   :description: Use custom field names in Hasura
   :keywords: hasura, docs, schema, custom field name

.. _custom_field_names:

Customise auto-generated field names
====================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

It is possible to override the auto-generated table and column field names exposed over the GraphQL API.

..  note::

  This feature is supported in versions ``v1.0.0-beta.8`` and later.

Expose columns with a different name in the GraphQL API
-------------------------------------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

     Head to the ``Data -> [table-name] -> Modify``

     .. thumbnail:: ../../../img/graphql/manual/schema/custom-field-name-column.png
        :alt: Customise GraphQL field name

  .. tab:: Metadata API

    A custom field name can be set for a column via the following 2 methods:

    - passing a :ref:`table_config` with the :ref:`CustomColumnNames` to the :ref:`track_table_v2` API while
      tracking a table
    - using the :ref:`set_table_custom_fields` API to set the :ref:`CustomColumnNames`


Expose table root fields with a different name in the GraphQL API
-----------------------------------------------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

     Head to the ``Data -> [table-name] -> Modify``

     .. thumbnail:: ../../../img/graphql/manual/schema/custom-field-name-root-fields.png
        :alt: Customise GraphQL root field

  .. tab:: Metadata API

    A custom field name can be set for a table root field via the following 2 methods:

    - passing a :ref:`table_config` with the :ref:`custom_root_fields` names to the :ref:`track_table_v2` API while
      tracking a table
    - using the :ref:`set_table_custom_fields` API to set the :ref:`custom_root_fields` names


