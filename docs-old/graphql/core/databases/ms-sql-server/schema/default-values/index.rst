.. meta::
   :description: Set default field values for MS SQL Server in Hasura
   :keywords: hasura, docs, ms sql server, schema, default value

.. _mssql_default_field_values:

MS SQL Server: Setting default values for fields
================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Let's say you want certain fields to have their values set automatically when not explicitly passed. You can do this in
the following ways:

- :ref:`MS SQL Server defaults <mssql_defaults>`: configure default values, using fixed values or simple SQL functions,
  for columns in the table definition. E.g. an auto-incrementing ``id``, a ``created_at`` timestamp, etc.
- :ref:`Role based column presets <mssql_column_presets>`: set up presets, using session variables or fixed values, that are
  applied when a row is created/updated with a particular :ref:`user role <roles_variables>`.
  E.g. set a ``user_id`` field automatically from a session variable/authorization header.

.. toctree::
  :maxdepth: 1
  :hidden:

  MS SQL Server defaults <mssql-defaults>
  Role based column presets <mssql-column-presets>
