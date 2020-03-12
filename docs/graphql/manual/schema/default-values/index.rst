.. meta::
   :description: Set default field values in Hasura
   :keywords: hasura, docs, schema, default value

.. _default_field_values:

Setting default values for fields
=================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Let's say you want certain fields to have their values set automatically when not explicitly passed. You can do this in
the following ways:

- :ref:`Postgres defaults <postgres_defaults>`: configure default values, using fixed values or simple SQL functions,
  for columns in the table definition. E.g. an auto-incrementing ``id``, a ``created_at`` timestamp, etc.
- :ref:`Custom SQL functions <sql_functions_as_default>`: set up Postgres triggers which run custom SQL functions/stored procedures
  to set the values of certain columns on inserts/updates on the table. This is useful to set values of fields which
  depend on other fields passed in the input. e.g. set ``submission_time`` of an online quiz as 1 hour from the
  ``start_time``.
- :ref:`Role based column presets <column_presets>`: set up presets, using session variables or fixed values, that are
  applied when a new row is created with a particular :ref:`user role <roles_variables>`.
  E.g. set a ``user_id`` field automatically from a session variable/authorization header.

.. toctree::
  :maxdepth: 1
  :hidden:

  Postgres defaults <postgres-defaults>
  Custom SQL functions/stored procedures <sql-functions>
  Role based column presets <column-presets>

