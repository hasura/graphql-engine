.. _postgres_schema_metadata:

Managing Postgres Schema and Hasura Metadata
============================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

If you don't already use any tool to manage your Postgres schema, you can use
Hasura to do that for you. Hasura has a CLI which will help you save each
action that you do on the console, including creating tables/views and schema
modifying SQL statements, as YAML files. These files are called migrations and
they can be applied and rolled back step-by-step. These files can be version
controlled and can be used with your CI/CD system to make incremental updates.

When you're looking to setup migrations, there are two scenarios:

#. :doc:`You already have a database and Hasura setup <existing-database>`.
#. :doc:`You're starting from scratch - an empty database and a fresh Hasura instance <new-database>`.

.. toctree::
  :maxdepth: 1
  :hidden:

  Hasura is already set up <existing-database>
  Hasura is not set up yet <new-database>
  
