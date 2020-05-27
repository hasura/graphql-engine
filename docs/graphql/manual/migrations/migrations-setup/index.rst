.. meta::
   :description: Migrations setup
   :keywords: hasura, docs, migration, setup

.. _migration_setup:

Migrations setup
================

If you don’t already use any tool to manage your Postgres schema, you can use Hasura to do that for you. 
Hasura has a CLI which will help you save each action that you do on the console, including creating tables/views and schema modifying SQL statements, as YAML files. 
These files are called migrations and they can be applied and rolled back step-by-step. These files can be version controlled and can be used with your CI/CD system to make incremental updates.

When you’re looking to set up migrations, there are two scenarios:

.. toctree::
  :maxdepth: 1

  Hasura is already set up <existing-hasura>
  Hasura is not set up yet <new-hasura>
