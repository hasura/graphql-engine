Writing migrations manually
===========================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

While the Hasura Console can auto generate migrations for every action,
sometimes you might want to write the migrations yourself, by hand. Using the
Hasura CLI, you can bootstrap these migration files and write the SQL for
Postgres Schema and YAML for Hasura metadata actions.

#. Set up the migration files:

   .. code-block:: bash

      hasura migrate create <name-of-migration>

   This command will create up and down migration yaml files in the
   ``migrations`` directory.

#. Edit the file and add your migration actions. For the file format and
   instructions on what actions can be added, refer to
   :doc:`../reference/migration-file-format`.

#. Apply the migration:

   .. code-block:: bash

      hasura migrate apply

