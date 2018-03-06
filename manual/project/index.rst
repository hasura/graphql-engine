.. .. meta::
   :description: Describing the hasura project directory structure
   :keywords: hasura, docs, CLI, HasuraCTL, hasuractl, hasuracli

.. _hasura-project-manual:

.. highlight:: bash

Hasura project
==============

.. .. todo::
   - What is a hasura project?: Source code and configuration for microservices, migrations, conf files in a mono-repo to make life easy
   - Hasura project directory structure: Get this from hasuractl/
   - Description of each folder, file. TOCTREE should mimic directory structure


A Hasura project is a *"git-able"* directory in the file system, which captures
all the information regarding clusters, microservices and migrations. It can also be
used to keep source code for custom microservices that you write.
It follows a particular :doc:`directory structure <directory-structure/index>` to ensure everything works.

You can ``git push hasura master`` to deploy everything, including your custom
microservices, database migrations and project configuration.

.. toctree::
  :maxdepth: 1
  :titlesonly:

  create
  collaboration
  conf-files
  secrets
  directory-structure/index>
