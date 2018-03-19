.. .. meta::
   :description: Describing the hasura project directory structure
   :keywords: hasura, docs, CLI, HasuraCTL, hasuractl, hasuracli

.. _hasura-project-manual:

.. highlight:: bash

Hasura project
==============

A Hasura project is a *"git-able"* directory in the file system, which captures
all the information regarding clusters, microservices and migrations. It can also be
used to keep source code for custom microservices that you write.
It follows a particular :doc:`directory structure <directory-structure/index>` to ensure everything works.

A Hasura project can be deployed on a :doc:`Hasura cluster <../cluster/index>` and is composed of multiple :doc:`microservices <../microservices/index>` which run together.

You can ``git push hasura master`` to deploy everything, including your custom
microservices, database migrations and project configuration.

See:
^^^^

.. toctree::
  :maxdepth: 1
  :titlesonly:

  create
  deploy
  collaboration
  Conf files templating <conf-files-templating>
  Secrets <secrets/index>
  Directory structure <directory-structure/index>
