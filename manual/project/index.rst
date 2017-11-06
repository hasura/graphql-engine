.. meta::
   :description: Describing the hasura project directory structure
   :keywords: hasura, docs, CLI, HasuraCTL, hasuractl, hasuracli

.. _hasuractl-manual:

.. highlight:: bash

Hasura project
==============

..
.. todo::
   - What is a hasura project?: Source code and configuration for services, migrations, conf files in a mono-repo to make life easy
   - Hasura project directory structure: Get this from hasuractl/
   - Description of each folder, file. TOCTREE should mimic directory structure


A Hasura project is a *"git-able"* directory in the file system, which captures
all the information regarding clusters, services and migrations. It can also be
used to keep source code for custom services that you write.

You can `git push hasura master` to deploy everything, including your custom
microservices, database migrations and project configuration.

Let's understand the project directory in more detail.

.. toctree::
   :maxdepth: 1

   understanding-project

