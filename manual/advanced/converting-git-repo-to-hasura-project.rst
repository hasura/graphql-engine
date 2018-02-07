.. .. meta::
   :description: Any git repo can be made into a Hasura project by creating a particular folder and file structure. 
   :keywords: hasura, project, microservices, git, conf, migrations, directory

.. _convert-existing-git-repo:

Converting an existing git repo to a Hasura project
===================================================

A Hasura project is a collection of certain directories and files in a specific format. Hence, any git repo can be converted to a Hasura project just by recreating these directories and files.

Step 1: Clone `hasura/base`
-----------------------------

|base-github-link| is a blank Hasura project, with all the default configuration, but no migrations and no microservices. It is the single reference point for the structure of a Hasura project. Clone this repo to a particular location:

.. code-block:: bash

   $ git clone https://github.com/hasura/base

Step 2: Copy-paste from `base` to your git repo
-----------------------------------------------

Copy the following files and directories from ``base`` to the root of your own git repo:

- ``conf/``

  Copy this directory and all it's contents from ``base`` to the root of the repo. ``conf/`` directory contains ``yaml`` files which together define the configuration of a cluster on-to which the project will be deployed.

- ``migrations/``

  This directory is meant to hold all the database migrations required for the project. Make sure it is not empty by at least keeping a README.md, otherwise git will refuse to commit and the directory validation by CLI would fail.

- ``microservices/``

  Configuration and optionally source code for all microservices required by the project should be kept inside this directory. It should not be empty, keep at least a single file. ``base`` comes with a README and .gitkeep files so that the directory is always committed, even if there are no microservices.

- ``clusters.yaml``
  
  Details about the clusters added to the project is kept in this file. An empty file indicates that no clusters have been added to the project. But, the files should exist.

- ``hasura.yaml``

  This file defines some metadata about the project like name and compatible platform version.

These five elements define a Hasura project. The CLI will only deal with these files and directories, and ignores everything else in the repo. You are free to organize everything else according to your needs.

Step 3: Commit your changes
---------------------------

.. code-block:: bash

   $ git add conf/ migrations/ microservices/ clusters.yaml hasura.yaml
   $ git commit -m "add hasura project structure"

Step 4: Create a Hasura cluster and add to the project
------------------------------------------------------

.. code-block:: bash

   $ hasura cluster create --type=free
   $ hasura cluster add [cluster-name] -c hasura
   $ hasura cluster set-default hasura

Step 5: Add your SSH key and push to the cluster
------------------------------------------------

.. code-block:: bash

   $ hasura ssh-key add
   $ git push hasura master

.. |base-github-link| raw:: html

   <a href="https://github.com/hasura/base" target="_blank">hasura/base</a>

