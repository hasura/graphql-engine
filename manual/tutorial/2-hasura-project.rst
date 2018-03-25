.. .. meta::
   :description: Part 2 of a set of learning exercises meant for exploring Hasura in detail. This part introduces the Auth microservice's User & Session management model.
   :keywords: hasura, getting started, step 2

================================
Part II: Create a Hasura project
================================

A Hasura project is a folder (representing a code repo) that contains all the configuration files, the database migrations and the source code and configuration for your custom microservices. This project folder should be a git repo, so that you can `git push hasura master` to deploy everything in the repo to the cluster.


Create a 'base' project
-----------------------

Run the following command:

.. code-block:: console

   $ hasura clone hasura/base

::

   INFO Getting app details...                        app=hasura/base
   INFO Found on Hasura hub
   INFO Downloading...
   INFO Downloaded app to directory                   app=hasura/base directory=/home/sid/gen/base
   INFO Cloned app successfully


This will 'clone' a base project from `hasura.io/hub <https://hasura.io/hub>`_.
Note, you can clone any project from the hub and use that as a starting point for your new project.

.. admonition:: Note

   ``hasura/hello-world`` is another project that contains a few database
   migrations, some sample data and even a sample microservice to get started quickly.

Understand the project structure
--------------------------------
The project (a.k.a. project directory) has a particular directory structure and it has to be maintained strictly, else hasura cli would not work as expected. A representative project is shown below:

.. code-block:: bash

   .
   ├── hasura.yaml
   ├── clusters.yaml
   ├── conf
   │   ├── authorized-keys.yaml
   │   ├── auth.yaml
   │   ├── ci.yaml
   │   ├── domains.yaml
   │   ├── filestore.yaml
   │   ├── gateway.yaml
   │   ├── http-directives.conf
   │   ├── notify.yaml
   │   ├── postgres.yaml
   │   ├── routes.yaml
   │   └── session-store.yaml
   ├── migrations
   │   ├── 1504788327_create_table_user.down.yaml
   │   ├── 1504788327_create_table_user.down.sql
   │   ├── 1504788327_create_table_user.up.yaml
   │   └── 1504788327_create_table_user.up.sql
   └── microservices
       ├── adminer
       │   └── k8s.yaml
       └── flask
           ├── src/
           ├── k8s.yaml
           └── Dockerfile

hasura.yaml
^^^^^^^^^^^

This file contains some metadata about the project, namely a name, description, repoUrl, keywords and some media data. Also contains `platformVersion` which says which Hasura platform version is compatible with this project. It is kind of like a
``requirements.txt`` or a ``package.json`` file you would find in ``python`` & ``nodejs`` apps respectively, and is not a file you have to worry about much during your day to day development effort.

.. code-block:: yaml

  name: <project_name>
  description: <description_text>
  keywords: <tag1>, <tag2>
  repoUrl: <public_code_repo_link>
  media:
    images:
    - <image_link_1>
    - <image_link_2>
    videos:
    - <video_link_1>
  platformVersion: v0.15.3

clusters.yaml
^^^^^^^^^^^^^

Info about the clusters added to this project can be found in this file. Each cluster is defined by its name allotted by Hasura. While adding the cluster to the project you are prompted to give an alias, which is just hasura by default. The kubeContext mentions the name of kubernetes context used to access the cluster, which is also managed by hasura. The config key denotes the location of cluster's metadata on the cluster itself. This information is parsed and cluster's metadata is appended while conf is rendered. The data key is for holding custom variables that you can define.

.. code-block:: bash

   - name: ambitious93
     alias: hasura
     kubeContext: ambitious93
     config:
      configmap: controller-conf
      namespace: hasura
     data: null

conf/
^^^^^

This directory contains the project configuration files such as HTTP routes, continuous integration remotes, etc. You can find more information about each conf file at the top of the file itself.


migrations/
^^^^^^^^^^^

This directory contains database migrations.

microservices/
^^^^^^^^^^^^^^

This directory contains everything related to the microservices that you create; such as the Kubernetes configuration, source code etc.

For more information regarding each directory, you can look at the README.md present in it.

Next: Create a Hasura cluster
-----------------------------

Next, let's head to :doc:`Part III: Create a Hasura cluster<3-hasura-cluster>`.
