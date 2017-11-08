.. .. meta::
   :description: Describing the hasura project directory structure
   :keywords: hasura, docs, CLI, HasuraCTL, hasuractl, hasuracli

.. _hasuractl-manual:

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

You can `git push hasura master` to deploy everything, including your custom
microservices, database migrations and project configuration.

Creating a project
------------------

.. code:: bash

   $ hasuractl clone base my-project

   # creates a directory called `my-project` and initialize an empty Hasura project

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
   migrations, some sample data and even a sample mircoservice to get started quickly.

.. note::

  This command automatically intializes a git repository


Files and directories
---------------------

The project (a.k.a. project directory) has a particular directory structure and
it has to be maintained strictly, else hasura cli would not work as expected. A
representative project is shown below:

.. code:: bash
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
  │   ├── 1504788327_create_table_userprofile.down.yaml
  │   ├── 1504788327_create_table_userprofile.down.sql
  │   ├── 1504788327_create_table_userprofile.up.yaml
  │   └── 1504788327_create_table_userprofile.up.sql
  └── microservices 
      ├── adminer
      │   └── k8s.yaml
      └── flask
          ├── src/
          ├── k8s.yaml
          └── Dockerfile


* `hasura.yaml`

This file contains some metadata about the project, namely a name, description
and some keywords. Also contains `platformVersion` which says which Hasura
platform version is compatible with this project.

* `clusters.yaml`

Info about the clusters added to this project can be found in this file. Each
cluster is defined by it's name allotted by Hasura. While adding the cluster to
the project you are prompted to give an alias, which is just hasura by default.
The `kubeContext` mentions the name of kubernetes context used to access the
cluster, which is also managed by hasura. The `config` key denotes the location
of cluster's metadata on the cluster itself. This information is parsed and
cluster's metadata is appended while conf is rendered. `data` key is for
holding custom variables that you can define.

.. code-block:: yaml

  - name: h34-ambitious93-stg
    alias: hasura
    kubeContext: h34-ambitious93-stg
    config:
      configmap: controller-conf
      namespace: hasura
    data: null  


* ``conf``
      
  * ``authorized-keys.yaml``
    
    * SSH keys allowed to access the cluster
    * One public key per line
      
  * ``*.yaml``
    
    * Configuration for the cluster, split into various yaml files
      
* ``migrations``

  * Database migration files are kept in this directory
    
* ``microservices``

  * Default directory to store source code for custom microservices
  * Each sub-directory contains source code and *Dockerfile*
  

.. note::

  hasura cli doesn't consider any other files or directories outside of those mentioned above
