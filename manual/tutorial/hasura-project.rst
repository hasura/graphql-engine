Part I: Create a Hasura project
===============================

A Hasura project is a directory (ie: a code repo) that contains all the configuration files, the database
migrations, along with the source code and configuration of your custom microservices. This project directory should be
a git repo, so that you can ``git push hasura master`` to deploy everything in the repo to a Hasura cluster.


Create a 'hello-world' project
------------------------------

Run the following command:

.. code-block:: bash

   $ hasura clone hasura/hello-world

::

   Cloning project...
   ✓ Project cloned directory=<dir-path>/hello-world


This will 'clone' the `hasura/hello-world <https://hasura.io/hub/projects/hasura/hello-world>`_ project from
`hasura.io/hub <https://hasura.io/hub>`_.

.. admonition:: Note

   ``hasura/hello-world`` is a starter project that contains a few database migrations to add a sample schema and
   some sample data to let you start experimenting quickly.

   You can clone any project from the `hub <https://hasura.io/hub>`_ and use that as a starting point for your new project.

Understand the project structure
--------------------------------
A Hasura project has a particular directory structure and it has to be maintained strictly, else ``hasura CLI`` will not work
as expected.

Move to the project directory we just cloned.

Run the following command:

.. code-block:: bash

   $ cd hello-world


Every Hasura project follows the below structure:

.. code-block:: bash

   .
   ├── .hasura
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
   │   ├── <1504788327_create_table_user.down.yaml>
   │   ├── <1504788327_create_table_user.down.sql>
   │   ├── <1504788327_create_table_user.up.yaml>
   │   └── <1504788327_create_table_user.up.sql>
   └── microservices
       ├── <adminer>
       │   └── k8s.yaml
       └── <flask>
           ├── src/
           ├── k8s.yaml
           └── Dockerfile

.. note::

   In our ``hello-world`` project, the ``microservices`` directories will by empty right now

hasura.yaml
^^^^^^^^^^^

This file contains some metadata about the project, namely a ``name`` and a ``platformVersion`` which says which Hasura platform
version is compatible with this project. It is kind of like a ``requirements.txt`` or a ``package.json`` file you would
find in ``python`` & ``nodejs`` apps respectively, and is not a file you have to worry about much during your day to day
development effort.

.. code-block:: yaml

  name: <project_name>
  platformVersion: v0.15.23

clusters.yaml
^^^^^^^^^^^^^

This file contains the configuration of your infrastructure. The idea is to have a declarative configuration of your
infrastructure so that you can create instances of your infra on-demand.

.. code-block:: yaml

   version: v1
   provider: digital-ocean
   region: blr1
   nodes:
   - type: s-2vcpu-4gb
     labels:
       app: postgres
   volumes:
   - name: postgres
     size: 10
   - name: filestore
     size: 30
   - name: sessionstore
     size: 5
   # custom volume
   - name: my-volume
     size: 10

.hasura
^^^^^^^

Info about the actual clusters added to this project can be found in this file. Each cluster is defined by it’s ``name``
allotted by Hasura, and an ``alias`` that matches with one in ``clusters.yaml``. While adding the cluster to the project
you are prompted to give an alias, which is just ``hasura`` by default.
The ``kubeContext`` mentions the name of kubernetes context used to access the cluster, which is also managed by hasura.
The ``data`` key is for holding custom variables that you can define.

.. code-block:: yaml

   clusters:
   - alias: hasura
     config:
       configmap: controller-conf
       namespace: hasura
     data: null
     kubeContext: h33-blinders97
     name: h33-blinders97
   defaultCluster: hasura

conf/
^^^^^

This directory contains the project configuration files such as HTTP routes, continuous integration remotes, etc. You
can find more information about each conf file at the top of the file itself.


migrations/
^^^^^^^^^^^

This directory contains database migrations.

microservices/
^^^^^^^^^^^^^^

This directory contains everything related to the microservices that you create; such as the Kubernetes configuration,
source code etc.


Next: Create a Hasura cluster
-----------------------------

Next, let's head to :doc:`hasura-cluster`.
