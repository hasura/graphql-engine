.. _hasura-dir-conf-ci.yaml:

ci.yaml
=======

.. note::

   This file is rendered as a template. Refer to :ref:`Using Templates <using-templates>` for more details.

Continuous integration settings for a cluster is specified in this file. Two parameters together define continuous integration: ``remotes`` and ``registry``.

remotes
-------

Details under a remote indicate which microservices has to be updated when a git push happens against that remote. Each key creates a git remote on the cluster. When a push is made to this cluster, docker images are built and the microservices are updated as per the definition.

.. code-block:: yaml

   remote-name:                                   # name of the remote
     namespace.microservice-name:                 # microservice to be updated
       container-name:                            # container name to be updated
         path: microservices/app                  # path where image has to be built
         dockerfile: microservices/app/Dockerfile # dockerfile for building the image

registry
--------

If a registry is defined, all docker images built will be pushed to and pulled from this registry. You can create a private registry and configure it to be used here. More details at [link].

.. code-block:: yaml

   prefix: gcr.io/example-registry
   regSecret: registry-secret-name
   

You can find the default file at `conf/ci.yaml <https://github.com/hasura/base/blob/master/conf/ci.yaml>`_ in the base repo.

