.. _hasura-dir-ms-dockerfile:

Project structure: microservices/\*/Dockerfile
==============================================

A `Dockerfile <https://docs.docker.com/engine/reference/builder/>`_ defines how a Docker image should be built and run. Hasura uses the dockerfile when continuous integration is enabled for a microservice. The location for dockerfile and the path where which docker build has to be executed should be mentioned in :ref:`ci.yaml <hasura-dir-conf-ci.yaml>`.

You can find sample file for a python-flask microservice at `hasura/hello-python-flask/microservices/app/Dockerfile <https://github.com/hasura/hello-python-flask/blob/master/microservices/app/Dockerfile>`_.
