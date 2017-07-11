.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. meta::
   :description: Reference documentation for encapsulating a Hasura project's configuration and how the state of an project is changed and reported based on its value.
   :keywords: hasura, docs, project configuration, proj conf, configuration

Project Configuration and Status
================================

A Hasura project configuration is specified via a JSON object that is
stored as a kubernetes configmap.

The project configuration is used by the Hasura kubernetes operator (Shukra)
and changes are applied. Shukra notes the status (progress, errors) of the
project configuration application in another configmap called the Hasura project
status.

There are 3 items involved in configuring any hasura project and 1 (``hasura-project-status``) in
reporting the status of configuration being applied to the project:

.. toctree::
  :maxdepth: 1

  hasura-project-conf
  hasura-project-secrets
  controller-conf
  hasura-project-status
