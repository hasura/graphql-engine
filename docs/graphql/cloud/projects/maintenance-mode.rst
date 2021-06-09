.. meta::
   :description: Cloud maintenance mode 
   :keywords: hasura, docs, project, maintenance, maintenance mode

.. _cloud_maintenance_mode:

Maintenance mode
================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

When updates are being rolled out to your Hasura Cloud project which demand 
no interruptions to your instance's configuration, maintenance mode is activated
for your project. 

.. note::

  Currently, maintenance mode will only be activated for your project while you 
  are updating it to Hasura GraphQL Engine ``v2.0``. 

.. note::
  
  This is not to be confused with environment variable ``HASURA_GRAPHQL_ENABLE_MAINTENANCE_MODE``. 
  The two are different as the env var enables the server maintenance mode which disables metadata APIs 
  (writes) on the project while the cloud maintenance mode does not. See below for actions disabled 
  by the cloud maintenance mode.

Check if maintenance mode is activated
--------------------------------------

Navigate to your project's settings page, you should see a top banner mentioning if 
your project is under maintenance. 

.. thumbnail:: /img/graphql/cloud/projects/maintenance-mode.png
   :alt: Project with maintenance mode activated

If you don't see such a banner, your project is not under maintenance mode.

Disabled actions when maintenance mode is activated
---------------------------------------------------

All actions that update your project's configurations are not allowed when maintenance
mode is activated. The following actions are disallowed: 

- `Switching pricing plans <pricing>`_
- `Adding, updating and deleting an environment variable <env-vars>`_
- `Changing project region <regions>`_
- `Enabling and disabling Heroku database URL sync <heroku-url-sync>`_
- `Deleting project <delete>`_


If you are trying to apply these changes when maintenance mode is activated, you will encounter 
an error. 


