.. meta::
   :description: Securing projects on Hasura Cloud
   :keywords: hasura, docs, project

.. _secure_project:

Securing projects
=================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

To make sure that your GraphQL endpoint is not publicly accessible,
a randomly generated admin secret key is added by default to your project at the
time of project creation.

Updating the admin secret
-------------------------

Step 1: Go to settings
^^^^^^^^^^^^^^^^^^^^^^

On the project overview, click on the settings icon on the top right of the relevant project.

.. thumbnail:: /img/graphql/cloud/projects/secure-settings.png
   :alt: Go to settings
   :width: 865px

Step 2: Navigate to env vars
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

On the ``Env vars`` tab, you will see the ``HASURA_GRAPHQL_ADMIN_SECRET`` env var.

.. thumbnail:: /img/graphql/cloud/projects/secure-admin-envvar.png
   :alt: Navigate to env vars
   :width: 1100px

Step 3: Update admin secret
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Click on the ``HASURA_GRAPHQL_ADMIN_SECRET`` env var to update the value.

.. thumbnail:: /img/graphql/cloud/projects/secure-update-envvar.png
   :alt: Set admin secret
   :width: 1100px

Accessing Hasura
----------------

When you launch the console from the Hasura Cloud dashboard, you'll be authenticated as an admin.
If you want to make API calls from outside the console, you need to pass the admin secret as the `x-hasura-admin-secret` request header.

.. note::

    The admin secret should be treated like a password i.e. it should be kept secret and shouldn't be passed from frontend clients.
    Refer :ref:`this <authentication>` to set up user authentication.
