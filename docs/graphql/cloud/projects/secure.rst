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

To make sure that your GraphQL endpoint and the Hasura console are not publicly accessible, you need to configure an admin secret key.

Adding an admin secret
----------------------

Step 1: Go to settings
^^^^^^^^^^^^^^^^^^^^^^

On the project overview, click on the settings icon on the top right of the relevant project.

.. thumbnail:: /img/graphql/cloud/projects/secure-settings.png
   :alt: Go to settings
   :width: 865px

Step 2: Navigate to env vars
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

On the ``Env vars`` tab, click the button to add a new env var.

.. thumbnail:: /img/graphql/cloud/projects/secure-envvars.png
   :alt: Navigate to env vars
   :width: 865px

Step 3: Add an admin secret
^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the ``Key`` dropdown, choose ``ADMIN_SECRET`` and add the secret in the ``Value`` field. Then click the ``Add`` button.

.. thumbnail:: /img/graphql/cloud/projects/secure-add-envvar.png
   :alt: Navigate to env vars
   :width: 865px

Accessing Hasura
----------------

After setting an admin secret, when you launch the console from the Hasura Cloud dashboard, you'll be authenticated as an admin. 
If you want to make API calls from outside the console, you need to pass the admin secret as the `x-hasura-admin-secret` request header.

.. note::

    The admin secret should be treated like a password i.e. it should be kept secret and shouldn't be passed from frontend clients.
    Refer :ref:`this <authentication>` to set up user authentication.
