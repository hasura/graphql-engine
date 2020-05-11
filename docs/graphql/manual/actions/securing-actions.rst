.. meta::
   :description: Securing Hasura actions
   :keywords: hasura, docs, actions, secure

.. _securing_actions:

Securing actions
================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

You will want to make sure that an action can only get called by your own Hasura instance.
You can do so by adding a header to the action that is automatically sent with each request to the webhook.

Step 1: Add an action secret
----------------------------

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

     Head to the ``Actions -> [action-name]`` tab in the console and scroll down to ``Headers``.
     You 

     .. thumbnail:: /img/graphql/manual/actions/action-secret.png
        :alt: Console action secret
        :width: 75%

     Make sure you tick the checkbox to ``Forward client headers to webhook``. 
     
     Then hit ``Create``.

  .. tab:: CLI

     TODO

This secret is only known by Hasura and is passed to your endpoint with every call, 
thus making sure only Hasura can successfully authenticate with the action handler.

Step 2: Configure production instance
-------------------------------------

In your Hasura instance, add the action secret as an environment variable.

Step 3: Verify secret
---------------------

In your :ref:`action handler <action_handlers>`, write some code to check that the action secret passed as a header equals to the one you stored as an environment variable.
