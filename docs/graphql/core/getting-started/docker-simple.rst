.. meta::
   :description: Get started with Hasura using Docker
   :keywords: hasura, docs, start, docker

.. _docker_simple:

Quickstart with Docker
======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This guide will help you get the Hasura GraphQL engine and Postgres running as
Docker containers using Docker Compose. This is the easiest way to set up
Hasura GraphQL engine on your **local environment**.

In case you'd like to run Hasura on an existing Postgres database, follow :ref:`this guide <deployment_docker>`
to deploy the Hasura GraphQL engine as a standalone docker container and connect it to your Postgres instance.

Stay updated with product news
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We push frequent updates to keep improving your Hasura experience.
Sign up for our newsletter to stay updated with product news.

.. raw:: html

   <form id="mc-embedded-subscribe-form" name="mc-embedded-subscribe-form" class="validate post-subscription-form mc_embed_signup newsletter-form newsletter-form-inner" target="_blank" rel="noopener" novalidate>
      <div style="width: 40%">
         <div class="input-box">
            <input type="email" name="EMAIL" id="inner-mce-EMAIL" class="mce-EMAIL" aria-label="Email" placeholder="Your Email Address" pattern="^([^\x00-\x20\x22\x28\x29\x2c\x2e\x3a-\x3c\x3e\x40\x5b-\x5d\x7f-\xff]+|\x22([^\x0d\x22\x5c\x80-\xff]|\x5c[\x00-\x7f])*\x22)(\x2e([^\x00-\x20\x22\x28\x29\x2c\x2e\x3a-\x3c\x3e\x40\x5b-\x5d\x7f-\xff]+|\x22([^\x0d\x22\x5c\x80-\xff]|\x5c[\x00-\x7f])*\x22))*\x40([^\x00-\x20\x22\x28\x29\x2c\x2e\x3a-\x3c\x3e\x40\x5b-\x5d\x7f-\xff]+|\x5b([^\x0d\x5b-\x5d\x80-\xff]|\x5c[\x00-\x7f])*\x5d)(\x2e([^\x00-\x20\x22\x28\x29\x2c\x2e\x3a-\x3c\x3e\x40\x5b-\x5d\x7f-\xff]+|\x5b([^\x0d\x5b-\x5d\x80-\xff]|\x5c[\x00-\x7f])*\x5d))*(\.\w{2,})+$" required>
         </div>
         <div id="mce-responses" class="clear display-inline mce-responses">
            <div id="inner-mce-error-response" class="mce-error-response response error-message hide">
            </div>
            <div id="inner-mce-success-response" class="mce-success-response response success-message hide">
            </div>
         </div>
      </div>
      <div style="position: absolute; left: -5000px;" aria-hidden="true"><input type="text" name="b_9b63e92a98ecdc99732456b0e_f5c4f66bcf" tabindex="-1" value=""></div>
      <div class="clear submit-box" style="max-width: 120px !important">
         <input type="submit" disabled="true" value="Subscribe" name="subscribe" id="inner-mc-embedded-subscribe" class="button mc-embedded-subscribe">
      </div>
   </form>

Prerequisites
-------------

- `Docker <https://docs.docker.com/install/>`_
- `Docker Compose <https://docs.docker.com/compose/install/>`_

Step 1: Get the docker-compose file
-----------------------------------

The `hasura/graphql-engine/install-manifests <https://github.com/hasura/graphql-engine/tree/stable/install-manifests>`__ repo
contains all installation manifests required to deploy Hasura anywhere. Get the docker compose file from there:

.. code-block:: bash

   # in a new directory run
   wget https://raw.githubusercontent.com/hasura/graphql-engine/stable/install-manifests/docker-compose/docker-compose.yaml
   # or run
   curl https://raw.githubusercontent.com/hasura/graphql-engine/stable/install-manifests/docker-compose/docker-compose.yaml -o docker-compose.yml

Step 2: Run Hasura GraphQL engine & Postgres
--------------------------------------------

.. code-block::  bash

   $ docker-compose up -d

Check if the containers are running:

.. code-block:: bash

  $ docker ps

  CONTAINER ID IMAGE                 ... CREATED STATUS PORTS          ...
  097f58433a2b hasura/graphql-engine ... 1m ago  Up 1m  8080->8080/tcp ...
  b0b1aac0508d postgres              ... 1m ago  Up 1m  5432/tcp       ...

Step 3: Open the Hasura console
-------------------------------

Head to ``http://localhost:8080/console`` to open the Hasura console.

Step 4: Try Hasura out
----------------------

Make your :ref:`first graphql query <first_graphql_query>` or set up your :ref:`first event trigger <first_event_trigger>`

You can also check out our `30-Minute Hasura Basics Course <https://hasura.io/learn/graphql/hasura/introduction/>`__
and other `GraphQL & Hasura Courses <https://hasura.io/learn/>`__ for a more detailed introduction to Hasura.

Advanced
--------

This was a quickstart guide to get the Hasura GraphQL engine up and running
quickly. For more detailed instructions on deploying using Docker with an
external database, check out :ref:`deployment_docker`.
