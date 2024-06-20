.. meta::
   :description: Deploy Hasura GraphQL engine with Qovery
   :keywords: hasura, docs, deployment, qovery

.. _deploy_qovery:

Run Hasura GraphQL engine on Qovery
===================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

This guide shows how to deploy Hasura GraphQL engine on Qovery.

Deploying Hasura with a new Postgres DB
---------------------------------------

Step 1: Create a Qovery Account
**************************************************

Visit the `Qovery Dashboard <https://start.qovery.com>` to create an account if you don't already have one.

Step 2: Create a project
************************************************

Click on the "Create a project" button and give a name to your project. Eg. `hasura`

Click on "Next".

Step 3: Create a project
************************************************

Click on "I want to use a template".

.. thumbnail:: https://github.com/Qovery/public-resources/raw/master/deploy/hasura/use-template.png
   :alt: Use a template

Select "Hasura".

.. thumbnail:: https://github.com/Qovery/public-resources/raw/master/deploy/hasura/select-template.png
   :alt: Select Hasura template
   
Select your GitHub or GitLab repository where Qovery will save your configuration files (Qovery uses Git as the source of truth).

.. thumbnail:: https://github.com/Qovery/public-resources/raw/master/deploy/hasura/connect-repo.png
   :alt: Connect GitHub

Click on "Deploy".

Support
---------------------------------------

Chat with Qovery developers on `Discord <https://discord.qovery.com>` if you need help.
