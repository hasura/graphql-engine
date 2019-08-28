Run Hasura GraphQL engine on Nhost
===================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Getting started
------------------------------------------

To deploy a Nhost project with Hasura follow the :doc:`Nhost quickstart guide <../../getting-started/nhost-simple>`.

A Nhost project includes a database with Hasura. It also includes authentication and storage integrated with the same permission system used by Hasura.


Change Hasura GraphQL settings
------------------------------------------

1. Go to your Nhost project.
2. Click on Settings in the left menu
3. Click on "Hasura" to expand and see your current settings for Hasura GraphQL engine
4. Click "Edit" in the bottom left corner
5. Edit your settings
6. Click Save

You can change the following settings:

- Hasura GraphQL engine version
- Hasura GraphQL admin secret
- Unauthorized role
- Secret webhook header
