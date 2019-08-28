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

- Go to your Nhost project.
- Click on Settings in the left menu
- Click on "Hasura" to expand and see your current settings for Hasura GraphQL engine
- Click "Edit" in the bottom left corner
- Edit your settings
- Click Save

You can change the following settings:

- Hasura GraphQL engine version
- Hasura GraphQL admin secret
- Unauthorized role
- Secret webhook header
