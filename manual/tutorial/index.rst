.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _getting_started:

The complete tutorial: Learn by example
=======================================

.. todo::

   - Install hasura CLI
   - Clone base
   - Cluster create --type=free
   - App architecture
   - API console: Users, roles & sessions
   - API console: Create tables (data modelling)
   - API console: Explore APIs as admin user, import sample data
   - API console: Permissions / access control & testing APIs
   - API console: Relationships & testing APIs
   - API console: Aggregations & views, manual relationships
   - API console: File upload
   - Add a custom login/signup API to register + add profile data
   - Add a custom API to generate stats and send email with session middleware
   - Building your UI:

     - Webapp with server-side rendering: express + mustache
        - Notes on how this can be extended to PHP or other frameworks
     - SPA (react/angular): this is only a react example
        - Use generate API code
     - react-native
        - Use generate API code
     - Android/iOS: this is only an android example
        - Use generate API code
   - Add another docker-based microservice: ghost-blog
      - Connect to postgres
      - Environment variables
   - Publish to hasura-hub



Let's build a simple clone of the popular publishing platform `medium <https://medium.com/>`_ and use that to learn about the features provided by the Hasura platform. Our clone, called ``ether`` will have the following features:

#. Anyone can register on the site
#. Users can publish posts, like and comment
#. Anyone can view published posts, overall likes and comments

.. admonition:: Video reference

   You can also follow a roughly equivalent version of this getting started guide as a video series:
   `Getting Started Playlist <https://www.youtube.com/playlist?list=PLTRTpHrUcSB-dJ02BRn1mMhJtgngX-mzI>`_

This tutorial is split across the following sections:

.. toctree::
  :maxdepth: 1

  1-intro
  2-hasura-project
  3-hasura-cluster
  4-explore-hasura-cluster
  5-build-blog-app
  6-data-permissions
  7-custom-api


