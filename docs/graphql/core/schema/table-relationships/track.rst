.. meta::
   :description: Track relationships in Hasura
   :keywords: hasura, docs, schema, relationship, track

.. _track_relationships:

Tracking relationships
======================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

It is possible to have relationships in your underlying database without having them tracked and exposed over the GraphQL API. 
For example, this can be the case when you import a database schema into Hasura. This page explains how to track relationships.

Tracking a relationship
-----------------------

Assuming the relevant tables are tracked, but not the relationships, a single relationship can be tracked as follows:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    On the ``Data`` tab in the Hasura console, click the ``Track`` button for the relationships you'd like to track:

    .. thumbnail:: /img/graphql/core/schema/track-single-relationship.png
        :alt: Track single relationship
        :width: 700px

  .. tab:: CLI

  .. tab:: API

Tracking all relationships
--------------------------

Assuming the relevant tables are tracked, but not the relationships, all available relationships can be tracked as follows:

.. rst-class:: api_tabs
.. tabs::

  .. tab:: Console

    On the ``Data`` tab in the Hasura console, click the button ``Track All``:

    .. thumbnail:: /img/graphql/core/schema/track-all-relationships.png
        :alt: Track all relationships
        :width: 700px

  .. tab:: CLI

  .. tab:: API
