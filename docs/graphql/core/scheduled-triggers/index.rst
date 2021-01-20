.. meta::
   :description: Create a scheduled trigger with Hasura
   :keywords: hasura, docs, scheduled trigger

.. _scheduled_triggers:

Scheduled Triggers
==================

.. contents:: Table of contents
  :backlinks: none
  :depth: 1
  :local:

Introduction
------------

Scheduled triggers are used to execute custom business logic at specific points in time.

.. thumbnail:: /img/graphql/core/scheduled-triggers/scheduled-trigger-arch.png
  :class: no-shadow
  :width: 700px
  :alt: Hasura scheduled trigger architecture

.. admonition:: Supported from

  Scheduled triggers are supported from versions ``v.1.3.0`` and above.

Types
-----

There are two types of timed events:

- :ref:`Cron triggers <creating_cron_trigger>`: based on a cron schedule.
- :ref:`One-off scheduled events <creating_one_off_scheduled_event>`: based on a fixed timestamp.

.. toctree::
   :maxdepth: 2
   :titlesonly:
   :hidden:

   create-cron-trigger
   create-one-off-scheduled-event
