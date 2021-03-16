.. meta::
   :description: Moving project to v2
   :keywords: hasura, docs, project, upgrade, move, v2

.. _move_project_v2:

Manually moving Hasura Cloud v1.3 projects to Hasura v2.0
=========================================================

.. contents:: Table of contents
  :backlinks: none
  :depth: 2
  :local:

Introduction
------------

Hasura Cloud now creates new projects with Hasura ``v2.0`` by default. Due to
some underlying architectural changes in ``v2.0``, existing projects have not
yet been upgraded to ``v2.0``. **This upgrade will be done automatically in the
very near future.**

In the meanwhile it is possible to manually "move" your project to use Hasura
``v2.0``.

Do check the `changelog <https://github.com/hasura/graphql-engine/releases>`__ first
to see what changes and features have been introduced.

Move existing v1.3 project to a v2.0 project
--------------------------------------------

As it is not possible to actually upgrade your ``v1.3`` project to ``v2.0``, you will have to
essentially create a new Hasura project with ``v2.0`` and connect it with your database with the same
Hasura metadata as in the previous project.

Step 1: Export metadata from existing project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

See :ref:`exporting metadata <exporting_metadata>` to get a copy of the current Hasura metadata
on your project.

Do ensure no further changes are made to the Hasura metadata post this.

Step 2: Create a v2.0 Cloud project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

See ``Step 1`` of :ref:`creating projects <create_project>` to create a new Hasura Cloud ``v2.0``
project.

Step 3: Connect your database with the name **default** to the new project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

See ``Step 2`` of :ref:`creating projects <create_project>` to connect your existing database to
the new project. Please ensure you set the database name as ``default``.

.. note::

   After connecting a database to a ``v2.0`` project it will not be usable with
   a ``v1.3`` project. Hence it is recommended to stop your ``v1.3`` project
   before doing this.

Step 4: Apply the exported metadata to the new project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

See :ref:`applying metadata <applying_metadata>` to apply the earlier exported metadata to the new
project.

Your GraphQL API should now be regenerated as in the earlier ``v1.3`` project.

