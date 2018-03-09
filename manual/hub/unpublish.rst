.. _unpublish_hub:

Unpublishing your project from /hub
===================================


To unpublish a project from the hub, ``cd`` into your project directory and run:

.. code-block:: bash

   $ # from inside your project directory
   $ hasura unpublish

This will delete all versions of the current project from the hub.

To delete only a particular version of a project, run:

.. code-block:: bash

   $ # from inside your project directory
   $ hasura unpublish --version <version>

.. note::

   Unpublishing a project means other developers will not be able to see and clone your project from the hub anymore.
   Although, other projects that had already been created by cloning this project will have no impact on them.