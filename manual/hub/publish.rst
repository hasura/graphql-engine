.. _hub:

============================
Publish your project to /hub
============================

Video tutorial
^^^^^^^^^^^^^^

Watch this quick video to learn how to publish, maintain content and unpublish projects on hub.

.. raw:: html

   <iframe src="https://player.vimeo.com/video/251167051" width="640" height="400" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>


hasura CLI publish
^^^^^^^^^^^^^^^^^^

Any Hasura project can be published to `/hub <https://hasura.io/hub>`_.
The project name that you want to publish at is specified in ``hasura.yaml``.


.. code-block:: bash

   $ #From inside your project directory
   $ hasura publish


This will create a tarball of the current directory state, and use the metadata from ``hasura.yaml`` information to publish
your project to the hub.

hasura.yaml
^^^^^^^^^^^

This file contains some metadata about the project, namely a name, description, repoUrl, keywords and some media data. Also contains `platformVersion` which says which Hasura platform version is compatible with this project.

.. code-block:: yaml

  name: <project_name>
  platformVersion: v0.15.3
