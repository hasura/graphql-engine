.. _hub:

============================
Publish your project to /hub
============================

Any Hasura project can be published to `/hub <https://hasura.io/hub>`_.
The details of what gets published are mentioned in ``hasura.yaml``.


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
  description: <description_text>
  keywords: <tag1>, <tag2>
  repoUrl: <public_code_repo_link>
  media:
    images:
    - <image_link_1>
    - <image_link_2>
    videos:
    - <video_link_1>
  platformVersion: v0.15.3
