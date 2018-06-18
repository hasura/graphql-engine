.. _publish_hub:

Publishing your project to /hub
===============================

You can publish your personal projects on the hub to share your work with the
community and also to help it. Other developers can then clone your project and deploy for their own use or even
build further on top of it.

Watch this quick video to learn how to publish, maintain content and unpublish projects on hub.

.. raw:: html

   <iframe src="https://player.vimeo.com/video/251167051" width="640" height="400" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>

.. note::

   After publishing, make sure to head to your newly published project's page and add content to it so that
   other users know what your project is and how to use it.


hasura CLI publish
^^^^^^^^^^^^^^^^^^

Any Hasura project can be published to `/hub <https://hasura.io/hub>`_.
The project name that you want to publish at is specified in ``hasura.yaml``.


.. code-block:: bash

   $ # from inside your project directory
   $ hasura publish  # optionally --version <version> to give a version number


This will create a tarball of the current directory state, and use the metadata from ``hasura.yaml`` information to publish
your project to the hub.

You can update a published app by running ``hasura publish`` again. This will create a new version of your project and push it to the hub.

This ``hasura.yaml`` file contains the project ``name`` and the ``platformVersion`` which describes with which Hasura platform version the project is compatible with.


.. code-block:: yaml

  name: <project_name>
  platformVersion: v0.15.3
