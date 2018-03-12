.. _hasura-dir-conf-dev-mode.yaml:

Project structure: conf/dev-mode.yaml
=====================================

.. note::

   This file is rendered as a template. Refer to :ref:`Conf files templating <conf-templating>` for more details.

If development mode is set as ``true`` for certain microservices, an environment variable mentioning the microservice is in development will be set.
This can be used to output debug logs or messages.

``gateway`` makes use of this to display context aware error pages. For e.g., when someone visits a route that does not exist, in development mode,
gateway shows an error page indicating all the routes that are available.

.. code-block:: yaml

   gateway: true

You can find the default file at `conf/dev-mode.yaml <https://github.com/hasura/base/blob/master/conf/dev-mode.yaml>`_ in the base repo.

