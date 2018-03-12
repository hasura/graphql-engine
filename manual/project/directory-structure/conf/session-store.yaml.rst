.. _hasura-dir-conf-session-store.yaml:

Project structure: conf/session-store.yaml
==========================================

.. note::

   This file is rendered as a template. Refer to :ref:`Conf files templating <conf-templating>` for more details.

Config for Redis microservice, which is used for storing session information by auth and gateway, are present here.

.. code-block:: yaml

   volume: {{ cluster.metadata.sessionStore.volume|json }}

Volume defines the Kubernetes volume to be mounted for persisting the Redis state, this is usually filled in from the cluster metadata.

You can find the default file at `conf/session-store.yaml <https://github.com/hasura/base/blob/master/conf/session-store.yaml>`_ in the base repo.


