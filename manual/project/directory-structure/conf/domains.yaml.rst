.. _hasura-dir-conf-domains.yaml:

Project structure: conf/domains.yaml
====================================

.. note::

   This file is rendered as a template. Refer to :ref:`Conf files templating <conf-templating>` for more details.

Domain configuration for the gateway is defined in this file. Hasura allots a ``[cluster-name].hasura-app.io`` for every cluster, for which the configuration should look like the following:

.. code-block:: yaml

   {{ cluster.name }}.hasura-app.io:
     ssl:
       type: LetsEncrypt
       conf:
         account: {{ cluster.name }}

An example configuration for a domain without SSL:

.. code-block:: yaml

   example-domain.com:
     ssl: null

If you own a domain and need point that domain to a Hasura cluster, you just need to point the domain to the cluster IP and add a domain configuration here.

Checkout :ref:`Adding a custom domain <adding-custom-domain>` for more details.

You can find the default file at `conf/domains.yaml <https://github.com/hasura/base/blob/master/conf/domains.yaml>`_ in the base repo.
