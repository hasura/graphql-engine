.. _hasura-dir-conf-notify.yaml:

Project structure: conf/notify.yaml
===================================

.. note::

   This file is rendered as a template. Refer to :ref:`Conf files templating <conf-templating>` for more details.

Hasura Notify Microservice can be used to send emails and sms to users.

All options are configured to read from the secret called hasura-secrets. To enable a provider:

1. Add the required secrets for a provider
2. Set the default provider

Checkout :ref:`Email <hasura-notify-email>` and :ref:`SMS <hasura-notify-sms>` docs for more details on setting providers.

You can find the default file at `conf/notify.yaml <https://github.com/hasura/base/blob/master/conf/notify.yaml>`_ in the base repo.

