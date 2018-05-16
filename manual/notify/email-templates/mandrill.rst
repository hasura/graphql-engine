Sending email templates: Mandrill
=========================

To use `Mandrill <https://www.mandrill.com/>`_ to start sending emails, `signup <https://www.mandrill.com/signup>`_
for an account and create a sending domain. You have to obtain an API key after verifying the sending domain. You can learn creating and adding templates in Mandrill `here <https://mandrill.zendesk.com/hc/en-us/articles/205582507-Getting-Started-with-Templates>`_. Use template names in API requests.

.. note::
  You have to own a domain for completing Mandrill setup.

Please remember that you have to setup SPF and DKIM records for your domain to
start sending emails from your domain. Otherwise, Mandrill will reject sending
emails.

Here are some Mandrill resources for `verifying sending domain
<https://mandrill.zendesk.com/hc/en-us/articles/205582387-How-to-Set-up-Sending-Domains>`_
and setting up `SPF/DKIM:
<https://mandrill.zendesk.com/hc/en-us/articles/205582267>`_

You need to configure the following options in ``conf/notify.yaml`` inside the
project directory:

* Make Mandrill to be the default provider, under ``email``

.. code-block:: yaml

  email:
    default: mandrill

* Now we need to configure Notify with the Mandrill API key. To do this we
  don't store the key directly in the ``notify.yaml`` file. Instead, we refer
  to a secret value in the conf, and our actual API key in the secret.

  Under ``email`` -> ``providers`` -> ``mandrill``, the ``apiKey`` already
  refers to the secret. So we have to update the secret

.. code-block:: shell

  $ hasura secrets update notify.mandrill.key "<your-mandrill-api-key>"
