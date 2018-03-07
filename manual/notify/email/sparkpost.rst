Sending emails: SparkPost
=========================

To start sending emails using SparkPost, `signup <https://www.sparkpost.com/>`_
for an account and create a sending domain. You have to obtain an API key after
verifying the sending domain. SparkPost provides helpful interface to guide
through the process. SparkPost's free plan should be enough to cover your
initial email volume.

.. note::
  You have to own a domain for completing SparkPost setup.

Please remember that you have to setup SPF and DKIM records for your domain to
start sending emails from your domain. Otherwise, SparkPost will reject sending
emails.

Here are some SparkPost resources for `verifying sending domain
<https://support.sparkpost.com/customer/portal/articles/1933360-verify-sending-domains>`_
and setting up `SPF/DKIM:
<https://www.sparkpost.com/blog/understanding-spf-and-dkim-in-sixth-grade-english/>`_

You need to configure the following options in ``conf/notify.yaml`` inside the
project directory:

* Make Sparkpost to be the default provider, under ``email``

.. code-block:: yaml

  email:
    default: sparkPost

* Now we need to configure Notify with the Sparkpost API key. To do this we
  don't store the key directly in the ``notify.yaml`` file. Instead, we refer
  to a secret value in the conf, and our actual API key in the secret.

  Under ``email`` -> ``providers`` -> ``sparkPost``, the ``apiKey`` already
  refers to the secret. So we have to update the secret

.. code-block:: shell

  $ hasura secrets update notify.sparkpost.key "<your-sparkpost-api-key>"
