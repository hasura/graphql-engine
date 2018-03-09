Sending SMS: Twilio
===================

`Twilio`_ is a SMS, Voice and Video provider. You can `signup
<https://www.twilio.com/try-twilio>`_ for a free account and start sending SMS
using Twilio. Once you have signed up and created a Twilio phone number, grab
the *Account SID* and *Auth Token* from `Twilio Console
<https://www.twilio.com/console/account/settings>`_.

You need to configure the following options in ``conf/notify.yaml`` in your
Hasura project directory:

* Make Twilio to be the default provider, under ``sms``

.. code-block:: yaml

  sms:
    default: twilio

* Under ``sms`` -> ``providers`` -> ``twilio``

.. code-block:: yaml

      sms:
        providers:
          twilioConf:
            from: "<your-twilio-number>"

* **from**: Sender number obtained from Twilio Console. Shows up as *From* at receiver's end.

If you are following the docs along with ``conf/notify.yaml`` file open, you
might notice ``accountSid`` and ``authToken`` fields. Ignore the ``accountSid``
and ``authToken`` fields. We don't store them directly in this conf. Rather we
store them in secrets and refer to them here.

* To configure Twilio account SID and auth token as secrets, let's update the secrets

.. code-block:: shell

  $ hasura secrets update notify.twilio.accountsid "<twilio-account-sid>"
  $ hasura secrets update notify.twilio.authtoken "<twilio-auth-token>"

Where,

**<twilio-account-sid>**: Unique identifier for your account, obtained from the
API Credentials section of Twilio Console.

**<twilio-auth-token>**: Password like secret key for the account.

.. _Twilio: https://www.twilio.com/