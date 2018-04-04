.. _htp-sms:

Sending SMS: Hasura Test Provider
=================================

`Hasura` is a test provider that helps you send Emails and SMS with Notify. Since it is a test provider, you are restricted to up till 10 emails and SMS per day.

To start sending SMS using ``Hasura``, follow the steps below.

* Add ``hasura`` as a provider to ``conf/notify.yaml`` (if it is not there) and set it as default. The sms section of ``conf/notify.yaml`` should look like the code block below:

.. code-block:: yaml
   :emphasize-lines: 3, 5-9

   sms:
     # default can take values 'msg91' or 'twilio'
     default: hasura
     providers:
       hasura:
         authToken:
           secretKeyRef:
             key: notify.hasura.token
             name: hasura-secrets
       msg91:
         from: ""
         authKey:
           secretKeyRef:
             key: notify.msg91.key
             name: hasura-secrets
       twilio:
         from: ""
         accountSid:
           secretKeyRef:
             key: notify.twilio.accountsid
             name: hasura-secrets
         authToken:
           secretKeyRef:
             key: notify.twilio.authtoken
             name: hasura-secrets


* Set ``Hasura`` as the default sms provider in ``conf/notify.yaml`` inside the project directory.

.. code-block:: yaml

  sms:
    default: hasura


* Get your user information.

.. code-block:: shell

  $ hasura user-info

* Copy the ``Token`` and update it as a secret

.. code-block:: shell

  $ hasura secrets update notify.hasura.token "<token>"

``Hasura`` is now configured as your default SMS provider. You can start using it to send SMS.

.. note::
  You can only send 10 SMS per day using ``Hasura``.
