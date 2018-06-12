.. _htp-email:

Sending emails: Hasura Test Provider
====================================

`Hasura` is a test provider that helps you send Emails and SMS with Notify. Since it is a test provider, you are restricted to up till 10 emails and SMS per day.

To send emails using ``Hasura``, follow the steps below.

* Add ``hasura`` as a provider to ``conf/notify.yaml`` (if it is not there) and set it as default. The email section of ``conf/notify.yaml`` should look like the code block below:

.. code-block:: yaml
   :emphasize-lines: 3, 5-9

   email:
     # default can take values 'sparkPost' or 'mandrill'
     default: hasura
     providers:
       hasura:
         authToken:
           secretKeyRef:
             key: notify.hasura.token
             name: hasura-secrets
       mandrill:
         apiKey:
           secretKeyRef:
             key: notify.mandrill.key
             name: hasura-secrets
       sparkPost:
         apiKey:
           secretKeyRef:
             key: notify.sparkpost.key
             name: hasura-secrets

* Set ``Hasura`` as the default email provider in ``conf/notify.yaml`` inside the project directory.

.. code-block:: yaml

  email:
    default: hasura


* Get your user information.

.. code-block:: shell

  $ hasura user-info

* Copy the ``Token`` and update it as a secret

.. code-block:: shell

  $ hasura secrets update notify.hasura.token "<token>"

``Hasura`` is now configured as your default email provider. You can start using it to send emails.

.. note::
  You can only send 10 emails per day using ``Hasura``.
