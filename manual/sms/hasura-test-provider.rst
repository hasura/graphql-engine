Hasura Test Provider
--------------------

`Hasura` is a test provider that helps you send Emails and SMS with Notify. Since it is a test provider, you are restricted to up till 10 emails and SMS per day.

To start sending SMS using ``Hasura``, follow the steps below.

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


