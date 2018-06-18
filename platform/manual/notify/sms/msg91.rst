Sending SMS: MSG91
==================

`MSG91 <https://msg91.com/>`_ is a SMS provider where you can `signup <https://msg91.com/signup>`_
and get an API key to use with Hasura. Take a look at `this
<http://help.msg91.com/article/177-where-can-i-find-my-authentication-key>`_
guide by MSG91 to obtain your API key.

You need to configure the following options in ``conf/notify.yaml`` in your
Hasura project directory:

* Make MSG91 to be the default provider, under ``sms``

.. code-block:: yaml

  sms:
    default: msg91

* Under ``sms`` -> ``providers`` -> ``msg91``

.. code-block:: yaml

      sms:
        providers:
          msg91:
            from: "<your-msg91-identification>"

**<your-msg91-identification>**: Sender identification (maximum 6 characters)
from which the SMS will be sent.

* Now we need to configure Notify with the MSG91 Auth key. To do this we
  don't store the key directly in the ``notify.yaml`` file. Instead, we refer
  to a secret value in the conf, and our actual Auth key in the secret.

  Under ``sms`` -> ``providers`` -> ``msg91``, the ``authKey`` already refers
  to the secret. So we have to update the secret

.. code-block:: shell

  $ hasura secrets update notify.msg91.key "<your-msg91-key>"

Where, **<your-msg91-key>** is the Unique identifier obtained from MSG91
Dashboard.

