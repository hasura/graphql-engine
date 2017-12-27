.. .. meta::
   :description: Manual for using Notify Microservice on Hasura. Notify is used to send email or sms from Hasura project to users through providers like SMTP, Spark Post, Twilio, MSG91.
   :keywords: hasura, docs, notify, email, sms, smtp, sparkpost, twilio, msg91

Email & SMS
===========
The ``Notify`` microservice on Hasura lets you contact / notify your project
users through email and SMS.

.. note::

  This is an admin-only micro-service. Only those users with ``admin`` role
  associated to them can access the API. Ideally, use it from a custom API, so
  that you can set ``X-Hasura-Role: admin`` and ``X-Hasura-User-Id: 1`` and
  make requests to the API.


Email
-----
Notify currently supports sending emails via `SMTP`_, `Sparkpost`_.

Also, for testing out ``Notify``, we have our own test provider called ``Hasura`` which will allow you to send 10 emails per day. 

The simplest way to send emails is by configuring SMTP. Your existing email
address (like GMail, Yahoo, Live.com etc.) can be used to send emails via SMTP.
Although this should be avoided in production instances.

Configuring SMTP
^^^^^^^^^^^^^^^^
You need to configure the following options in the file ``conf/notify.yaml`` in
your Hasura project directory.

* Make SMTP to be the default provider, under ``email``

.. code-block:: yaml

  email:
    default: smtp

* Now configure your SMTP host and port, under ``email`` -> ``providers`` ->
  ``smtp``

.. code-block:: yaml

  email:
    default: smtp
    providers:
      smtp:
        hostname: "String"
        port: Int


**hostname**: SMTP hostname for email provider (e.g. smtp.gmail.com).

**port**: SMTP port number (e.g. 465) (Notify only supports SSL authentication
as of now.)

If you are following the docs along with ``conf/notify.yaml`` file open, you
might notice ``username`` and ``password`` fields. Ignore the ``username`` and
``password`` fields. We don't store the username and password directly in this
conf. Rather we store them in secrets and refer to them here.

* To configure SMTP username and password as secrets, let's update the secrets

.. code-block:: shell

  $ hasura secrets update notify.smtp.username "yourusername@example.com"
  $ hasura secrets update notify.smtp.password "youremailpassword"


.. note::
  If you are using GMail as your SMTP provider, you have to enable access to
  "Less Secure Apps" on your Google account. You can read about it `here
  <https://support.google.com/accounts/answer/6010255>`_.  GMail also enforces
  limits on emails sent (default 500/day).

.. note::
  For Hasura projects on Google Compute Engine / Google Container Engine, SMTP
  settings with standard ports like 25, 465, 587 will not work, since Google
  Compute Engine does not allow outbound connections on these ports. Hence,
  make sure that your SMTP provider have alternate ports like 2525 in case you
  want to deploy on Google Cloud. You can find more details and possible
  solutions `here
  <https://cloud.google.com/compute/docs/tutorials/sending-mail/>`_.


To see all the secrets in your cluster:

.. code-block:: shell

  $ hasura secrets list


Configuring SparkPost
^^^^^^^^^^^^^^^^^^^^^
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

.. note::
  Make sure that you have checked Transmissions: Read/Write in API Permission
  section on SparkPost while creating the API key.

To see all the secrets in your cluster:

.. code-block:: shell

  $ hasura secrets list


Configuring Hasura Test Provider
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
To send emails using ``Hasura``, follow the steps below.

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

Sending an email
^^^^^^^^^^^^^^^^

.. note::
  ``Notify`` is an admin-only microservice. Hence, only users with ``admin`` role
  will be able to access these APIs

Send an email as per the options given is request body.

.. code-block:: http

  POST https://notify.<cluster-name>.hasura-app.io/v1/send/email HTTP/1.1
  Content-Type: application/json
  Authorization: Bearer <admin-token>

  {
    "to": "Example User <user@example.com>",
    "from": "admin@project.com",
    "fromName": "Project Admin",
    "sub": "This is the email subject line",
    "text": "This is the email content in plain text",
    "html": "<p>This is the <b>email content</b> in html format</p>"
  }


**Example response**:

.. code-block:: http

  HTTP/1.1 200 OK
  Content-Type: application/json

  {
    "id": "<provider-reference-id>",
    "detail": "<details>"
  }


``Authorization`` header is not required if the request is being made from a
browser, since ``Cookie`` will be set.

.. note::

  If you are writing backend code and want to send email from that code, you can
  directly contact the ``Notify`` microservice using the URL
  ``http://notify.hasura/v1/send/email``. You will also need to set headers
  ``X-Hasura-User-Id: 1`` and ``X-Hasura-User-Role: admin`` to make the request
  as ``admin``.


SMS
---
Notify currently supports sending SMS via `Twilio`_ or `MSG91`_.

Also, for testing out ``Notify``, we have our own SMS test provider called ``Hasura`` which will allow you to send 10 SMS per day. 

Configuring Twilio
^^^^^^^^^^^^^^^^^^
`Twilio`_ is a SMS, Voice and Video provider. You can `signup
<https://www.twilio.com/try-twilio>`_ for a free account and start sending SMS
using Twilio. Once you have signed up and created a Twilio phone number, grab
the *Account SID* and *Auth Token* from `Twilio Console
<https://www.twilio.com/console/account/settings>`_.

You need to configure the following options in ``conf/notify.yaml`` in your
Hasura project directory:

* Make Twilio to be the default provider, under ``sms``

.. code-block:: yaml

  email:
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


To see all the secrets in your cluster:

.. code-block:: shell

  $ hasura secrets list


Configuring MSG91
^^^^^^^^^^^^^^^^^
`MSG91`_ is a SMS provider where you can `signup <https://msg91.com/signup>`_
and get an API key to use with Hasura. Take a look at `this
<http://help.msg91.com/article/177-where-can-i-find-my-authentication-key>`_
guide by MSG91 to obtain your API key.

You need to configure the following options in ``conf/notify.yaml`` in your
Hasura project directory:

* Make MSG91 to be the default provider, under ``sms``

.. code-block:: yaml

  email:
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

To see all the secrets in your cluster:

.. code-block:: shell

  $ hasura secrets list


Configuring Hasura Test Provider
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To send SMS using ``Hasura``, follow the steps below.

* Set ``Hasura`` as the default SMS provider in ``conf/notify.yaml`` inside the
project directory.

.. code-block:: yaml

  sms:
    default: hasura


* Get your user information.

.. code:: shell

  $ hasura user-info

* Copy the ``Token`` and update it as a secret

.. code:: shell

  $ hasura secrets update notify.hasura.token "<token>"

``Hasura`` is now configured as your default SMS provider. You can start using it to send SMS.

.. note::
  You can only send 10 SMS per day using Hasura.

Sending SMS
^^^^^^^^^^^

.. http:post:: /v1/send/sms
   :noindex:

   **Example request**:

   .. sourcecode:: http

      POST https://notify.<cluster-name>.hasura-app.io/v1/send/sms HTTP/1.1
      Content-Type: application/json
      Authorization: Bearer <admin-token>

      {
        "to": "9876543210",
        "countryCode": "91",
        "message": "This is the body of SMS"
      }

   **Example response**:

   .. sourcecode:: http

      HTTP/1.1 200 OK
      Content-Type: application/json

      {
        "id": "<provider-reference-id>",
        "detail": "<details>"
      }

``Authorization`` header is not required if the request is being made from a
browser, since ``Cookie`` will be set.

.. note::

  If you are writing backend code and want to send SMS, you can directly contact
  the ``Notify`` microservice using the URL
  ``http://notify.hasura/v1/send/sms``. You will also need to set headers
  ``X-Hasura-User-Id: 1`` and ``X-Hasura-User-Role: admin`` to make the request
  as ``admin``.


.. _Sparkpost: https://sparkpost.com
.. _SMTP: https://en.wikipedia.org/wiki/Simple_Mail_Transfer_Protocol
.. _Twilio: https://www.twilio.com/
.. _MSG91: https://msg91.com/
