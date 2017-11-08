.. .. meta::
   :description: Manual for using Notify Microservice on Hasura. Notify is used to send email or sms from Hasura project to users through providers like SMTP, Spark Post, Twilio, MSG91. 
   :keywords: hasura, docs, notify, email, sms, smtp, sparkpost, twilio, msg91

Email & SMS
===========

The ``Notify`` service on Hasura lets you contact / notify your project users through Email and SMS.

.. note::

  This is an admin only micro-service. Only those users with ``admin`` role associated to them can access the API.

Email
-----

Any one of the following providers can be configured and enabled for sending emails. The simplest way to send emails is by configuring ``SMTP``. Your existing email address (like GMail, Yahoo, Live.com etc.) can be used to send emails via SMTP.  

Configuring SMTP
^^^^^^^^^^^^^^^^

You need to configure the following options in notify.yaml in cluter/<clusterName> directory:

* Under **email** -> **providers** -> **smtpConf**

.. code-block:: yaml

      email:
        providers:
          smtpConf:
            smtpHostname: "String"
            smtpPort: Int
            smtpUsername: "String"
            smtpPassword: "String"

* **smtpHostname**: SMTP hostname for email provider (e.g. smtp.gmail.com).
* **smtpPort**: SMTP port number (e.g. 465) (Notify only supports SSL authentication as of now.)
* **smtpUsername**: SMTP username (e.g. yourusername@gmail.com)
* **smtpPassword**: SMTP password (e.g. yourgmailpassword)

.. note ::

  If you are using GMail as your SMTP provider, you have to enable access to "Less Secure Apps" on your Google account. You can get a full explanation and how to enable it `here <https://support.google.com/accounts/answer/6010255>`_.
  GMail also enforces limits on emails sent (default 500/day).

.. note ::

  For Hasura projects on Google Compute Engine / Google Container Engine, SMTP settings with standard ports like 25, 465, 587 will not work, since Google Compute Engine does not allow outbound connections on these ports. Hence, make sure that your SMTP provider have alternate ports like 2525 in case you want to deploy on Google Cloud. You can find more details and possible solutions `here <https://cloud.google.com/compute/docs/tutorials/sending-mail/>`_.

Configuring SparkPost
^^^^^^^^^^^^^^^^^^^^^

To start sending emails using SparkPost, `signup <https://www.sparkpost.com/>`_ for an account and create a sending domain. You have to obtain an API key after verifying the sending domain. SparkPost provides helpful interface to guide through the process. SparkPost's free plan should be enough to cover your initial email volume.

.. note::

  You have to own a domain for completing SparkPost setup.

Please remember that you have to setup SPF and DKIM records for your domain
to start sending emails from your domain. Otherwise, SparkPost will
reject sending emails.

Here are some SparkPost resources for `verifying sending domain <https://support.sparkpost.com/customer/portal/articles/1933360-verify-sending-domains>`_ and setting up `SPF/DKIM: <https://www.sparkpost.com/blog/understanding-spf-and-dkim-in-sixth-grade-english/>`_

You need to configure the following options in notify.yaml in cluter/<clusterName> directory:

* Under **email** -> **providers** -> **sparkPostConf**

.. code-block:: yaml    git push --set-upstream origin v0.15_notify_docs


      email:
        providers:
          sparkPostConf:
            sparkKey: "String"

* **sparkKey**: The API key obtained from your SparkPost account, for the sending domain. Make sure that you have checked Transmissions: Read/Write in API Permission section on SparkPost while creating the API key.

Sending an email
^^^^^^^^^^^^^^^^

.. note::
   ``Notify`` is an admin-only microservice. Hence, only users with ``admin`` role will be able to access these APIs

.. http:post:: /v1/send/email
   :noindex:

   Send an email as per the options given is request body.

   **Example request**:

   .. sourcecode:: http

      POST https://notify.project-name.hasura-app.io/v1/send/email HTTP/1.1
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

   .. sourcecode:: http

      HTTP/1.1 200 OK
      Content-Type: application/json

      {
        "id": "<provider-reference-id>",
        "detail": "<details>"
      }

``Authorization`` header is not required if the request is being made from a browser, since ``Cookie`` will be set.

.. note:: If you are writing backed code and want to send email from that code, you can directly contact the ``Notify`` service using the URL ``http://notify.hasura/v1/send/email``. You will also need to set headers ``X-Hasura-User-Id: 1`` and ``X-Hasura-User-Role: admin`` to make the request as ``admin``.

SMS
---
You can configure either ``Twilio`` or ``MSG91`` for sending SMS.

Configuring Twilio
^^^^^^^^^^^^^^^^^^

`Twilio <https://www.twilio.com>`_ is a SMS, Voice and Video provider. You can `signup <https://www.twilio.com/try-twilio>`_ for a free account and start sending SMS using Twilio. Once you have signed up and created a Twilio phone number, grab the *Account SID* and *Auth Token* from `Twilio Console <https://www.twilio.com/console/account/settings>`_.

You need to configure the following options in notify.yaml in cluter/<clusterName> directory:

* Under **sms** -> **providers** -> **twilioConf**

.. code-block:: yaml

      sms:
        providers:
          twilioConf:
            accountSid: "String"
            authToken: "String"
            twilioFrom: "String"

* **accountSid**: Unique identifier for your account, obtained from the API Credentials section of Twilio Console.
* **authToken**: Password like secret key for the account.
* **twilioFrom**: Sender number obtained from Twilio Console. Shows up as *From* at receiver's end.


Configuring MSG91
^^^^^^^^^^^^^^^^^

`MSG91 <https://msg91.com/>`_ is a SMS provider where you can `signup <https://msg91.com/signup>`_ and get an API key to use with Hasura. Take a look at `this <http://help.msg91.com/article/177-where-can-i-find-my-authentication-key>`_ guide by MSG91 to obtain your API key.

You need to configure the following options in notify.yaml in cluter/<clusterName> directory:

* Under **sms** -> **providers** -> **msg91Conf**

.. code-block:: yaml

      sms:
        providers:
          msg91Conf:
            msg91Key: "String"
            msg91From: "String"

* **msg91Key**: Unique identifier obtained from MSG91 Dashboard.
* **msg91From**: Sender identification (maximum 6 characters) from which the SMS will be sent.

Sending SMS
^^^^^^^^^^^

.. http:post:: /v1/send/sms
   :noindex:

   **Example request**:

   .. sourcecode:: http

      POST https://notify.project-name.hasura-app.io/v1/send/sms HTTP/1.1
      Content-Type: application/json 
      Authorization: Bearer <admin-token>

      {
        "to": 9876543210,
        "countryCode": 91,
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

``Authorization`` header is not required if the request is being made from a browser, since ``Cookie`` will be set.

.. note:: If you are writing backed code and want to send SMS, you can directly contact the ``Notify`` service using the URL ``http://notify.hasura/v1/send/sms``. You will also need to set headers ``X-Hasura-User-Id: 1`` and ``X-Hasura-User-Role: admin`` to make the request as ``admin``.
