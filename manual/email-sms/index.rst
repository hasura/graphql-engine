.. meta::
   :description: Manual for using Notify Microservice on Hasura. Notify is used to send email or sms from Hasura project to users through providers like SMTP, Spark Post, Twilio, MSG91. 
   :keywords: hasura, docs, notify, email, sms, smtp, sparkpost, twilio, msg91

Email & SMS
===========
For sending email or sms notifications to users from your Hasura project, you can use Notify service. The simplest way to send emails is by configuring ``SMTP``. Your existing email address (like GMail, Yahoo, Live.com etc.) can be used to send emails via SMTP.  

Email
-----
Configuring SMTP
^^^^^^^^^^^^^^^^

You need to configure the following options on the Hasura project console under Notify email settings:

* **Hostname**: SMTP hostname for email provider (e.g. smtp.gmail.com)
* **Port (SSL)**: SMTP port number (e.g. 465) [``Notify`` only supports SSL authentication as of now. TLS is on the roadmap.]
* **Username**: SMTP username (e.g. yourusername@gmail.com) 
* **Password**: SMTP password (e.g. yourgmailpassword)

.. note ::

  If you are using GMail as your SMTP provider, you have to enable access to "Less Secure Apps" on your Google account. You can get a full explanation and how to enable it `here <https://support.google.com/accounts/answer/6010255>`_.
  GMail also enforces limits on emails sent (default 500/day).

SparkPost
^^^^^^^^^

If you already own a domain, configuring ``SparkPost`` would be better. You will have to register for an account on SparkPost and add an API key to the console.

For more details, please refer to a detailed guide :ref:`here <notify-sparkpost-settings>`.

Sending an email
^^^^^^^^^^^^^^^^

.. note::
   ``Notify`` is an admin-only microservice. Hence, only users with ``admin`` role will be able to access these APIs

.. http:post:: /v1/send/email

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
You can configure either ``Twilio`` or ``MSG91`` for sending SMS. Refer to a detailed guide here for further instructions.

Sending SMS
^^^^^^^^^^^

.. http:post:: /v1/send/sms

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
