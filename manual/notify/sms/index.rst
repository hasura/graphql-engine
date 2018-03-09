.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _hasura-notify-sms:

Sending SMS
===========

This section covers configuring Hasura ``notify`` microservice to send SMS. Notify currently supports sending SMS via `Twilio <https://www.twilio.com>`_ and `MSG91 <https://msg91.com>`_.

Also, for testing out Notify, Hasura has its own test provider called ``Hasura`` which you can use to send 10 SMS per day.

Sending an SMS
--------------

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

  If you are writing backed code and want to send SMS, you can directly contact
  the ``Notify`` microservice using the URL
  ``http://notify.hasura/v1/send/sms``. You will also need to set headers
  ``X-Hasura-User-Id: 1`` and ``X-Hasura-User-Role: admin`` to make the request
  as ``admin``.


.. _Sparkpost: https://sparkpost.com
.. _SMTP: https://en.wikipedia.org/wiki/Simple_Mail_Transfer_Protocol
.. _Twilio: https://www.twilio.com/
.. _MSG91: https://msg91.com/


SMS Providers:
^^^^^^^^^^^^^^

.. toctree::
  :maxdepth: 1

  Hasura Test Provider <hasura-test-provider>
  Twilio <twilio>
  MSG91 <msg91>
