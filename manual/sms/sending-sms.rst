Sending SMS
-----------

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

