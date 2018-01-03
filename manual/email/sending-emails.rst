Sending an email
----------------

Before sending emails, you must have configured one of the providers for sending Emails.

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

  If you are writing backed code and want to send email from that code, you can
  directly contact the ``Notify`` microservice using the URL
  ``http://notify.hasura/v1/send/email``. You will also need to set headers
  ``X-Hasura-User-Id: 1`` and ``X-Hasura-User-Role: admin`` to make the request
  as ``admin``.

