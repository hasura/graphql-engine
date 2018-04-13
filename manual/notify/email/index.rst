.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _hasura-notify-email:

Sending emails
==============

This section covers configuring Hasura ``notify`` microservice to send emails. Notify currently supports sending emails via :doc:`SparkPost <sparkpost>` and :doc:`SMTP <smtp>`.

Also, for testing out Notify, Hasura has its own test provider called :doc:`Hasura <hasura-test-provider>` which you can use to send 10 emails per day.

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
  Authorization: Bearer <auth-token> # optional if cookie is set
  X-Hasura-Role: <role>  # optional. Required if request needs particular user role

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

Email providers:
^^^^^^^^^^^^^^^^

.. toctree::
  :maxdepth: 1

  SparkPost <sparkpost>
  SMTP <smtp>
  Hasura Test Provider <hasura-test-provider>
