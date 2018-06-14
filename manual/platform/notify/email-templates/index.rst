.. Hasura Platform documentation master file, created by
   sphinx-quickstart on Thu Jun 30 19:38:30 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _hasura-notify-email-templates:

Sending email templates
=======================

This section covers configuring Hasura ``notify`` microservice to send email templates. Notify currently supports sending email templates via :doc:`SparkPost <sparkpost>` and :doc:`Mandrill <mandrill>`.

Sending an email template
-------------------------

Before sending email templates, you must have configured one of the providers for sending Email Templates.

.. note::

  ``Notify`` is an admin-only microservice. Hence, only users with ``admin`` role
  will be able to access these APIs

Send an email as per the options given is request body.

.. code-block:: http

  POST https://notify.<cluster-name>.hasura-app.io/v1/send/email-template HTTP/1.1
  Content-Type: application/json
  Authorization: Bearer <auth-token> # optional if cookie is set
  X-Hasura-Role: <role>  # optional. Pass if only specific user role has access

  {
    "to": "Example User <user@example.com>",
    "fromEmail": "admin@project.com",
    "fromName": "Project Admin",
    "templateName": "This is name of the template",
    "templateContent": [
        { 
          "name": "name of the template variable",
          "content": "value of the variable"
        }
      ]
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
  ``http://notify.hasura/v1/send/email-template``. You will also need to set headers
  ``X-Hasura-User-Id: 1`` and ``X-Hasura-User-Role: admin`` to make the request
  as ``admin``.

Email Template providers:
^^^^^^^^^^^^^^^^^^^^^^^^^

.. toctree::
  :maxdepth: 1

  SparkPost <sparkpost>
  Mandrill <mandrill>
