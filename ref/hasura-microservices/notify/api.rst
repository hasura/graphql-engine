.. meta::
   :description: API reference for Hasura's File microservice. POST, GET and DELETE endpoints for uploading, downloading and deleting files respectively.
   :keywords: hasura, docs, File, fileStore, API reference

API
===

``POST /v1/send/email``
---------------------------

This POST endpoint lets you send an email.

A typical request is as follows:

Request
^^^^^^^

.. code-block:: http

     POST /v1/send/email HTTP/1.1
     Content-Type: application/json 

     {
       "to": "Example User <user@example.com>",
       "from": "admin@project.com",
       "fromName": "Project Admin", 
       "sub": "This is the email subject line",
       "text": "This is the email content in plain text",
       "html": "<p>This is the <b>email content</b> in html format</p>"
     }

.. note :: 
  
  * Make sure that the *from* field you will be using to send emails is from the sending domain you have configured with the SparkPost or the exact email for which you are using SMTP with. For example, if your sending domain is ``project.com``, you can only send emails from ``<something>@project.com``, like ``admin@project.com``.

  * Either **text** or **html** key should be present. Both can also be provided.
  * **fromName** is optional.

Response
^^^^^^^^
An example response looks like:

.. code-block:: http

    HTTP/1.1 200 OK 
    Content-Type: application/json

    {
      "id": "<provider-reference-id>",
      "detail": "<details>"
    }

.. note ::
  
  For SMTP email provider, the ``provider-reference-id`` will be ``NA``.

``POST /v1/send/sms``
--------------------------

This endpoint can be used to send SMS.

Request
^^^^^^^

An example request to send an SMS to +91-9876543210 would look like:


.. code-block:: http

     POST /v1/send/sms HTTP/1.1
     Content-Type: application/json 

     {
       "to": 9876543210,
       "countryCode": 91, 
       "message": "This is the body of SMS" 
     }

Response
^^^^^^^^

A typical response will look like this:

.. code-block:: http

    HTTP/1.1 200 OK 
    Content-Type: application/json

    {
      "id": "<provider-reference-id>",
      "detail": "<details>"
    }


Errors
------

.. list-table::
   :widths: 10 10 30
   :header-rows: 1

   * - Status code
     - Description
     - Response structure

   * - ``<status-code>``
     -  Status
     - .. parsed-literal::

          {
              "code"  : String,
              "message" : String
          }
