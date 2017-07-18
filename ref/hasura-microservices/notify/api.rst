.. meta::
   :description: API reference for Hasura's File microservice. POST, GET and DELETE endpoinds for uploading, downloading and deleting files respectively.
   :keywords: hasura, docs, File, fileStore, API reference

API
===

``POST /v1/send/email``
---------------------------

This POST endpoint let's you send an email.

A typical request is as follows:

Request
^^^^^^^

.. code-block:: http

     POST /v1/send/email HTTP/1.1
     Content-Type: application/json 

     {
       "to": "Example User <user@example.com>",
       "from": "Project Admin <admin@hasura-project.com>",
       "sub": "This is the email subject line",
       "text": "This is the email content in plain text",
       "html": "This is the email content in html format"
     }

Either **text** or **html** keys should be present. Both can also be provided.

Response
^^^^^^^^
An example response looks like:

.. code-block:: http

    HTTP/1.1 201 Created 
    Content-Type: application/json

    {
      "id": "<provider-reference-id>"
    }


``GET /v1/send/sms``
--------------------------

This endpoint can be used to send SMS.

Request
^^^^^^^

An example request would look like:


.. code-block:: http

     POST /v1/send/sms HTTP/1.1
     Content-Type: application/json 

     {
       "to": 9876543210
       "countryCode": 91, 
       "smsFrom": "MYPROJECT",
       "message": "This is the body of SMS" 
     }

Response
^^^^^^^^

A typical response will look like this:

.. code-block:: http

    HTTP/1.1 201 Created 
    Content-Type: application/json

    {
      "id": "<provider-reference-id>"
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
