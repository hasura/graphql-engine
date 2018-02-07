.. meta::
   :description: API reference for Hasura's File microservice. POST, GET and DELETE endpoints for uploading, downloading and deleting files respectively.
   :keywords: hasura, docs, File, fileStore, API reference

API
===

.. _filestore-api-upload:

``POST /v1/file/<file_id>``
---------------------------

The POST endpoint let's you upload files.

You are required to generate a unique ``file_id`` for the file, and the send
the file contents in the request body and setting the ``Content-Type`` header
set to the file's MIME type.

A typical request is as follows:

Request
^^^^^^^

.. code-block:: http

     POST /v1/file/05c40f1e-cdaf-4e29-8976-38c899 HTTP/1.1
     Content-Type: image/png

     <contents of the file>

Response
^^^^^^^^
An example response looks like:

.. code-block:: http

    HTTP/1.1 200 OK
    Content-Type: application/json

    {
      "file_id": "05c40f1e-cdaf-4e29-8976-38c899",
      "content_type": "image/png",
      "file_status": "uploaded",
      "file_size": 351667,
      "user_id": 42,
      "user_role": "user",
      "created_at": "2017-04-25T08:26:22.834266+00:00"
    }

.. _filestore-api-download:

``GET /v1/file/<file_id>``
--------------------------

This endpoint can be used to download the files. The files are streamed to the
client.


Request
^^^^^^^

An example request would look like:


.. code-block:: http

    GET /v1/file/05c40f1e-cdaf-4e29-8976-38c899 HTTP/1.1


Response
^^^^^^^^

The file is streamed to the client with the ``Content-Type`` header set to the
file's MIME type.

.. _filestore-api-delete:

``DELETE /v1/file/<file_id>``
-----------------------------

This endpoint can be used to delete the files. The files are streamed to the
client.


Request
^^^^^^^

An example request would look like:


.. code-block:: http

    DELETE /v1/file/05c40f1e-cdaf-4e29-8976-38c899 HTTP/1.1


Response
^^^^^^^^

An example response looks like:

.. code-block:: http

    HTTP/1.1 200 OK
    Content-Type: application/json

    {
      "file_id": "05c40f1e-cdaf-4e29-8976-38c899",
      "content_type": "image/png",
      "file_status": "uploaded",
      "file_size": 351667,
      "user_id": 42,
      "user_role": "user",
      "created_at": "2017-04-25T08:26:22.834266+00:00"
    }



Errors
------

.. list-table::
   :widths: 10 10 30
   :header-rows: 1

   * - Status code
     - Description
     - Response structure

   * - ``200``
     - Success
     - .. parsed-literal::

          Request specific

   * - ``400``
     - Bad request
     - .. code-block:: haskell

          {
              "path"  : String,
              "error" : String
          }

   * - ``401``
     - Unauthorized
     - .. code-block:: haskell

          {
              "error" : String
          }

   * - ``500``
     - Internal server error
     - .. code-block:: haskell

          {
              "error" : String
          }
