Downloading files
=================

A file can be downloaded using the unique ID that was used to upload it. This ID can be stored as a reference to the file in your own table.

For example, if your application is storing user's profile pictures, this unique ID can simply be the ``hasura_id`` and can be easily referred to again. Another option is to generate a unique string and save it in the user table and later use it to retrieve the file.

API is as follows:

.. http:get:: /v1/file/(file_id)
   :noindex:

   Download/retrieve a file with a given `file_id`. The file is streamed to the client with correct `Content-Type` header.

   **Example request**:

   .. sourcecode:: http

      GET https://filestore.project-name.hasura-app.io/v1/file/05c40f1e-cdaf-4e29-8976-38c899 HTTP/1.1
      Authorization: Bearer <token>

   **Example response**:

   .. sourcecode:: http

      HTTP/1.1 200 OK
      Content-Type: image/png

      <file-stream>

.. note::
    If the permissions webhook is set to ``Public`` on the console. The files will be available to the public, i.e. anyone on the internet with the link can download the file.

``Authorization`` header is not required from a web browser, since ``Cookie`` will be sent.

For more details, see  :ref:`API reference <filestore-api-download>`.


