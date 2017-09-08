.. meta::
   :description: Manual for using File Microservice on Hasura. The service lets users to upload and download files with access controls.   
   :keywords: hasura, docs, fileStore, file, file upload, file download

Files
=====

File APIs on Hasura lets users upload and store files on a Hasura project and also download when required. The API exposes upload, download and delete methods as well as provide permission options based on user's ID or Role to decide who can create, read or delete files.

Uploading a file
----------------
One of the basic usage of File API is to upload and store a file for later retrieval. The API is described as follows: 


.. http:post:: /v1/file/(file_id)

   Upload a file with a given `file_id`. You are required to generate a `file_id` and post the content of file in request body with the correct MIME type set as `Content-Type` header. 

   **Example request**:

   .. sourcecode:: http

      POST https://filestore.project-name.hasura-app.io/v1/file/05c40f1e-cdaf-4e29-8976-38c899 HTTP/1.1
      Content-Type: image/png
      Authorization: Bearer <token>

      <content-of-file-as-body>

   **Example response**:

   .. sourcecode:: http

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

``Authorization`` header is not required if the request is being made from a browser, since ``Cookie`` will be set.

.. note::
   By default, File APIs are only accessible to those users with ``admin`` role. Goto Permissions tab under File Service in the console and enable ``Private`` webhook to enable ``user`` role also to upload and download files. This default permissions are based only on role and not on user ID. Hence, anybody with a ``user`` role can download a file another user has uploaded. For a more fine grained access control, refer to :ref:`Custom Webhooks <filestore-authz-webhooks>`. 

Uploaded files will be visible on the console.

For more details, see  :ref:`API reference <filestore-api-upload>`.

Downloading a file
------------------
A file can be downloaded using the unique ID that was used to upload it. This ID can be stored as a reference to the file in your own table.

For example, if your application is storing user's profile pictures, this unique ID can simply be the ``hasura_id`` and can be easily referred to again. Another option is to generate a unique string and save it in the user table and later use it to retrieve the file.

API is as follows:

.. http:get:: /v1/file/(file_id)

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

Deleting a file
---------------
The unique file_id is used to delete an uploaded file.

.. http:delete:: /v1/file/(file_id)

   Delete a file with a given `file_id`.

   **Example request**:

   .. sourcecode:: http

      GET https://filestore.project-name.hasura-app.io/v1/file/05c40f1e-cdaf-4e29-8976-38c899 HTTP/1.1
      Authorization: Bearer <token>

   **Example response**:

   .. sourcecode:: http

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

.. note::
    If the permissions webhook is set to ``Public`` on the console. The files will be available to the public, i.e. anyone on the internet with the link can download the file.

``Authorization`` header is not required from a web browser, since ``Cookie`` will be sent.

For more details, see :ref:`API reference <filestore-api-delete>`.

Permission Webhooks
-------------------

The following options are available on the console for configuring webhooks:

- Private: Only logged in users can read and upload
- Public: Anybody can read, but only logged in users can upload
- Custom Permission URL: For any other custom permissions, you need to define your own service. Refer to :ref:`Authorization webhooks <filestore-authz-webhooks>` to see how this can be done.

