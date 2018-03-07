Upserting files
===============

The unique file_id is used to update an uploaded file or create a file if file_id does not exist.


.. http:put:: /v1/file/(file_id)
   :noindex:

   Upsert a file with a given `file_id`.

   **Example request**:

   .. sourcecode:: http

      PUT https://filestore.project-name.hasura-app.io/v1/file/05c40f1e-cdaf-4e29-8976-38c899 HTTP/1.1
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

