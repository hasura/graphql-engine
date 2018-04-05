Filestore authorization webhooks
================================

Filestore needs to authorize if the current user is allowed to call a ``Filestore``
API (say to upload or download a file). This authorization is done via a
webhook.

By default, only the ``admin`` role has access to the ``Filestore`` APIs, for other
roles the webhook has to be configured.

The Filestore microservice contacts the webhook with the following parameters:

1. The File ID: the unique id of the file.
2. The operation on the file: one of ``create``, ``read``, ``delete``.

The user's information is passed in the request headers as
``X-Hasura-User-Id``, ``X-Hasura-User-Role`` and ``X-Hasura-Allowed-Roles``.

.. note::

    The headers ``X-Hasura-User-Id``, ``X-Hasura-User-Role`` and
    ``X-Hasura-Allowed-Roles`` are a Hasura platform feature to enable access
    control. To know more about it read :doc:`here <../gateway/session-middleware>`.


Webhook response
----------------

If the webhook returns a ``200`` response then the authorization is granted,
and if it returns ``403`` response, then the file operation is denied.

.. list-table::
   :widths: 10 10 30
   :header-rows: 1

   * - Status code
     - Authorization action
     - Description

   * - ``200``
     - Access granted
     - Success

   * - ``403``
     - Access denied
     - The message returned by the webhook is sent to the client

   * - Anything else
     - Error
     - Internal error is thrown


Setting up the webhook
----------------------

As a user, you are supposed to provide the ``Filestore`` microservice with a webhook, which is a
HTTP URL running inside the Hasura cluster.

Which means, you need to write a custom endpoint which receives the above
parameters from the ``File`` microservice, perform required authorization checks and return
a response. You can deploy this custom endpoint as a custom  microservice.

Let's say the webhook that you have deployed is available at
``http://filecheck.<cluster-name>.hasura-app.io/check``. Internally,
this endpoint will be available at ``filecheck.default/check``. Then the
``File`` microservice will call your API to authorize before doing the actual file
operation, like so:

.. code-block:: http

    GET filecheck.default/check?file_id=<file-id>&file_op=<operation> HTTP/1.1


Where ``operation`` is one of ``create``, ``read``, ``delete``.

Once you get this information about the file and the file operation, and
retrieve the user and roles information from the Hasura headers, you should
perform the required authorization checks. If you want to grant the access to
the user then the webhook should return a ``200`` response. If you want to deny
access to the user then your webhook should return a ``403`` response.

