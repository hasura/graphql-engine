.. .. meta::
  :description: Learn how to customize Hasura Auth
  :keywords: hasura, auth, customize, signup, login, hooks

Customizing authorization flow
==============================
You might want to customize the authorization flow during signup or login to do
add any additional checks, add roles dynamically to the user etc.

For example, if you are using the ``username`` provider you might have a regex
to validate usernames, or if you are using the ``email`` provider you might want
to validate if emails belong to a particular domain.

Platform version ``v0.15.34`` onwards you can customize the authorization flow
in signup or login. This is done by adding webhooks into the auth system.

Authorization flow (signup/login)
---------------------------------
1. Auth recieves signup/login request from the client.
2. Auth forwards the entire request from client to the webhook, if configured.
3. If Auth gets back any succesful response then it continues with the flow,
   else aborts it returning the webhook response to the client.

This webhook authorization flow works for all providers, including Hasura's
default providers and your custom providers.

Spec for the webhook
--------------------

Request payload
^^^^^^^^^^^^^^^
Your webhook will receive the request in following format, received from the client:

.. code-block:: json

   {
     "provider": "<name-of-the-provider>",
     "data": <a-json-object>
   }

- ``provider``: the name of the provider (see :doc:`../authentication/index` for more details)
- ``data``: payload containing relevant fields based on the provider. If the
  client sends any extra fields, that is also passed along.

How to respond
^^^^^^^^^^^^^^

Success
+++++++
Successful reponse means the authorization is successful and auth will continue
with signup/login flow. Any 2xx response is considered successful.

During the signup flow, along with 2xx status code, you can send a list of roles
in the reponse body. If a list of roles is found, then auth will assign those
roles to the user along with the ``defaultRoles`` defined in the conf for that
provider.

.. code-block:: http

   HTTP/1.1 200 OK
   Content-Type: application/json

   {"roles": ["merchant"]}

Failure
+++++++
Failure reponse means the authorization is unsuccessful, and the request should
be rejected. On a failure reponse, auth will abort the flow and return an error
to the client. Any 4xx response is considered failure response.

In the case of failure, send the following JSON format in the response body.
This response will be proxied as is to the client:

.. code-block:: json

   {
     "code": "<code>",
     "message": "<message>",
     "detail": <optional-any-valid-json-value-string-object>
   }

- ``code`` is an error code in string (useful for client applications to handle errors).
- ``message`` is a short error message in string.
- ``detail`` is an optional field, which can be any valid JSON. (It can be a string or even a nested object)

For example a valid failure response is:

.. code-block:: http

   HTTP/1.1 403 Forbidden
   Content-Type: application/json

   {
     "code": "invalid-email",
     "message": "Only users with company.com domain are allowed"
   }


Configuring the webhooks
------------------------
Configure the webhooks in ``conf/auth.yaml``. See :doc:`../../project/directory-structure/conf/auth.yaml`

The conf looks like:

.. code-block:: yaml

   authorizationHooks:
     preSignupHook: 'http://myapp.default/signup-hook'
     preLoginHook: 'http://myapp.default/login-hook'

``authorizationHooks`` is an optional field.

``preSignupHook`` and ``preLoginHook`` each of them are optional fields as well. You can have only one of them.

``preSignupHook`` and ``preLoginHook`` each point to the URL of the webhook.
