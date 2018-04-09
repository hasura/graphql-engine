.. .. meta::
   :description: Reference documentation for integrating Facebook OAuth2.0 based user signup & login with Hasura's Auth microservice for your web and mobile applications.
   :keywords: hasura, docs, auth, facebook signup, facebook login, social login, facebook OAuth, facebook OAuth2.0, integration

Facebook authentication
=======================

Setup
-----

Configuration
~~~~~~~~~~~~~

* To use Facebook login with Hasura, create an App on Facebook from
  https://developers.facebook.com/.

* In the newly created app's page, and obtain the "App ID" and "App Secret"
  values.

* Now you need to configure Hasura Auth microservice with these credentials.

* To configure, go to :doc:`conf/auth.yaml <../../../project/directory-structure/conf/auth.yaml>` file inside your Hasura
  project.

* Under ``facebook``, set your ``clientId``.

    Note: The ``facebook`` key might be commented out. Make sure to uncomment it.

.. code-block:: yaml

      facebook:
        clientId: "String"
        clientSecret:
          secretKeyRef:
            key: auth.facebook.client_secret
            name: hasura-secrets

* **clientId**: The app ID obtained when creating the application.

* **clientSecret**: The app secret obtained when creating the application. As you see in the above code snippet, client secret is a reference to a :ref:`hasura project secret <hasura-secrets-manual>` called ``auth.facebook.client_secret``.
  To add your client secret to ``hasura project secrets``, run the following command from your project directory.

  .. code-block:: bash

    $ hasura secret update auth.facebook.client_secret

Web apps
~~~~~~~~

For web apps, use the Facebook Login for Web Javascript SDK to integrate with
Hasura Auth.

See here on how to use it:
https://developers.facebook.com/docs/facebook-login/web/

Mobile apps
~~~~~~~~~~~

For mobile apps, you Facebook mobile SDKs to integrate with Hasura Auth.

See here on how to use it:

* Android: https://developers.facebook.com/docs/facebook-login/android/
* iOS: https://developers.facebook.com/docs/facebook-login/ios/


API
---

Login/Signup
~~~~~~~~~~~~

* Use Facebook SDK from above to obtain ``accessToken`` (or ``access_token``)
  of the logged in Facebook user.

* Once the ``access_token`` is obtained, send the ``access_token`` to Hasura Auth
  microservice:

  .. code:: http

   POST auth.<cluster-name>.hasura-app.io/v1/login HTTP/1.1
   Content-Type: application/json

   {
     "provider" : "facebook",
     "data" : {
        "access_token": "String",
     }
   }


* If successful, this will return a response as follows:

  .. code:: http

    HTTP/1.1 200 OK
    Content-Type: application/json

    {
      "auth_token": "b4b345f980ai4acua671ac7r1c37f285f8f62e29f5090306",
      "hasura_id": 79,
      "new_user": true,
      "hasura_roles": [
          "user"
      ]
    }


* If the user is a new user, ``new_user`` will be true, else false.

* To check if the current user is logged in, make a call to: ``/v1/user/info``.

* To logout, make a call to ``/v1/user/logout``.

* To get Hasura credentials of current logged in user, ``/v1/user/info``.
