.. .. meta::
   :description: Reference documentation for integrating Google OAuth2.0 based user signup & login with Hasura's Auth microservice for your web and mobile applications.
   :keywords: hasura, docs, auth, Google signup, Google login, social login, Google OAuth, Google OAuth2.0, integration

Google authentication
=====================

Configuration
~~~~~~~~~~~~~

* To use Google login with Hasura, create a Project on
  https://console.developers.google.com/.  You can know more about using Google
  login from https://developers.google.com/identity/.

* Register an application with Google, obtain the Client Id and Client secret.

* Now you need to configure Hasura Auth microservice with these credentials.

* To configure, go to :doc:`conf/auth.yaml <../../../project/directory-structure/conf/auth.yaml>` file inside your Hasura
  project.

* Under ``google``, set the array of ``clientIds``.

.. note::
     The ``google`` key might be commented out. Make sure to uncomment it if it is.

.. snippet:: yaml
  :filename: auth.yaml

  google:
    clientIds: ["String"]
    clientSecret:
      secretKeyRef:
        key: auth.google.client_secret
        name: hasura-secrets


* **clientIds**: The client IDs obtained when creating the application. This is an array of strings. For example, it could be an array of your web client ID, iOS client ID etc.

* **clientSecret**: The client secret obtained when creating the application. As you see in the above code snippet, client secret is a reference to a :ref:`hasura project secret <hasura-secrets-manual>` called ``auth.google.client_secret``.
  To add your client secret to ``hasura project secrets``, run the following command from your project directory.

  .. code-block:: bash

    $ hasura secret update auth.google.client_secret

* Choose your device and Google SDK from here:
  https://developers.google.com/identity/choose-auth


Web apps
~~~~~~~~

For web apps, use the Google SDK to integrate with Hasura Auth.

See here on how to use it:
https://developers.google.com/identity/sign-in/web/


Mobile apps
~~~~~~~~~~~

For mobile apps, you can use Google SDK to integrate with Hasura Auth.

See here for more information:

* Android: https://developers.google.com/identity/sign-in/android/
* iOS: https://developers.google.com/identity/sign-in/ios/

API
---

Login/Signup
~~~~~~~~~~~~

* Use Google SDK from above to obtain ``id_token`` (or ``idToken``) of the
  logged in Google user. (In some older platforms you might receive
  ``access_token`` (or ``accessToken``). In that case, you can replace
  ``id_token`` with ``access_token`` in the rest of article.)

* Once the ``id_token`` is obtained, send the ``id_token`` to Hasura Auth
  microservice:

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/login HTTP/1.1
   Content-Type: application/json

   {
     "provider" : "google",
     "data" : {
        "id_token": "String",
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
