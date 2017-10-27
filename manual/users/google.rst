.. meta::
   :description: Reference documentation for integrating Google OAuth2.0 based user signup & login with Hasura's Auth service for your web and mobile applications.
   :keywords: hasura, docs, auth, Google signup, Google login, social login, Google OAuth, Google OAuth2.0, integration

Google Provider
===============


Pre-requisites
--------------

* To use Google login with Hasura, create an Project on on
  https://console.developers.google.com/.  You can know more about using Google
  login from https://developers.google.com/identity/.

* Register an application with Google, obtain the Client Id and Client secret.

* Now you need to configure Hasura Auth service with these credentials.

* To configure, go to ``auth.yaml`` in ``conf`` directory inside your Hasura
  project.

* Under ``google``, set the array of ``clientIds``

.. code-block:: yaml

      google:
        clientIds: ["String"]


* **clientId**: The client IDs obtained when creating the application.

* Choose your device and Google SDK from here:
  https://developers.google.com/identity/choose-auth


Web apps
--------

For web apps, use the Google SDK to integrate with Hasura Auth.

See here on how to use it:
https://developers.google.com/identity/sign-in/web/


Mobile apps
-----------

For mobile apps, you can use Google SDK to integrate with Hasura Auth.

See here for more information:

* Android: https://developers.google.com/identity/sign-in/android/
* iOS: https://developers.google.com/identity/sign-in/ios/


Login/Signup a user with Hasura Auth
------------------------------------

* Use Google SDK from above to obtain ``id_token`` (or ``idToken``) of the
  logged in Google user. (In some older platforms you might receive
  ``access_token`` (or ``accessToken``). In that case, you can replace
  ``id_token`` with ``access_token`` in the rest of article.)

* Once the ``id_token`` is obtained, send the ``id_token`` to Hasura Auth
  service:

.. code-block:: http

   POST auth.<project-name>.hasura-app.io/v2/signup HTTP/1.1
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
      "auth_token": "tpdq0m9whrj7i4vcjn48zq43bqx2",
      "hasura_roles": [
        "user"
      ],
      "hasura_id": 79,
      "new_user": true
    }


* If the user is a new user, ``new_user`` will be true, else false.

* To check if the current user is logged in, make a call to: ``/v1/user/info``.

* To logout, make a call to ``/v1/user/logout``.

* To get Hasura credentials of current logged in user, ``/v1/user/info``.
