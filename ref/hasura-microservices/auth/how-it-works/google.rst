.. meta::
   :description: Reference documentation for integrating Google OAuth2.0 based user signup & login with Hasura's Auth service for your web and mobile applications.
   :keywords: hasura, docs, auth, Google signup, Google login, social login, Google OAuth, Google OAuth2.0, integration

Google
======

Web apps
--------

For web apps, one can use the `implicit grant flow (OAuth2.0)`_ and get access
tokens directly on the client-side without writing any backend server code.
This is the recommended way of implementing Google login with Hasura.

Overview
++++++++

For Javascript-based web applications, you would have to setup a Google
application. Then use the OAuth credentials (client ID and secret etc.)
obtained, to make requests to Google APIs and get the access token directly
into the client side application. Then, you have to validate them by passing to
a Hasura Auth endpoint. Hasura will validate the token, and will login the user
(if the user is seen for the first time then Hasura will also create the user).

Pre-requisites
++++++++++++++

* Register an application with Google, obtain the Client ID and Client secret.

  * Go to https://console.developers.google.com/ and from the dropdown menu on
    top right and click on "Create a project" to create a project.

  * Once the project is created, you should be able to see a "Credentials" link
    on the left. Click on it.

  * Then from the main screen, click on "Create credentials". On the popup
    option choose "OAuth client ID". Follow the next on screen options to create
    your credentials. Choose your application type to be "Web application".

* In the next screen, it will ask you to enter the "Authorized redirect
  URIs". This is where you will put your application's endpoint/URL which
  will receive the access token and parse it.

* Then click on "Create". After the creation, it will show your client ID and
  secret. Copy them and keep them safe.

* Now you need to configure Hasura Auth to tell it to use these credentials.

  To configure, head over to your project's dashboard (Usually,
  https://console.your-project.hasura-app.io). On the left sidebar, click
  on the "Auth" icon. You'll get more options in another sidebar. Click on
  "Google" on the left under the "Configure" section.

  * **CLIENT ID**: The client ID obtained when creating the application.

  * **CLIENT SECRET**: The client secret.


The flow
++++++++

* Redirect the browser (full page or popup) to
  ``https://accounts.google.com/o/oauth2/v2/auth`` with the following set of
  query parameters. **Please note** that you have to make the browser redirect
  to this URL and **not** make AJAX calls. The following parameters are
  mandatory:

  * ``response_type`` : This should always be ``token``.

  * ``client_id`` : The client ID you obtain from your Google project settings.

  * ``scope`` : Permissions that your application needs. Set this to ``email``.

  * ``redirect_uri`` : The ``redirect_uri`` value you have already setup (from
    Step 2 in pre-requisites).

  For example::

    https://accounts.google.com/o/oauth2/v2/auth?
      scope=email&
      redirect_uri=https%3A%2F%2Foauth2.example.com%2Foauthcallback&
      response_type=token&
      client_id=812741506391.apps.googleusercontent.com&
      nonce=DgkRrHXmyu3KLd0KDdfq

  The documentation (in details) for this can be found here:
  https://developers.google.com/identity/protocols/OAuth2UserAgent#formingtheurl


* Google will handle the authentication, and if the user has granted permission
  for your app, it will redirect back to the "Redirect URI" with access token as
  `hash fragment`_ in the URL.

* Now your application has to parse the URL and retrieve the access token.

* Once the access token is retrieved, make a call to Hasura Auth's
  ``/google/authenticate``  endpoint to validate the token and then
  create/login the user. The response from Hasura will also indicate if this
  user is a newly created user or an old user (via the ``new_user`` attribute
  in the response).

Below is a JavaScript snippet that parses the response and passes the
access token to Hasura Auth for validation and user login/creation.

.. code-block:: javascript

    // First, parse the query string
    var params = {}, queryString = location.hash.substring(1),
        regex = /([^&=]+)=([^&]*)/g, m;
    while (m = regex.exec(queryString)) {
      params[decodeURIComponent(m[1])] = decodeURIComponent(m[2]);
    }

    // Verify that the access_token is in the response
    if (params['access_token']) {
      // And send the token over to the server
      var url = 'https://auth.<your-project>.hasura-app.io/google/authenticate?access_token='
        + params['access_token'];
      $.get(url, function(response) {
        // Sample response:
        // {
        //   "hasura_id": 23,
        //   "hasura_roles": ["user"],
        //   "auth_token": "aeo8u3dhauh3d39pdsiaw",
        //   "new_user": true
        // }
        console.log(response);
      });
    }

Google's flow for OAuth is documented in detail here:
https://developers.google.com/identity/protocols/OAuth2UserAgent

Hasura Auth API
+++++++++++++++

* To validate the access token and then log the user in (and create if not
  exists), make a call to
  ``/google/authenticate?access_token=<ACCESS-TOKEN>``

* To check if the current user is logged in, make a call to:
  ``/user/account/info``.

* To logout, make a call to ``/user/logout``.

* To get Hasura credentials of current logged in user, ``/user/account/info``.

Read the API docs to know more about Hasura Auth endpoints
https://hasura.io/_docs/auth/4.0/swagger-ui/.


Mobile apps
-----------

For setting up your mobile application for Google sign-in, first follow these
instructions:

* https://developers.google.com/identity/sign-in/android/start-integrating
* https://developers.google.com/identity/sign-in/android/sign-in

By the end of the example in the second link, you would have got the
``GoogleSignInAccount`` object from Google. Call the ``getIdToken()`` method on
this object to get an ``id_token``.  You need to pass this ``id_token`` to
Hasura Auth for validating and login/creating the user. In the Hasura SDK, in
the ``auth`` module, use the ``socialAuth`` method for authenticating.

See the Android SDK docs for more details.


.. _hash fragment: https://en.wikipedia.org/wiki/Fragment_identifier
.. _implicit grant flow (OAuth2.0): http://tools.ietf.org/html/rfc6749#section-4.2
