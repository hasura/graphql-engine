Facebook
========

Web apps
--------

For web apps, one can use the `implicit grant flow (OAuth2.0)`_ and get access
tokens directly on the client-side without writing any backend server code.
This is the recommended way of implementing Facebook login with Hasura.

Overview
++++++++

For Javascript-based web applications, you would have to setup a Facebook
application. Then use the OAuth credentials (app ID and secret etc.)
obtained, to make requests to Facebook APIs and get the access token directly
into the client side application. Then, you have to validate them by passing to
a Hasura Auth endpoint. Hasura will validate the token, and will login the user
(if the user is seen for the first time then Hasura will also create the user).

Pre-requisites
++++++++++++++

* Register an application with Facebook, obtain the App ID and App secret.

  * Go to https://developers.facebook.com/ and from the dropdown menu on
    top right and click on "Add a New App" to create a new app. Select
    "Website" from the platforms option. Follow the on screen options.

  * Once the app is created, you should be able to see a settings wizard. Go
    through it to setup your app. Then from https://developers.facebook.com/,
    you can choose your application from the "My apps" on top right.

  * Go the newly created app's page, and obtain the "App ID" and "App Secret"
    values.

* In case of Facebook, you can directly pass the ``redirect_uri`` to the URL
  in your app. This Redirect URI is the endpoint/URL of your application which
  will receive the access token and parse it.

* Now you need to configure Hasura Auth to tell it to use these credentials.

  To configure, head over to your project's dashboard (Usually,
  https://console.your-project.hasura-app.io). On the left sidebar, click
  on the "Auth" icon. You'll get more options in another sidebar. Click on
  "Facebook" on the left under the "Configure" section.

  * **APP ID**: The app ID obtained when creating the application.

  * **APP SECRET**: The app secret.


The flow
++++++++

* Redirect the browser to
  ``https://www.facebook.com/dialog/oauth`` with the following set of
  query parameters. **Please note** that you have to make the browser redirect
  to this URL and **not** make AJAX calls. The following parameters are
  mandatory:

  * ``client_id`` : The app ID you obtained from your Facebook app settings.

  * ``redirect_uri`` : The URL of your application which will receive the
    access token in the URL and retrieve it.

  * ``response_type`` : This should always be ``token``.

  * ``scope`` : Permissions that your application needs. Set this to ``email``.


  For example::

    https://www.facebook.com/dialog/oauth?
      client_id=812741506391&
      redirect_uri=https%3A%2F%2Foauth2.example.com%2Foauthcallback&
      response_type=token&
      scope=email

  The documentation (in details) for this can be found here:
  https://developers.facebook.com/docs/facebook-login/manually-build-a-login-flow


* Facebook will handle the authentication, and if the user has granted permission
  for your app, it will redirect back to the "Redirect URI" with access token as
  `hash fragment`_ in the URL.

* Now your application has to parse the URL and retrieve the access token.

* Once the access token is retrieved, make a call to Hasura Auth's
  ``/facebook/authenticate``  endpoint to validate the token and then
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
      var url = 'https://auth.<your-project>.hasura-app.io/facebook/authenticate?access_token='
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

Facebook's flow for OAuth is documented in detail here:
https://developers.facebook.com/docs/facebook-login/manually-build-a-login-flow


Hasura Auth API
+++++++++++++++

* To validate the access token and then log the user in (and create if not
  exists), make a call to
  ``/facebook/authenticate?access_token=<ACCESS-TOKEN>``

* To check if the current user is logged in, make a call to:
  ``/user/account/info``.

* To logout, make a call to ``/user/logout``.

* To get Hasura credentials of current logged in user, ``/user/account/info``.

Read the API docs to know more about Hasura Auth endpoints
https://hasura.io/_docs/auth/4.0/swagger-ui/.


Mobile apps
-----------

For setting up your mobile application for Facebook sign-in, first follow these
instructions: https://developers.facebook.com/docs/facebook-login/android.

In the above example, in the section "Access Tokens and Profiles" there are
examples of how to get the access token (basically call
``AccessToken.getCurrentAccessToken()``). Once you obtain the access token, you
need to pass this ``access_token`` to Hasura Auth for validating and
login/creating the user. In the Hasura SDK, in the ``auth`` module, use the
``socialAuth`` method for authenticating.

See the Android SDK docs for more details.


.. _hash fragment: https://en.wikipedia.org/wiki/Fragment_identifier
.. _implicit grant flow (OAuth2.0): http://tools.ietf.org/html/rfc6749#section-4.2
