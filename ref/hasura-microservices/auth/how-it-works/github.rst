Github
======

Web apps
--------

For web apps using Github, we have to use the "Authorization Code" grant flow
(or the traditional web server flow) as Github doesn't support the `implicit
grant flow`_. So this flow needs a backend server to handle intermediate
requests.

Overview
++++++++

First, you have setup and configure your Github application.  Then, you have
to make a request to Github API for an "authorization code", specifying a
"Redirect URI" (which is basically a URL to your application). Github would
respond to this "Redirect URI" with the "authorization code". Your backend
application has to parse the authorization code and make one more request to
Github API. This final API call will return an access token in the response.
Your application should now validate the access token by passing to a Hasura
Auth endpoint. Hasura will validate the token, and will login the user (if the
user is seen for the first time then Hasura will also create the user).

Pre-requisites
++++++++++++++

* Register an application with Github, obtain the Client ID and Client secret.

  * Go to https://github.com/settings/developers and on top right and click on
    "Register a new application" to create a new application. Fill the
    following form that appears to create your application.

  * In the form, you have to add "Authorized Redirect URLs". This is where you
    will put your application's endpoint/URL which will receive the
    authorization code.

  * Now you should see your application's settings page, which also lists the
    client ID and secret. Copy and keep them safe.

* Now you need to configure Hasura Auth to tell it to use these credentials.

  To configure, head over to your project's dashboard (Usually,
  https://console.your-project.hasura-app.io). On the left sidebar, click on
  the "Auth" icon. You'll get more options in another sidebar. Click on
  "Github" on the left under the "Configure" section.

  * **CLIENT ID**: The client ID obtained when creating the application.

  * **CLIENT SECRET**: The client secret.

The flow
++++++++

* Redirect the browser (full page or popup) to
  ``https://github.com/login/oauth/authorize`` with the following set of
  query parameters. **Please note** that you have to make the browser redirect
  to this URL and **not** make AJAX calls. The following parameters are
  mandatory:

  * ``client_id`` : The client ID you obtain from your Github application settings.

  * ``scope`` : Permissions that your application needs. Set this to ``user:email``.

  * ``redirect_uri`` : The ``redirect_uri`` value you have already setup (from
    Step 2 in pre-requisites).

  * ``state``: A unique string value of your choice that is hard to guess. Used
    to prevent `CSRF`_.

  For example::

    https://github.com/login/oauth/authorize?
      scope=user:email&
      redirect_uri=https%3A%2F%2Foauth2.example.com%2Foauthcallback&
      client_id=812741506391&
      state=DgkRrHXmyu3KLd0KDdfq

  The documentation (in details) for this can be found here:
  https://developer.github.com/v3/oauth/

* Github will handle the authentication, and if the user has granted permission
  for your app, it will redirect back to the "Redirect URI" with the
  authorization code as ``code`` in the URL query string.

* Now your application has to parse the URL and retrieve the authorization code.

* Once the code is retrieved, you have to exchange this code to get the access
  token.  This is done by making the following ``x-www-form-urlencoded`` HTTP
  ``POST`` request to https://github.com/login/oauth/access_token with the
  following parameters (all of them are mandatory).

  * ``code`` : The authorization code you received from the above step.

  * ``redirect_uri`` : The URL in your application where users will be sent
    after authorization.

  * ``client_id`` : Your application client ID.

  * ``client_secret`` : Your application client secret.

  * ``state`` : The unguessable random string you optionally provided earlier.

  Example ::

      POST https://github.com/login/oauth/access_token HTTP/1.1
      Content-Type: application/x-www-form-urlencoded

      code=987654321&redirect_uri=https%3A%2F%2Fwww.myapp.com%2Fexample&client_id=123456789&client_secret=shhdonottell

* Once the access token is retrieved from the previous step, make a call to
  Hasura Auth's ``/github/authenticate``  endpoint to validate the token and
  then create/login the user. The response from Hasura will also indicate if
  this user is a newly created user or an old user (via the ``new_user``
  attribute in the response).


API Endpoints
+++++++++++++

* To validate the access token and then log the user in (and create if not
  exists), make a call to
  ``/github/authenticate?access_token=<ACCESS-TOKEN>``

* To check if the current user is logged in, make a call to:
  ``/user/account/info``.

* To logout, make a call to ``/user/logout``.

* To get Hasura credentials of current logged in user, ``/user/account/info``.

Read the API docs to know more about Hasura Auth endpoints
https://hasura.io/_docs/auth/4.0/swagger-ui/.


.. _implicit grant flow: http://tools.ietf.org/html/rfc6749#section-4.2
.. _CSRF: http://en.wikipedia.org/wiki/Cross-site_request_forgery
