.. .. meta::
   :description: Reference documentation for integrating Github OAuth2.0 based user signup & login with Hasura's Auth microservice for your web and mobile applications.
   :keywords: hasura, docs, auth, Github signup, Github login, social login, Github OAuth, Github OAuth2.0, integration

LinkedIn authentication
=======================

Web apps
--------

For web apps using LinkedIn, we have to use the "Authorization Code" grant flow
(or the traditional web server flow) as LinkedIn doesn't support the `implicit
grant flow`_. So this flow needs a backend server to handle intermediate
requests.

Overview
++++++++

First, you have setup and configure your LinkedIn application.  Then, you have
to make a request to LinkedIn API for an "authorization code", specifying a
"Redirect URI" (which is basically a URL to your application). LinkedIn would
respond to this "Redirect URI" with the "authorization code". Your backend
application has to parse the authorization code and make one more request to
LinkedIn API. This final API call will return an access token in the response.
Your application should now validate the access token by passing to a Hasura
Auth endpoint. Hasura will validate the token, and will login the user (if the
user is seen for the first time then Hasura will also create the user).

Pre-requisites
++++++++++++++

* Register an application with LinkedIn, obtain the Client ID and Client secret.

  * Go to https://www.linkedin.com/developer/apps and on top right and click on
    "Create Application" to create a new application. Fill the following form
    that appears to create your application.

  * Now you should see your application's settings page, which also lists the
    client ID and secret. Copy and keep them safe.

* Next, in the same settings page, you have to add "Authorized Redirect
  URLs". This is where you will put your application's endpoint/URL which
  will receive the authorization code.

* Under "Default Application Permissions", check the "r_emailaddress", so that
  your application can read the email address of the user. **This is
  important**. If you fail to do this, Hasura Auth won't be able to fetch your
  user's email address.

* Now you need to configure Hasura Auth microservice with these credentials.

* To configure, go to ``auth.yaml`` in ``conf`` directory inside your Hasura
  project.

* Under ``linkedin``, set ``clientId`` and ``clientSecret``

.. code-block:: yaml

      linkedin:
        clientId: "String"
        clientSecret: "String"


The flow
++++++++

* Redirect the browser (full page or popup) to
  ``https://www.linkedin.com/oauth/v2/authorization`` with the following set of
  query parameters. **Please note** that you have to make the browser redirect
  to this URL and **not** make AJAX calls. The following parameters are
  mandatory:

  * ``response_type`` : This should always be ``code``.

  * ``client_id`` : The client ID you obtain from your LinkedIn application settings.

  * ``scope`` : Permissions that your application needs. Set this to ``r_emailaddress``.

  * ``redirect_uri`` : The ``redirect_uri`` value you have already setup (from
    Step 2 in pre-requisites).

  * ``state``: A unique string value of your choice that is hard to guess. Used
    to prevent `CSRF`_.

  For example::

    https://www.linkedin.com/oauth/v2/authorization?
      scope=r_emailaddress&
      redirect_uri=https%3A%2F%2Foauth2.example.com%2Foauthcallback&
      response_type=code&
      client_id=812741506391&
      state=DgkRrHXmyu3KLd0KDdfq

  The documentation (in details) for this can be found here:
  https://developer.linkedin.com/docs/oauth2

* LinkedIn will handle the authentication, and if the user has granted permission
  for your app, it will redirect back to the "Redirect URI" with the
  authorization code as ``code`` in the URL query string.

* Now your application has to parse the URL and retrieve the authorization code.

* Once the ``code`` is obtained, send the ``code`` to Hasura Auth
  microservice:

.. code-block:: http

   POST auth.<cluster-name>.hasura-app.io/v1/login HTTP/1.1
   Content-Type: application/json

   {
     "provider" : "linkedin",
     "data" : {
        "code": "String",
        "redirect_uri": "String",
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

.. _implicit grant flow: http://tools.ietf.org/html/rfc6749#section-4.2
.. _CSRF: http://en.wikipedia.org/wiki/Cross-site_request_forgery