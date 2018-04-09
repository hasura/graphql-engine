.. .. meta::
   :description: Reference documentation for integrating Github OAuth2.0 based user signup & login with Hasura's Auth microservice for your web and mobile applications.
   :keywords: hasura, docs, auth, Github signup, Github login, social login, Github OAuth, Github OAuth2.0, integration

Github authentication
=====================

Setup
-----

Web apps
~~~~~~~~

For web apps using Github, we have to use the "Authorization Code" grant flow
(or the traditional web server flow) as Github doesn't support the `implicit
grant flow`_. So this flow needs a backend server to handle intermediate
requests.

Overview
~~~~~~~~

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
~~~~~~~~~~~~~~

* Register an application with Github, obtain the Client ID and Client secret.

  * Go to https://github.com/settings/developers and on top right and click on
    "Register a new application" to create a new application. Fill the
    following form that appears to create your application.

  * In the form, you have to add "Authorized Redirect URLs". This is where you
    will put your application's endpoint/URL which will receive the
    authorization code.

  * Now you should see your application's settings page, which also lists the
    client ID and secret. Copy and keep them safe.

Configuration
~~~~~~~~~~~~~

* Now you need to configure Hasura Auth to tell it to use these credentials.

* To configure, go to :doc:`conf/auth.yaml <../../../project/directory-structure/conf/auth.yaml>` file inside your Hasura
  project.

* Under ``github``, set the ``clientId``.

    Note: The ``github`` key might be commented out. Make sure to uncomment it.

.. code-block:: yaml

      github:
        clientId: "String"
        clientSecret:
          secretKeyRef:
            key: auth.github.client_secret
            name: hasura-secrets

* **clientId**: The client ID obtained when creating the application.

* **clientSecret**: The client secret obtained when creating the application. As you see in the above code snippet, client secret is a reference to a :ref:`hasura project secret <hasura-secrets-manual>` called ``auth.github.client_secret``.
  To add your client secret to ``hasura project secrets``, run the following command from your project directory.

  .. code-block:: bash

    $ hasura secret update auth.github.client_secret


The flow
--------

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

* Once the ``code`` is obtained, send the ``code`` to Hasura Auth
  microservice:

.. code-block:: http

  POST auth.<cluster-name>.hasura-app.io/v1/login HTTP/1.1
  Content-Type: application/json

  {
    "provider" : "github",
    "data" : {
       "code": "String",
       "redirect_uri": "String (optional)",
       "state": "String"
    }
  }

* If successful, this will return a response as follows:

.. code-block:: http

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
