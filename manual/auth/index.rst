.. .. meta::
   :description: Learn how to use Hasura Users
   :keywords: hasura, users, signup, login, email, mobile, email verification, mobile verification, password forgot, password reset, social login, google, facebook, github, linkedin


Users & Authentication
======================

Hasura Auth APIs let's you create, authenticate and manage user accounts on
your Hasura project. It also let's you manage sessions and roles for users.

Like any other authentication API, Hasura Auth has support for multiple ways to
authenticate an user (e.g. username-based, email-based, mobile-based etc.).
Hasura Auth calls each authentication method a "provider".

Once a user is registered (or signed-up) on Hasura, it attaches a Hasura
Identity or (``hasura_id``) to every user.  A Hasura identity is an integer.
You can use this value in your application to tie your application's user to
this identity.

Hasura Auth APIs also has a bunch of admin APIs to perform administrative tasks
on your user accounts.

Accessing the Auth API
----------------------

The base URL for the Auth microservice would be:
``https://auth.<cluster-name>.hasura-app.io``.

All APIs accept and respond only in JSON. Which means that you have to make
sure that all requests you send, should contain the ``Content-Type:
application/json`` header.

Exploring the Auth API
----------------------

Use the API console to browse various Auth APIs.

.. code-block:: bash

  # in the project directory
  $ hasura api-console

See:
^^^^

.. toctree::
   :maxdepth: 1

   providers/index
   user-actions/index
   admin-actions/index
   config
   sessions
   extra-user-info


.. _recaptcha: https://www.google.com/recaptcha/intro/index.html
.. _Google: https://google.com
.. _Facebook: https://facebook.com
.. _LinkedIn: https://linkedin.com
.. _Github: https://github.com
