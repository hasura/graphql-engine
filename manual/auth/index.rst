.. meta::
   :keywords: hasura_id

Users & Authentication
======================

Hasura Auth APIs lets you create, authenticate and manage user accounts on
your Hasura project. It also lets you manage sessions and roles for users.

Like any other authentication API, Hasura Auth has support for multiple ways to
authenticate a user (e.g. username-based, email-based, mobile-based etc.).
Hasura Auth calls each authentication method a "provider".

Once a user is registered (or signed-up) on Hasura, it attaches a ``Hasura
Identity`` or (``hasura_id``) to every user.  A Hasura identity is an integer.
You can use this value in your application to tie your application's user to
this identity.

Hasura Auth APIs also has a bunch of admin APIs to perform administrative tasks
on your user accounts.


**Explore the Auth APIs**

Use the API console to browse the various Auth APIs.

.. code-block:: bash

  # in the project directory
  $ hasura api-console

See:
^^^^

.. toctree::
   :maxdepth: 1

   providers/index
   User actions <user-actions/index>
   Admin actions <admin-actions/index>
   config
   sessions
   extra-user-info


.. _recaptcha: https://www.google.com/recaptcha/intro/index.html
.. _Google: https://google.com
.. _Facebook: https://facebook.com
.. _LinkedIn: https://linkedin.com
.. _Github: https://github.com
