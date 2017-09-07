.. meta::
   :description: Overview of the general setup required for integrating Hasura's Auth service with third-party providers via social login for identity management.
   :keywords: hasura, docs, auth, identity, social login, 3rd party provider integraton, integration


Social Login
============

Hasura supports integrating third-party providers for identity management.
Currently, Hasura supports `Google`_, `Facebook`_, `Github`_ and `LinkedIn`_.
There are plans for supporting other providers in the roadmap.

Integrating social login with Hasura
------------------------------------

No matter what provider you use, there is a general setup required for
integrating with third-party providers. It is:

* Create an application with the provider you want (e.g. Google, Facebook,
  LinkedIn).
* Once you create an application, the provider will provide a unique ID and a
  secret associated with the application you created. It is usually called APP
  ID or CLIENT ID, and APP SECRET or CLIENT SECRET respectively.
* In the application settings (after you create the application, you should be
  able to see a settings page for your application), there should be a field
  called "Redirect URI" or "OAuth Callback URI" or something similar. You
  should fill up this field with your own application's endpoint/URL.
* You should also set "scope" or "permissions" in the application settings to
  be "email" and "profile" info (which is to say - you want to access your
  users' email and profile information). This step is important, as otherwise
  Hasura won't be able to fetch the user's email address.
* Now, you should configure Hasura Auth to enable that particular provider and
  use the CLIENT ID and CLIENT SECRET you obtained in the second step.


Now, once you have created and configured your application (with the provider)
**and** configured Hasura Auth to use those credentials; you have to setup your
application to perform the actual login mechanism, which are different for
different providers. Let's look at them in details.

.. _Google: https://google.com
.. _Facebook: https://facebook.com
.. _LinkedIn: https://linkedin.com
.. _Github: https://github.com

.. toctree::
   :maxdepth: 1

   google
   facebook
   github
   linkedin
