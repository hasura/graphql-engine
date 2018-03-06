.. .. meta::
   :description: Learn how to use Hasura Users
   :keywords: hasura, users, signup, login, email, mobile, email verification, mobile verification, password forgot, password reset, social login, google, facebook, github, linkedin

.. _providers:

Authentication Providers
========================

Hasura Auth has providers to support different modes of authentication.
It has the following providers by default.

.. list-table::
   :header-rows: 1

   * - Provider
     - Provider Name (in API)
     - Description
   * - Username
     - ``username``
     - Basic username and password based authentication.
   * - Email 
     - ``email``
     - Email and password based authentication, with email verification.
   * - Mobile/OTP
     - ``mobile``
     - Mobile-based password-less authentication. For signup and login an OTP is sent to the mobile number.
   * - Mobile/Password
     - ``mobile-password``
     - Mobile and password based authentication, with mobile verification.
   * - Google
     - ``google``
     - Google login based authentication.
   * - Facebook
     - ``facebook``
     - Facebook login based authentication.
   * - Linkedin
     - ``linkedin``
     - Linkedin login based authentication.
   * - Github
     - ``github``
     - Github login based authentication.


Choose a provider from above based on your requirement.

You can enable/disable providers in your auth configuration. Once a provider is
enabled you can use them to signup your users. You **can** have multiple
providers enabled at the same time.

You can also create your custom provider (i.e if you have any custom
authentication logic) and configure it with Hasura Auth.

Providers:
^^^^^^^^^^

.. toctree::
   :maxdepth: 1

   username
   email
   mobile-otp
   mobile-password
   google
   facebook
   github
   linkedin
   custom-provider

.. _recaptcha: https://www.google.com/recaptcha/intro/index.html
.. _Google: https://google.com
.. _Facebook: https://facebook.com
.. _LinkedIn: https://linkedin.com
.. _Github: https://github.com
