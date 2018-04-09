.. .. meta::
   :description: Learn how to use Hasura Users
   :keywords: hasura, users, signup, login, email, mobile, email verification, mobile verification, password forgot, password reset, social login, google, facebook, github, linkedin

.. _providers:

Authentication methods a.k.a. Providers
=======================================

Hasura Auth has **"providers"** to support different modes of authentication.

You can :doc:`enable/disable providers <configure-auth-methods>` in your auth configuration. Once a provider is
enabled you can use them to signup your users. **You can have multiple
providers enabled at the same time.**

You can also **create your own custom provider** (i.e if you have any custom
authentication logic) and configure it with Hasura Auth.

The following providers come by default with Hasura Auth:

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


Choose providers from the above based on your requirement.

**See more about providers:**

.. toctree::
   :maxdepth: 1

   configure-auth-methods
   Username <username>
   Email <email>
   Mobile/OTP <mobile-otp>
   Mobile/password <mobile-password>
   Google <google>
   Facebook <facebook>
   Github <github>
   LinkedIn <linkedin>
   Custom provider <custom-provider>

.. _recaptcha: https://www.google.com/recaptcha/intro/index.html
.. _Google: https://google.com
.. _Facebook: https://facebook.com
.. _LinkedIn: https://linkedin.com
.. _Github: https://github.com
