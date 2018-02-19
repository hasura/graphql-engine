.. .. meta::
   :description: Hasura Auth UI Kit Redirect URL
   :keywords: hasura, users, auth, uikit, conf


.. _uikit-usage:

Usage
=====

The UI Kit runs on ``auth.<cluster-name>.hasura-app.io/ui``

- Redirect URL

The UI Kit accepts a ``redirect_url`` query parameter. Once any action is successfully performed, the UI Kit will redirect to the given parameter.

Example: If you want your users to redirect to your home page after logging in, you will provide the ``redirect_url`` query param.

``https://auth.<cluster-name>.hasura-app.io/ui?redirect_url=https://www.<cluster-name>.hasura-app.io``

In this case, once login or signup happens successfully, the user will be redirected back to ``https://www.<cluster-name>.hasura-app.io``

- UI Pages

All the pages below accept the ``redirect_url``

  ``/ui`` - Landing page. Shows login options automatically for enabled providers

- Login

  ``/ui/login/username`` - Login page for Username Provider

  ``/ui/login/email`` - Login page for Email Provider

  ``/ui/login/mobile`` - Login page for Mobile/Password Provider

  ``/ui/login/mobile-otp`` - Login page for Mobile/OTP Provider

- Signup

  ``/ui/signup/username`` - Signup page for Username Provider

  ``/ui/signup/email`` - Signup page for Email Provider

  ``/ui/signup/mobile`` - Signup page for Mobile/Password Provider
  
  ``/ui/signup/mobile-otp`` - Signup page for Mobile/OTP Provider

- Verification

  ``/ui/verify-email`` - Email Verification page. Accepts a ``token`` query parameter for verifying the email. Usually users come to this page after clicking on this link from their email.
  Example - /ui/verify-email?token=<verification-token>

  ``/ui/forgot-password`` - Forgot password page. Users can be redirected to this page when they forget password. The user then submits their email address and gets a reset password email.

  ``/ui/reset-password`` - Reset password page. Accepts a ``token`` query parameter for verifying the email. Usually users come to this page after clicking on the link from their email. The user then enters their new password on this page to reset their password. The flow for this page would be Forgot Password -> Email Sent -> Reset Password.
  Example - /ui/reset-password?token=<verification-token>

- Advanced

  ``/ui/restricted`` - Users can be redirected to this page when any of your app URLs need authorization. For example, let's say you have an admin panel running at ``admin`` subdomain of your cluster. i.e ``https://admin.<cluster-name>.hasura-app.io``. You would only want users with admin role to be able to see this page.

Signup Flow
-----------

Once the application user signups using Auth UI Kit, you would most likely want to store the user information on your user table as well. This can be achieved by redirecting them to a URL in your website, where you can check the session information using the headers. 

If ``X-Hasura-User-Id`` is set and ``X-Hasura-Allowed-Roles`` doesn't contain ``anonymous``, then a session has been created for the user. 

You can make a GET request to ``auth.<cluster-name>.hasura-app.io/v1/user/info`` in your server side code to get session information as well. Now using this information, you can make an ``insert`` query with ``on_conflict`` clause , (which will insert if constrait matches, or will update if a constraint violates) to store user data in your own application database.


