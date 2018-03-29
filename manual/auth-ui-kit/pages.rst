All Auth UI Kit pages
=====================

The Auth UI Kit comes with the following pre-built pages:

Landing page
------------
``/ui`` - Shows login options automatically for enabled providers

Login pages
-----------
``/ui/login`` - Shows login options automatically for enabled providers

``/ui/login/username`` - Login page for Username Provider

``/ui/login/email`` - Login page for Email Provider

``/ui/login/mobile`` - Login page for Mobile/Password Provider

``/ui/login/mobile-otp`` - Login page for Mobile/OTP Provider

Signup pages
------------
``/ui/signup`` - Shows signup options automatically for enabled providers

``/ui/signup/username`` - Signup page for Username Provider

``/ui/signup/email`` - Signup page for Email Provider

``/ui/signup/mobile`` - Signup page for Mobile/Password Provider

``/ui/signup/mobile-otp`` - Signup page for Mobile/OTP Provider

Logout page
-----------
``/ui/logout`` - Logs out user and shows logged out message.

Email verification page
-----------------------
``/ui/verify-email`` - Accepts a ``token`` query parameter for verifying the email. Usually users come to this page after clicking on this link from their email.
Example - /ui/verify-email?token=<verification-token>

Forgot password page
--------------------
``/ui/forgot-password`` - Users can be redirected to this page when they forget their password. The user then submits their email address and gets a reset password email.

Reset password page
-------------------
``/ui/reset-password`` - Accepts a ``token`` query parameter for verifying the email. Usually users come to this page after clicking on the link from their email. The user then enters their new password on this page to reset their password. The flow for this page would be Forgot Password -> Email Sent -> Reset Password.
Example - /ui/reset-password?token=<verification-token>

Restricted access page
----------------------
``/ui/restricted`` - Users can be redirected to this page when they try to access a page they don't have access to. For example, if you have an admin panel page in your app, you would only allow users with admin role to see this page and redirect other users to the restricted page.