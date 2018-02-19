.. .. meta::
   :description: Hasura Auth UI Kit Email Verification
   :keywords: hasura, users, auth, uikit, conf


.. _uikit-verification:

Verification Pages
==================

``Verify Email and Reset Password Pages``

The UI Kit comes with a verify email page, which automatically tries to verify the token (available as token query parameter) and on successful verification, redirects to login page.

The URL for verify email page is ``/ui/verify-email?token=<verification-token>``

In case of Forgot Password, there is a reset password email that is sent. Clicking on the link will take the user to the UI Kit's Reset Password page. On successful verification of the token, the given new password is set. 

The URL for ``forgot password`` page is ``/ui/forgot-password``.
The URL for reset password page is ``/ui/reset-password?token=<verification-token>``.

