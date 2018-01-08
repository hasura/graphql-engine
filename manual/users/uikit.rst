Auth UI Kit
===========

.. ..todo::
   Auth configuration: this can link to hasura project/conf/auth

The Auth UI Kit is a ready to use frontend interface for web apps that comes pre-loaded with the Hasura Auth Microservice.

It allows your application users to login seamlessly using the providers configured in the auth conf. The UI automatically shows enabled auth providers.

The UI Kit runs on ``auth.<cluster-name>.hasura-app.io/ui``

There are two theme options to choose from. Dark theme is the default choice while light theme can be configured via auth conf.

.. figure:: ../../img/uikit-dark.png
   :class: 'dark'
.. figure:: ../../img/uikit-light.png
   :class: 'light'


Redirect URL
------------

The UI Kit accepts a ``redirect_url`` query parameter. Once any action is successfully performed, the UI Kit will redirect to the given parameter.

Verify Email and Reset Password Pages
-------------------------------------

The UI Kit comes with a verify email page, which automatically tries to verify the token (available as token query parameter) and on successful verification, redirects to login page.

In case of Forgot Password, there is a reset password email that is sent. Clicking on the link will take the user to the UI Kit's Reset Password page. On successful verification of the token and the given new password is set. 


Restricted Role
---------------

There are cases where you would like to deploy some microservices which are accessible only by admin or particular set of people (based on authentication roles). 

In those cases, if you have setup Authorisation Policy with restrictToRoles along with 

1. ``noAccessRedirectUrl``

If this is setup, the gateway will redirect to the UI Kit's restricted page asking the user to logout and login as a user with a role that is allowed to access the restricted page.

2. ``noSessionRedirectUrl``

If this is setup, the gateway will redirect to the UI Kit's login page, because no session is present.

Using Custom Interface vs UI Kit
--------------------------------

You might want to use your own custom interface if:

- The design, look and feel does not match your application's design. (You still have a choice between dark and light theme)
- You have a custom auth provider with hooks for signup/login flow.
- You have an advanced workflow for Signup (ex: referral code during signup, custom fields like first name, date of birth etc)


