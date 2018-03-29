Auth UI Kit
===========

.. ..todo::
   Auth configuration: this can link to hasura project/conf/auth

The Auth UI Kit is a ready to use frontend interface for web apps that comes pre-loaded with the Hasura Auth Microservice.

- Instantly add Authentication UI for your web app without writing any code.
- Supports all authentication providers of the Auth API. (Username, Email, Mobile, Social Login)
- Redirect user to your own app after performing the requests successfully.
- Built-in UI for handling forgot password, reset password, email and sms verification.
- Responsive Design
- Configurable Themes (Light, Dark)

It allows your application users to login seamlessly using the providers configured in the auth conf. The UI adapts automatically to display enabled auth providers. Just configure the auth conf of your Hasura Project and the UI Kit will immediatedly start working on the new changes.

The UI Kit runs on ``auth.<cluster-name>.hasura-app.io/ui``

There are two theme options to choose from. Dark theme is the default choice while light theme can be configured via auth conf.

.. figure:: ../../img/uikit-dark.png
   :class: 'dark'
.. figure:: ../../img/uikit-light.png
   :class: 'light'


Using Custom Interface vs UI Kit
--------------------------------

You might want to use your own custom interface if:

- The design, look and feel does not match your application's design. (You still have a choice between dark and light theme)
- You have a custom auth provider with hooks for signup/login flow.
- You have an advanced workflow for Signup (ex: referral code during signup, custom fields like first name, date of birth etc)
- Your application is a mobile app, since this UI Kit is designed for web applications. In case, you are working with React Native, check out our `Auth UI Kit for React Native <https://github.com/hasura/react-native-auth-boilerplate>`_


See:
^^^^

.. toctree::
  :maxdepth: 1

  Add Login to App <add-login>
  Get User Data <get-user-data>
  Add Logout to App <add-logout>
  Protect your App <protect-app>
  Usage <usage>
  Configuration <conf>
  Restrict to Admin Only <admin-only>
  Verification Pages <verification>
