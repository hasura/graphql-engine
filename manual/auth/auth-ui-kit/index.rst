Auth UI Kit
===========

.. ..todo::
   Auth configuration: this can link to hasura project/conf/auth

.. figure:: ../../../img/uikit-dark.png
    :class: 'dark'
.. figure:: ../../../img/uikit-light.png
    :class: 'light'


The Auth UI Kit is a ready to use frontend interface for your web apps that comes pre-loaded with the Hasura Auth Microservice.

It allows your application users to login/signup seamlessly using the :doc:`authentication providers <../authentication/providers/index>` configured in the :doc:`auth conf <../../project/directory-structure/conf/auth.yaml>`. The UI adapts automatically to display
the enabled auth providers. Just configure the auth conf of your Hasura Project and the UI Kit will immediately start working on the new changes.

**Features:**

- Instantly add authentication UI for your web app without writing any code.
- Built-in UI for handling forgot password, reset password, email and SMS verification.
- Redirect user to your own app after performing the requests successfully.
- Supports all authentication providers of the Auth API (ie: Username, Email, Mobile, Social Login).
- Adapts automatically to display only enabled auth providers.
- Responsive Design
- Configurable Themes (Light, Dark)

The UI kit runs on the url: ``auth.<cluster-name>.hasura-app.io/ui``.


Using your custom UI interface vs the UI Kit
--------------------------------------------

You might want to use your own custom interface if:

- The design, look and feel does not match your application's design. (Note: You have a choice between dark and light theme)
- You have a custom auth provider with hooks for signup/login flow.
- You have an advanced workflow for Signup (ex: referral code during signup, custom fields like first name, date of birth etc)
- Your application is a mobile app, since this UI Kit is designed for web applications. In case, you are working with React Native, check out our `Auth UI Kit for React Native <https://github.com/hasura/react-native-auth-boilerplate>`_


See:
^^^^

.. toctree::
  :maxdepth: 1

  Themes <themes>
  Adding a Login page to your app <add-login>
  Getting user info after logging in <get-user-data>
  Adding Logout to your app <add-logout>
  Redirecting users back to your app <redirect>
  Restricting access to your app <restrict-access>
  List of all UI kit pages <pages>
