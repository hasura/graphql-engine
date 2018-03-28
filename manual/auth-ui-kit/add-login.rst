.. .. meta::
   :description: Using Hasura Auth UI Kit for quickly adding a login page to a web app.
   :keywords: hasura, users, auth, uikit, login


.. _uikit-usecase-login:

Auth UI Kit: Add a Login Page to your App
=========================================

Using Hasura's Auth UI Kit, you can quickly add a login page to your existing application. 

Usually your application would have a `Login` button at the top (likely in the header). You can just hyperlink that button to the UI Kit's login page.

The URL to hyperlink - https://auth.cluster-name.hasura-app.io/ui/login (replace cluster name with your own cluster name).

You should also be passing a redirect URL so that once the user completes their login flow, they will be redirected back to your application page automatically.

In the URL to redirect, you will add a query parameter called `redirect_url`. For example

https://auth.catalpa92.hasura-app.io/ui/login?redirect_url=https://myapp.cluster-name.hasura-app.io (In this case, we are redirecting the user to myapp service. Replace values appropriately).