.. .. meta::
   :description: Using Hasura Auth UI Kit to quickly logout a user
   :keywords: hasura, users, auth, uikit, logout


.. _uikit-usecase-logout:

Auth UI Kit: Add Logout to your App
===================================


Using Hasura's Auth UI Kit, you can quickly add logout functionality. 

Usually your application would have a `Logout` button at the top (likely in the header). You can just hyperlink that button to the UI Kit's logout page.

The URL to hyperlink - https://auth.cluster-name.hasura-app.io/ui/logout (replace cluster name with your own cluster name).

You should also be passing a redirect URL so that once the user completes their logout flow, they will be redirected back to your application page automatically.

In the URL to redirect, you will add a query parameter called `redirect_url`. For example

https://auth.catalpa92.hasura-app.io/ui/logout?redirect_url=https://myapp.cluster-name.hasura-app.io (In this case, we are redirecting the user to myapp service. Replace values appropriately).

**Note** In a normal Auth API flow, you will need to make a POST request to the logout endpoint. To reduce the code written, you can make use of the UI Kit's logout page which internally makes a logout request and redirects the user back to the desired page.
