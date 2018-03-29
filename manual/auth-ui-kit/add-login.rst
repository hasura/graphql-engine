.. .. meta::
   :description: Using Hasura Auth UI Kit for quickly adding a login page to a web app.
   :keywords: hasura, users, auth, uikit, login


.. _uikit-usecase-login:

Auth UI Kit: Add a Login Page to your App
=========================================

Using Hasura's Auth UI Kit, you can quickly add a login page to your existing application. The APIs are already integrated in the UI and hence all you need to do is just point your user to come to the UI Kit of your app.

For example:

Let's say you have your main website running at `www` subdomain.
Usually your application would have a `Login` button at the top (likely in the header). You can just hyperlink that button to the UI Kit's login page.


.. code-block:: html

  <a href="https://auth.awesome45.hasura-app.io/ui/login?redirect_url=https://www.awesome45.hasura-app.io">
    <button class="btn">Login</button>
  </a>

The URL to hyperlink - https://auth.awesome45.hasura-app.io/ui/login?redirect_url=https://wwww.awesome45.hasura-app.io (replace awesome45 with your own cluster name).

So when your website user clicks on this link, they will be redirected to the UI Kit's Login page.

As you can see above, redirect URL has been passed as a query parameter `redirect_url` so that once the user completes their login flow, they will be redirected back to your application page automatically.
