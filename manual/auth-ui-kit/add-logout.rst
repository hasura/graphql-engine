.. .. meta::
   :description: Using Hasura Auth UI Kit to quickly logout a user
   :keywords: hasura, users, auth, uikit, logout


.. _uikit-usecase-logout:

Auth UI Kit: Add Logout to your App
===================================


Using Hasura's Auth UI Kit, you can quickly add logout functionality. 

In a normal Auth API flow, you will need to make a HTTP POST request to the logout endpoint and handle success / error cases. To reduce the code written, you can make use of the UI Kit's logout page which internally makes a logout request and redirects the user back to the desired page and handles the error case for you.

Usually your application would have a `Logout` button/link at the top (likely in the header). You can just hyperlink that to the UI Kit's logout page.

.. code-block:: html

  <a href="https://auth.awesome45.hasura-app.io/ui/login?redirect_url=https://www.awesome45.hasura-app.io">
    <button class="btn">Logout</button>
  </a>

The URL to hyperlink - https://auth.awesome45.hasura-app.io/ui/logout?redirect_url=https://www.awesome45.hasura-app.io (replace cluster name with your own cluster name).

So when your website user clicks on this link, they will be redirected to the UI Kit's Login page.

As you can see above, redirect URL has been passed as a query parameter `redirect_url` so that once the user completes their logout flow, they will be redirected back to your application page automatically, where you can display custom messaging if required.

