Adding a Login Page to your app using Auth UI Kit
=================================================

Using Hasura's Auth UI Kit, you can quickly add a login page to your existing application. The APIs are already integrated in the UI and hence all you need to do is just send your user to come to the UI Kit page.

Usually, your application would have a ``Login`` button at the top (likely in the header). You can just hyperlink that button to the UI Kit's login page.

The URL to hyperlink - ``https://auth.<cluster-name>.hasura-app.io/ui/login``

.. code-block:: html

  <a href="https://auth.<cluster-name>.hasura-app.io/ui/login">
    <button class="btn">Login</button>
  </a>


So when your website users click on this link, they will be sent to the UI Kit's Login page.

You can add the query parameter :doc:`redirect_url <redirect>` to the URL so that once the user completes their login flow, they will be redirected back to your application's page automatically.
