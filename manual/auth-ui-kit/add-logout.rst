Adding Logout to your app using the Auth UI Kit
===============================================

Using Hasura's Auth UI Kit, you can quickly add logout functionality to your app. 

In a normal Auth API flow, you will need to make a HTTP POST request to the logout endpoint and handle success / error cases. To reduce the code written, you can make use of the UI Kit's logout page which internally makes a logout request and redirects the user back to the desired page and handles the error case for you.

Usually your application would have a `Logout` button/link at the top (likely in the header). You can just hyperlink that to the UI Kit's logout page.

The URL to hyperlink - ``https://auth.<cluster-name>.hasura-app.io/ui/logout``.

.. code-block:: html

  <a href="https://auth.<cluster-name>.hasura-app.io/ui/logout">
    <button class="btn">Logout</button>
  </a>


So when your website users click on this link, they will be redirected to the UI Kit's Logout page.

You can add the query parameter :doc:`redirect_url <redirect>` to the URL so that once the user completes their logout flow, they will be redirected back to your application's page automatically, where you can display custom messaging if required.
