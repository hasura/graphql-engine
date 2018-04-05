Redirecting users back to your app from Auth UI Kit pages
=========================================================

The UI Kit pages accept a ``redirect_url`` query parameter. Once any action is successfully performed, the UI Kit will redirect to the given url.

**Example:** Let’s say you have your app running on the ``www`` subdomain and you want your users to redirect to your home page after logging in, you will provide the ``redirect_url`` query param as follows:

``https://auth.<cluster-name>.hasura-app.io/ui?redirect_url=https://www.<cluster-name>.hasura-app.io``

In this case, once login or signup happens successfully, the user will be redirected back to ``https://www.<cluster-name>.hasura-app.io``

Configuring a default redirect_url
----------------------------------

A default redirect_url can also be defined by editing the :doc:`conf/auth.yaml <../../project/directory-structure/conf/auth.yaml>` file in your hasura project.

.. code-block:: yaml
   :emphasize-lines: 3

   uiKit:
     ...
     redirectUrl: "https://auth.{{ cluster.name }}.hasura-app.io/v1/user/info"

In case the default redirect_url is configured, it will be considered when a ``redirect_url`` query param is not given to a Auth UI Kit page.
You can leave its value as ``null`` in case you want to only redirect if a ``redirect_url`` param was given to the page.


