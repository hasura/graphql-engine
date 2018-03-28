.. .. meta::
   :description: Using Hasura Auth UI Kit to protect a page from being visited by a non logged in user.
   :keywords: hasura, users, auth, uikit, protect_app


.. _uikit-usecase-protect-app:

Auth UI Kit: Protect your App from Anonymous Users
==================================================

Let's say you want your app to be usable only by logged in users. Typical use-cases would be Admin Interfaces, Internal Reporting/Analytics Tools, Database tools (adminer, php-my-admin, rockmongo) etc.

Assuming you have an admin interface running at `admin` - https://admin.cluster-name.hasura-app.io, when the user visits this page without logging in as admin, you would want them to force login using admin or user credentials (depending on requirements). You can achieve this using the Auth UI Kit and routes.yaml conf.


In ``conf/routes.yaml`` of your hasura project, your conf should look something like this.

.. code-block:: yaml

  admin:
    /:
      upstreamService:
        name: admin
        namespace: {{ cluster.metadata.namespaces.user }}
      upstreamServicePath: /
      upstreamServicePort: 80
      authorizationPolicy:
        restrictToRoles: ["admin"]
        noSessionRedirectUrl: https://auth.{{ cluster.name }}.hasura-app.io/ui/
        noAccessRedirectUrl: https://auth.{{ cluster.name }}.hasura-app.io/ui/restricted


Note the usage of ``noSessionRedirectUrl`` and ``noAccessRedirectUrl``.


If ``noAccessRedirectUrl`` is setup, the gateway will redirect to the UI Kit's restricted page asking the user to logout and login as a user with a role that is allowed to access the restricted page. In the above conf, in ``restrictToRoles`` key, we have given access to only ``admin`` role.


