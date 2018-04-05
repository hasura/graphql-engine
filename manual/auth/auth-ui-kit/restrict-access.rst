.. .. meta::
   :description: Using Hasura Auth UI Kit to protect a page from being visited by a non logged in user.
   :keywords: hasura, users, auth, uikit, protect_app


.. _uikit-usecase-protect-app:

Restricting access to your app using the Auth UI Kit
====================================================

There can be cases where you would like to deploy some apps which should be accessible only to logged in users or only by a
particular set of users based on their :doc:`roles <../authorization/index>`.
Typical use-cases would be admin interfaces, internal reporting/analytics tools, database tools (adminer, php-my-admin, rockmongo), etc.

**For example**, if you have an admin interface running on the subdomain ``admin-app``, ie: ``https://admin-app.<cluster-name>.hasura-app.io``,
when a user visits any page of this app, you would want to allow access only if the user is logged in and has the role ``admin``.
If the user is not logged in, you would want to redirect them to a login page and in case the user doesn't have the ``admin`` role,
you would like to show them a page with an access denied message.

You can achieve this using the Auth UI Kit and the :doc:`conf/routes.yaml <../../project/directory-structure/conf/routes.yaml>` file in your project directory.


In ``conf/routes.yaml`` of your hasura project, your conf for the ``admin-app`` subdomain should look something like this:

.. code-block:: yaml
   :emphasize-lines: 1, 8-11

   admin-app:
     /:
       upstreamService:
         name: admin
         namespace: {{ cluster.metadata.namespaces.user }}
       upstreamServicePath: /
       upstreamServicePort: 80
       authorizationPolicy:
         restrictToRoles: ["admin"]
         noSessionRedirectUrl: https://auth.{{ cluster.name }}.hasura-app.io/ui/login
         noAccessRedirectUrl: https://auth.{{ cluster.name }}.hasura-app.io/ui/restricted


Note the usage of ``restrictToRoles``, ``noSessionRedirectUrl`` and ``noAccessRedirectUrl``.

In the ``restrictToRoles`` key, we add the roles that have access to the app. Only ``admin`` in this case.

In the ``noSessionRedirectUrl`` key, we add the url to which the :doc:`API gateway <../../gateway/index>` should redirect to
if the user does not have a session. ie: is not logged in. In this case, it is the url of the UI Kit's login page.

In the ``noAccessRedirectUrl`` key, we add the url to which the :doc:`API gateway <../../gateway/index>` should redirect to
if the user does not have access to the page. In this case, it is the url of the UI Kit's ``restricted`` page which asks the user to
logout and login as a user that is allowed to access the page.
